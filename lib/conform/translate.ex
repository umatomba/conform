defmodule Conform.Translate do
  @moduledoc """
  This module is responsible for translating either from .conf -> .config or
  from .schema.exs -> .conf
  """

  @list_types [:list, :enum, :complex]

  @doc """
  This exception reflects an issue with the translation process
  """
  defmodule TranslateError do
    defexception message: "Translation failed!"
  end

  @doc """
  Translate the provided schema to it's default .conf representation
  """
  @spec to_conf([{atom, term}]) :: binary
  def to_conf(schema) do
    schema = Keyword.delete(schema, :import)
    case schema do
      [mappings: mappings, translations: _] ->
        Enum.reduce mappings, "", fn {key, info}, result ->
          # If the datatype of this mapping is an enum,
          # write out the allowed values
          datatype             = Keyword.get(info, :datatype, :binary)
          doc                  = Keyword.get(info, :doc, "")
          {custom?, mod, args} = is_custom_type?(datatype)
          comments = cond do
            custom? ->
              case {doc, mod.to_doc(args)} do
                {doc, false} -> to_comment(doc)
                {"", doc}    -> to_comment(doc)
                {doc, extra} -> to_comment("#{doc}\n#{extra}")
              end
            true ->
              to_comment(doc)
          end
          result = case datatype do
            [enum: values] ->
              allowed = "# Allowed values: #{Enum.join(values, ", ")}\n"
              <<result::binary, comments::binary, ?\n, allowed::binary>>
            _ ->
              <<result::binary, comments::binary, ?\n>>
          end
          default = Keyword.get(info, :default)
          case default do
            nil ->
              <<result::binary, "# #{key} = \n\n">>
            default ->
              <<result::binary, "#{key} = #{write_datatype(datatype, default, key)}\n\n">>
          end
        end
      _ ->
        raise Conform.Schema.SchemaError
    end
  end

  @doc """
  Translate the provided .conf to it's .config representation using the provided schema.
  """
  @spec to_config([{term, term}] | [], [{term, term}] | [], [{term, term}]) :: term
  def to_config(config, conf, schema) do
    schema = Keyword.delete(schema, :import)
    case schema do
      [mappings: mappings, translations: translations] ->
        # Apply mappings/translations
        conf = transform_conf(conf, mappings, translations)

        # Merge the config.exs/sys.config terms
        IO.inspect {:config, config}
        IO.inspect {:conf, conf}
        merged = merge(config, conf)
        IO.inspect {:merged, merged}

        # Convert config map to Erlang config terms
        settings_to_config(merged)
      _ ->
        raise Conform.Schema.SchemaError
    end
  end

  # Merges two sets of Elixir/Erlang terms, where the terms come in the form of lists of tuples.
  defp merge(old, new) when is_list(old) and is_list(new) do
    merge(old, new, [])
  end

  defp merge([{old_key, old_value} = h | t], new, acc) when is_tuple(h) do
    case :lists.keytake(elem(h, 0), 1, new) do
      {:value, {new_key, new_value}, rest} ->
        IO.inspect {:merge, old_value, new_value, Keyword.keyword?(old_value), Keyword.keyword?(new_value)}
        # Value is present in new, so merge the value
        cond do
          # TODO replace with call to merge_term
          Keyword.keyword?(old_value) && Keyword.keyword?(new_value) ->
            merged = merge(old_value, new_value)
            merge(t, rest, [{new_key, merged}|acc])
          :io_lib.char_list(old_value) && :io_lib.char_list(new_value) ->
            merge(t, rest, [{new_key, new_value}|acc])
          is_list(old_value) && is_list(new_value) ->

          old_value == nil && is_list(new_value) ->
            merge(t, rest, [{new_key, new_value}|acc])
          true ->
            merged = merge_term(h, new_value) |> IO.inspect
            merge(t, rest, [merged|acc])
        end
      false ->
        # Value doesn't exist in new, so add it
        merge(t, new, [h|acc])
    end
  end
  defp merge([], new, acc) do
    Enum.reverse(acc, new)
  end

  defp merge_term([hold|told], [hnew|tnew] = new) when is_list(new) do
    [merge_term(hold, hnew) | merge_term(told, tnew)]
  end
  defp merge_term([], new) when is_list(new), do: new
  defp merge_term(old, []) when is_list(old), do: old

  defp merge_term(old, new) when is_tuple(old) and is_tuple(new) do
    old
    |> Tuple.to_list
    |> Enum.with_index
    |> Enum.reduce([], fn
        {[], idx}, acc ->
          [elem(new, idx)|acc]
        {val, idx}, acc when is_list(val) ->
          case :io_lib.char_list(val) do
            true ->
              [elem(new, idx)|acc]
            false ->
              merged = val |> Enum.concat(elem(new, idx)) |> Enum.uniq
              [merged|acc]
          end
        {val, idx}, acc when is_tuple(val) ->
          [merge_term(val, elem(new, idx))|acc]
        {_val, idx}, acc ->
          [elem(new, idx)|acc]
       end)
    |> Enum.reverse
    |> List.to_tuple
  end

  defp transform_conf(conf, mappings, translations) do
    table_id = :ets.new(:conform_conf, [:set, keypos: 1])
    try do
      # Populate table
      for {key, value} <- conf, do: :ets.insert(table_id, {key, value})
      # Convert mappings/translations to same key format
      mappings = Enum.map(mappings, fn {key, mapping} ->
        new_key     = String.split(Atom.to_string(key), ".") |> Enum.map(&String.to_char_list/1)
        to          = Keyword.get(mapping, :to, "")
        new_to      = String.split(to, ".") |> Enum.map(&String.to_char_list/1)
        {new_key, Keyword.merge(mapping, [to: new_to])}
      end) |> Enum.sort_by(fn {key, _} -> Enum.count(key) end, fn x, y -> x >= y end)
      translations = Enum.map(translations, fn {key, translation} ->
        new_key = String.split(Atom.to_string(key), ".") |> Enum.map(&String.to_char_list/1)
        {new_key, translation}
      end)
      # Apply conversions
      convert_types(mappings, table_id)
      # Build complex types
      convert_complex_types(mappings, table_id)
      # Apply translations to aggregated values
      apply_translations(mappings, translations, table_id)
      # Fetch config from ETS
      result = :ets.tab2list(table_id)
      # Sort by longest keys so that we build the config hierarchy from the bottom up
      result = Enum.sort_by(result, fn {key, _} -> Enum.count(key) end, fn x, y -> x <= y end)
      # Build config
      result = Enum.reduce(result, [], fn {key, value}, acc ->
        key = Enum.map(key, &List.to_atom/1)
        {acc, _} = List.foldl(key, {acc, []}, fn key_part, {acc, parents} ->
          current       = [key_part|parents]
          current_path  = current |> Enum.reverse
          case get_in(acc, current_path) do
            nil -> {put_in(acc, current_path, []), current}
            val -> {acc, current}
          end
        end)
        put_in(acc, key, value)
      end)

    catch
      err ->
        Conform.Utils.error("Error thrown when constructing configuration: #{Macro.to_string(err)}")
        exit(1)
    after
      :ets.delete(table_id)
    end
  end

  defp convert_types([], _), do: true
  defp convert_types([{key, mapping}|rest], table) do
    # Get conf item
    select_expr = {Enum.map(key, fn '*' -> :'_'; k -> k end), :'_'}
    case :ets.match_object(table, select_expr) do
      # No matches
      [] -> convert_types(rest, table)
      # Matches requiring conversion
      results when is_list(results) ->
        for {conf_key, value} <- results do
          datatype = Keyword.get(mapping, :datatype, :binary)
          default  = Keyword.get(mapping, :default, nil)
          parsed = case value do
            nil -> default
            _   -> parse_datatype(datatype, value, conf_key)
          end
          :ets.insert(table, {conf_key, parsed})
        end
        convert_types(rest, table)
    end
  end

  defp convert_complex_types([], _), do: true
  defp convert_complex_types([{key, mapping}|rest], table) do
    case Keyword.get(mapping, :datatype) do
      :complex ->
        to_key = Keyword.get(mapping, :to, key)
        # Build complex type
        {selected, results} = construct_complex_type(key, mapping, table)
        # Iterate over the selected keys, deleting them from the table
        for {variables, _, _, conf_key} <- selected do
          # Map over to_key, applying replacements of the wildcards
          # Get the indices of wildcards in the mapping key
          to_key_vars = to_key
                        |> Enum.filter(fn '*' -> true; _ -> false end)
                        |> Enum.with_index
          # For each wildcard, find it's corresponding match in the conf key,
          # and iterate through `to_key` until we find the next unreplaced wildcard,
          # replacing it with the match found.
          to_key = Enum.reduce(to_key_vars, to_key, fn {_, index}, acc ->
            replacement = Enum.at(variables, index)
            {_, replaced} = List.foldr(acc, {false, []}, fn
              '*', {false, acc} -> {true, [replacement|acc]}
              '*', {true, acc}  -> {true, acc}
              part, {replaced?, acc}    -> {replaced?, [part|acc]}
            end)
            replaced
          end)
          # Get the child element in the result corresponding to this key
          path = variables |> Enum.map(&List.to_atom/1)
          child = get_in(results, path)
          # Insert the mapped value under the replaced key, merging with existing
          # value if present
          case :ets.lookup(table, to_key) do
            []              -> :ets.insert(table, {to_key, child})
            [{_, existing}] -> :ets.insert(table, {to_key, Keyword.merge(existing, child)})
          end
          :ets.delete(table, conf_key)
        end
        # Move to next mapping
        convert_complex_types(rest, table)
      complex when complex in [{:list, :complex}, [:complex]] ->
        to_key = Keyword.get(mapping, :to, key)
        # Get all records which match the current map_key + children
        {selected, results} = construct_complex_type(key, mapping, table)
        # Iterate over the selected keys, deleting them from the table
        for {[child_key|_], _, _, conf_key} <- selected do
          child = get_in(results, [List.to_atom(child_key)])
          to_key = to_key ++ [child_key]
          :ets.insert(table, {to_key, child})
          :ets.delete(table, conf_key)
        end
        convert_complex_types(rest, table)
      _ ->
        convert_complex_types(rest, table)
    end
  end

  defp construct_complex_type(key, mapping, table) do
    to_key = Keyword.get(mapping, :to, key)
    # Get all records which match the current map_key + children
    key_parts  = key
                 |> Enum.with_index
                 |> Enum.map(fn {'*', i} -> {:'$#{i+1}', i}; {k, i} -> {k, nil} end)
    variables  = key_parts
                 |> Enum.filter(fn {_, nil} -> false; _ -> true end)
                 |> Enum.map(fn {var, _} -> var end)
    key_parts  = key_parts
                 |> Enum.map(fn {part, _} -> part end)
                 |> Enum.reverse
    # We want to capture the subkey list, which is why we're building an inproper list here for the matchspec
    select_key  = [:'$99' | key_parts] |> Enum.reverse
    # Our match spec is saying: Match any records which match at least the given key, and have one or more
    # additional elements to the key, returning a tuple of {wildcard_variables, subkey_list, value, conf_key}
    # `wildcard_variables` contains the actual values in the conf which map to wildcards in the mapping key,
    # `subkey_list` is a list of keys which are children of the mapping key path.
    # `value` is the value of full conf key path
    select_expr = [{{select_key, :'$100'}, [{:'>=', {:length, :'$99'}, 1}], [{{variables, :'$99', :'$100', select_key}}]}]
    selected    = :ets.select(table, select_expr)
    # Make sure the hierarchy exists for this mapping
    results = Enum.reduce(selected, [], fn {variables, subkey, value, _}, acc ->
      to_key = (variables ++ [subkey]) |> Enum.map(&List.to_atom/1)
      # Fold over the key path, ensuring that each intermediate key in the path,
      # exists in the result object.
      {acc, _} = List.foldl(to_key, {acc, []}, fn key_part, {acc, parents} ->
        current       = [key_part|parents]
        current_path  = current |> Enum.reverse
        case get_in(acc, current_path) do
          nil -> {put_in(acc, current_path, []), current}
          val -> {acc, current}
        end
      end)
      # Put the value for the full key path in the result
      put_in(acc, to_key, value)
    end)
    {selected, results}
  end

  defp apply_translations(_mappings, [], _table), do: true
  defp apply_translations(mappings, [{key, translation}|rest], table) when is_function(translation) do
    # Get mapping for this translation
    case Enum.find(mappings, fn {mapping_key, _} -> mapping_key == key end) do
      nil     -> apply_translations(mappings, rest, table)
      {to_key, mapping} ->
        # Use mapping key to locate values to be translated
        select_expr = {Enum.map(to_key, fn '*' -> :'_'; k -> k end), :'_'}
        select_result = :ets.match_object(table, select_expr)
        case :ets.match_object(table, select_expr) do
          [] -> apply_translations(mappings, rest, table)
          results when is_list(results) ->
            # For each result, apply the translation to the value selected, and update the stored value
            Enum.reduce(results, [], fn {result_key, value}, acc ->
              current_key = List.last(result_key) |> List.to_atom
              translated = case :erlang.fun_info(translation, :arity) do
                {:arity, 2} ->
                  translation.(mapping, {current_key, value})
                {:arity, 3} ->
                  translation.(mapping, {current_key, value}, acc)
                _ ->
                  key = Enum.map(key, &List.to_string/1) |> Enum.join(".")
                  Conform.Utils.error("Invalid translation function arity for #{key}. Must be /2 or /3")
                  exit(1)
              end
              insert_key = Enum.slice(result_key, 0, Enum.count(result_key) - 1)
              :ets.insert(table, {insert_key, translated})
              :ets.delete(table, result_key)
              translated
            end)
        end
        apply_translations(mappings, rest, table)
    end
  end


  # Add a .conf-style comment to the given line
  defp add_comment(line), do: "# #{line}"

  # Convert config map to Erlang config terms
  # End result: [{:app, [{:key1, val1}, {:key2, val2}, ...]}]
  defp settings_to_config(map) when is_map(map),            do: Enum.map(map, &settings_to_config/1)
  defp settings_to_config({key, value}) when is_map(value), do: {String.to_atom(key), settings_to_config(value)}
  defp settings_to_config({key, value}),                    do: {String.to_atom(key), value}
  defp settings_to_config(value),                           do: value

  # Parse the provided value as a value of the given datatype
  defp parse_datatype(:atom, value, _setting),     do: "#{value}" |> String.to_atom
  defp parse_datatype(:binary, value, _setting),   do: "#{value}"
  defp parse_datatype(:charlist, value, _setting), do: '#{value}'
  defp parse_datatype(:boolean, value, setting) do
    try do
      case "#{value}" |> String.to_existing_atom do
        true  -> true
        false -> false
        _     -> raise TranslateError, messagae: "Invalid boolean value for #{setting}."
      end
    rescue
      ArgumentError ->
        raise TranslateError, messagae: "Invalid boolean value for #{setting}."
    end
  end
  defp parse_datatype(:integer, value, setting) do
    case "#{value}" |> Integer.parse do
      {num, _} -> num
      :error   -> raise TranslateError, message: "Invalid integer value for #{setting}."
    end
  end
  defp parse_datatype(:float, value, setting) do
    case "#{value}" |> Float.parse do
      {num, _} -> num
      :error   -> raise TranslateError, message: "Invalid float value for #{setting}."
    end
  end
  defp parse_datatype(:ip, value, setting) do
    case "#{value}" |> String.split(":", trim: true) do
      [ip, port] -> {ip, port}
      _          -> raise TranslateError, message: "Invalid IP format for #{setting}. Expected format: IP:PORT"
    end
  end
  defp parse_datatype([enum: valid_values], value, setting) do
    parsed = "#{value}" |> String.to_atom
    if Enum.any?(valid_values, fn v -> v == parsed end) do
      parsed
    else
      raise TranslateErorr, message: "Invalid enum value for #{setting}."
    end
  end
  defp parse_datatype([list: :ip], value, setting) do
    "#{value}"
    |> String.split(",")
    |> Enum.map(&String.strip/1)
    |> Enum.map(&(parse_datatype(:ip, &1, setting)))
  end
  defp parse_datatype([list: list_type], value, setting) do
    case :io_lib.char_list(value) do
      true  ->
        "#{value}"
        |> String.split(",")
        |> Enum.map(&String.strip/1)
        |> Enum.map(&(parse_datatype(list_type, &1, setting)))
      false ->
        Enum.map(value, &(parse_datatype(list_type, &1, setting)))
    end
  end
  defp parse_datatype({:atom, type}, {k, v}, setting) do
    {k, parse_datatype(type, v, setting)}
  end
  defp parse_datatype(_datatype, _value, _setting), do: nil

  # Write values of the given datatype to their string format (for the .conf)
  defp write_datatype(:atom, value, _setting), do: value |> Atom.to_string
  defp write_datatype(:ip, value, setting) do
    case value do
      {ip, port} -> "#{ip}:#{port}"
      _ -> raise TranslateError, message: "Invalid IP address format for #{setting}. Expected format: {IP, PORT}"
    end
  end
  defp write_datatype([enum: _], value, setting),  do: write_datatype(:atom, value, setting)
  defp write_datatype([list: [list: list_type]], value, setting) when is_list(value) do
    Enum.map(value, fn sublist ->
      elems = Enum.map(sublist, &(write_datatype(list_type, &1, setting))) |> Enum.join(", ")
      <<?[, elems::binary, ?]>>
    end) |> Enum.join(", ")
  end
  defp write_datatype([list: list_type], value, setting) when is_list(value) do
    value |> Enum.map(&(write_datatype(list_type, &1, setting))) |> Enum.join(", ")
  end
  defp write_datatype([list: list_type], value, setting) do
    write_datatype([list: list_type], [value], setting)
  end
  defp write_datatype(:binary, value, _setting) do
    <<?", "#{value}", ?">>
  end
  defp write_datatype({:atom, type}, {k, v}, setting) do
    converted = write_datatype(type, v, setting)
    <<Atom.to_string(k)::binary, " = ", converted::binary>>
  end
  defp write_datatype(_datatype, value, _setting), do: "#{value}"

  defp to_comment(str) do
    String.split(str, "\n", trim: true) |> Enum.map(&add_comment/1) |> Enum.join("\n")
  end

  defp is_custom_type?(datatype) do
    {mod, args} = case datatype do
      [{mod, args}]         -> {mod, args}
      mod                   -> {mod, nil}
    end
    case Code.ensure_loaded(mod) do
      {:error, :nofile} -> {false, mod, args}
      {:module, mod}    ->
        behaviours = get_in(mod.module_info, [:attributes, :behaviour]) || []
        case Enum.member?(behaviours, Conform.Type) do
          true  -> {true, mod, args}
          false -> {false, mod, args}
        end
    end
  end
end
