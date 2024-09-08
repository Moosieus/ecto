defmodule Ecto.Query.SearchBuilder do

  import Ecto.Query.Builder, only: [
    error!: 1,
    escape_field!: 3,
    validate_type!: 3,
    escape_with_type: 5,
    escape_kw_fragment: 4,
    expand_and_split_fragment: 2,
    escape_fragment: 4,
    merge_fragments: 2,
    assert_type!: 3,
    escape_interval: 5,
    literal: 3,
    quoted_type: 2,
    wrap_nil: 2,
    escape_var!: 2,
    try_expansion: 5,
    escape_call: 5
  ]

  @comparisons [
    is_nil: 1,
    ==: 2,
    !=: 2,
    <: 2,
    >: 2,
    <=: 2,
    >=: 2,
  ]

  @typedoc """
  Quoted types store primitive types and types in the format
  {source, quoted}. The latter are handled directly in the planner,
  never forwarded to Ecto.Type.

  The Ecto.Type module concerns itself only with runtime types,
  which include all primitive types and custom user types. Also
  note custom user types do not show up during compilation time.
  """
  @type quoted_type :: Ecto.Type.primitive() | {non_neg_integer, atom | Macro.t()}

  @typedoc """
  The accumulator during escape.

  If the subqueries field is available, subquery escaping must take place.
  """
  @type acc :: %{
    optional(:subqueries) => list(Macro.t()),
    optional(:take) => %{non_neg_integer => Macro.t()},
    optional(any) => any
  }

  @doc """
  Smart escapes a query expression and extracts interpolated values in
  a map.

  Everything that is a query expression will be escaped, interpolated
  expressions (`^foo`) will be moved to a map unescaped and replaced
  with `^index` in the query where index is a number indexing into the
  map.
  """
  @spec escape(
          Macro.t(),
          quoted_type | {:in, quoted_type} | {:out, quoted_type} | {:splice, quoted_type},
          {list, acc},
          Keyword.t(),
          Macro.Env.t() | {Macro.Env.t(), fun}
        ) :: {Macro.t(), {list, acc}}
  def escape(expr, type, params_acc, vars, env)

  # var.x - where var is bound
  def escape({{:., _, [callee, field]}, _, []}, _type, params_acc, vars, _env)
      when is_atom(field) do
    {escape_field!(callee, field, vars), params_acc}
  end

  # field macro
  def escape({:field, _, [callee, field]}, _type, params_acc, vars, _env) do
    {escape_field!(callee, field, vars), params_acc}
  end

  # param interpolation
  def escape({:^, _, [arg]}, type, {params, acc}, _vars, _env) do
    expr = {:{}, [], [:^, [], [length(params)]]}
    params = [{arg, type} | params]
    {expr, {params, acc}}
  end

  # tagged types
  def escape({:type, _, [{:^, _, [arg]}, type]}, _type, {params, acc}, vars, env) do
    type = validate_type!(type, vars, env)
    expr = {:{}, [], [:type, [], [{:{}, [], [:^, [], [length(params)]]}, type]]}
    params = [{arg, type} | params]
    {expr, {params, acc}}
  end

  def escape(
        {:type, _, [{{:., _, [{var, _, context}, field]}, _, []} = expr, type]},
        _type,
        params_acc,
        vars,
        env
      )
      when is_atom(var) and is_atom(context) and is_atom(field) do
    escape_with_type(expr, type, params_acc, vars, env)
  end

  def escape({:type, _, [{:coalesce, _, [_ | _]} = expr, type]}, _type, params_acc, vars, env) do
    escape_with_type(expr, type, params_acc, vars, env)
  end

  def escape({:type, _, [{:field, _, [_ | _]} = expr, type]}, _type, params_acc, vars, env) do
    escape_with_type(expr, type, params_acc, vars, env)
  end

  def escape({:type, _, [{math_op, _, [_, _]} = op_expr, type]}, _type, params_acc, vars, env)
      when math_op in ~w(+ - * /)a do
    escape_with_type(op_expr, type, params_acc, vars, env)
  end

  def escape({:type, _, [{fun, _, args} = expr, type]}, _type, params_acc, vars, env)
      when is_list(args) and fun in ~w(fragment avg count max min sum over filter)a do
    escape_with_type(expr, type, params_acc, vars, env)
  end

  def escape(
        {:type, _, [{:json_extract_path, _, [_ | _]} = expr, type]},
        _type,
        params_acc,
        vars,
        env
      ) do
    escape_with_type(expr, type, params_acc, vars, env)
  end

  def escape(
        {:type, _, [{{:., _, [Access, :get]}, _, _} = access_expr, type]},
        _type,
        params_acc,
        vars,
        env
      ) do
    escape_with_type(access_expr, type, params_acc, vars, env)
  end

  def escape(
        {:type, _, [{{:., _, [{:parent_as, _, [_parent]}, _field]}, _, []} = expr, type]},
        _type,
        params_acc,
        vars,
        env
      ) do
    escape_with_type(expr, type, params_acc, vars, env)
  end

  def escape({:type, meta, [expr, type]}, given_type, params_acc, vars, env) do
    case Macro.expand_once(expr, get_env(env)) do
      ^expr ->
        error!("""
        the first argument of type/2 must be one of:

          * interpolations, such as ^value
          * fields, such as p.foo or field(p, :foo)
          * fragments, such as fragment("foo(?)", value)
          * an arithmetic expression (+, -, *, /)
          * an aggregation or window expression (avg, count, min, max, sum, over, filter)
          * a conditional expression (coalesce)
          * access/json paths (p.column[0].field)
          * parent_as/1 (parent_as(:parent).field)

        Got: #{Macro.to_string(expr)}
        """)

      expanded ->
        escape({:type, meta, [expanded, type]}, given_type, params_acc, vars, env)
    end
  end

  # fragments
  def escape({:fragment, _, [query]}, _type, params_acc, vars, env) when is_list(query) do
    {escaped, params_acc} =
      Enum.map_reduce(query, params_acc, &escape_kw_fragment(&1, &2, vars, env))

    {{:{}, [], [:fragment, [], [escaped]]}, params_acc}
  end

  def escape({:fragment, _, [{:^, _, [var]} = _expr]}, _type, params_acc, _vars, _env) do
    expr = quote do: Ecto.Query.Builder.fragment!(unquote(var))
    {{:{}, [], [:fragment, [], [expr]]}, params_acc}
  end

  def escape({:fragment, _, [query | frags]}, _type, params_acc, vars, env) do
    pieces = expand_and_split_fragment(query, env)

    if length(pieces) != length(frags) + 1 do
      error!(
        "fragment(...) expects extra arguments in the same amount of question marks in string. " <>
          "It received #{length(frags)} extra argument(s) but expected #{length(pieces) - 1}"
      )
    end

    {frags, params_acc} = Enum.map_reduce(frags, params_acc, &escape_fragment(&1, &2, vars, env))
    {{:{}, [], [:fragment, [], merge_fragments(pieces, frags)]}, params_acc}
  end

  # interval
  def escape({:from_now, meta, [count, interval]}, type, params_acc, vars, env) do
    utc = quote do: ^DateTime.utc_now()
    escape({:datetime_add, meta, [utc, count, interval]}, type, params_acc, vars, env)
  end

  def escape({:ago, meta, [count, interval]}, type, params_acc, vars, env) do
    utc = quote do: ^DateTime.utc_now()

    count =
      case count do
        {:^, meta, [value]} ->
          negate = quote do: Ecto.Query.Builder.negate!(unquote(value))
          {:^, meta, [negate]}

        value ->
          {:-, [], [value]}
      end

    escape({:datetime_add, meta, [utc, count, interval]}, type, params_acc, vars, env)
  end

  def escape({:datetime_add, _, [datetime, count, interval]} = expr, type, params_acc, vars, env) do
    assert_type!(expr, type, {:supertype, :datetime})
    {datetime, params_acc} = escape(datetime, {:supertype, :datetime}, params_acc, vars, env)
    {count, interval, params_acc} = escape_interval(count, interval, params_acc, vars, env)
    {{:{}, [], [:datetime_add, [], [datetime, count, interval]]}, params_acc}
  end

  def escape({:date_add, _, [date, count, interval]} = expr, type, params_acc, vars, env) do
    assert_type!(expr, type, :date)
    {date, params_acc} = escape(date, :date, params_acc, vars, env)
    {count, interval, params_acc} = escape_interval(count, interval, params_acc, vars, env)
    {{:{}, [], [:date_add, [], [date, count, interval]]}, params_acc}
  end

  # sigils
  def escape({name, _, [_, []]} = sigil, type, params_acc, vars, _env)
      when name in ~w(sigil_s sigil_S sigil_w sigil_W)a do
    {literal(sigil, type, vars), params_acc}
  end

  # lists
  def escape(list, type, params_acc, vars, env) when is_list(list) do
    if Enum.all?(list, &(is_binary(&1) or is_number(&1) or is_boolean(&1))) do
      {literal(list, type, vars), params_acc}
    else
      fun =
        case type do
          {:array, inner_type} ->
            &escape(&1, inner_type, &2, vars, env)

          _ ->
            # In case we don't have an array nor a literal at compile-time,
            # such as p.links == [^value], we don't do any casting nor validation.
            # We may want to tackle this if the expression above is ever used.
            &escape(&1, :any, &2, vars, env)
        end

      Enum.map_reduce(list, params_acc, fun)
    end
  end

  # literals
  def escape({:<<>>, _, args} = expr, type, params_acc, vars, _env) do
    valid? =
      Enum.all?(args, fn
        {:"::", _, [left, _]} -> is_integer(left) or is_binary(left)
        left -> is_integer(left) or is_binary(left)
      end)

    unless valid? do
      error!(
        "`#{Macro.to_string(expr)}` is not a valid query expression. " <>
          "Only literal binaries and strings are allowed, " <>
          "dynamic values need to be explicitly interpolated in queries with ^"
      )
    end

    {literal(expr, type, vars), params_acc}
  end

  def escape({:-, _, [number]}, type, params_acc, vars, _env) when is_number(number),
    do: {literal(-number, type, vars), params_acc}

  def escape(number, type, params_acc, vars, _env) when is_number(number),
    do: {literal(number, type, vars), params_acc}

  def escape(binary, type, params_acc, vars, _env) when is_binary(binary),
    do: {literal(binary, type, vars), params_acc}

  def escape(nil, _type, params_acc, _vars, _env),
    do: {nil, params_acc}

  def escape(atom, type, params_acc, vars, _env) when is_atom(atom),
    do: {literal(atom, type, vars), params_acc}

  # negate any expression
  def escape({:-, meta, arg}, type, params_acc, vars, env) do
    {escaped_arg, params_acc} = escape(arg, type, params_acc, vars, env)
    expr = {:{}, [], [:-, meta, escaped_arg]}
    {expr, params_acc}
  end

  # comparison operators
  def escape({comp_op, _, [left, right]} = expr, type, params_acc, vars, env)
      when comp_op in ~w(== != < > <= >=)a do
    assert_type!(expr, type, :boolean)

    if is_nil(left) or is_nil(right) do
      error!(
        "comparison with nil is forbidden as it is unsafe. " <>
          "If you want to check if a value is nil, use is_nil/1 instead"
      )
    end

    ltype = quoted_type(right, vars)
    rtype = quoted_type(left, vars)

    {left, params_acc} = escape(left, ltype, params_acc, vars, env)
    {right, params_acc} = escape(right, rtype, params_acc, vars, env)

    {params, acc} = params_acc
    {{:{}, [], [comp_op, [], [left, right]]}, {params |> wrap_nil(left) |> wrap_nil(right), acc}}
  end

  # mathematical operators
  def escape({math_op, _, [left, right]}, type, params_acc, vars, env)
      when math_op in ~w(+ - * /)a do
    {left, params_acc} = escape(left, type, params_acc, vars, env)
    {right, params_acc} = escape(right, type, params_acc, vars, env)

    {{:{}, [], [math_op, [], [left, right]]}, params_acc}
  end

  # in operator
  def escape({:in, _, [left, right]} = expr, type, params_acc, vars, env)
      when is_list(right)
      when is_tuple(right) and elem(right, 0) in ~w(sigil_w sigil_W @)a do
    assert_type!(expr, type, :boolean)

    right = Macro.expand_once(right, get_env(env))
    {:array, ltype} = quoted_type(right, vars)
    rtype = {:array, quoted_type(left, vars)}

    {left, params_acc} = escape(left, ltype, params_acc, vars, env)
    {right, params_acc} = escape(right, rtype, params_acc, vars, env)
    {{:{}, [], [:in, [], [left, right]]}, params_acc}
  end

  def escape({:in, _, [left, right]} = expr, type, params_acc, vars, env) do
    assert_type!(expr, type, :boolean)

    ltype = {:out, quoted_type(right, vars)}
    rtype = {:in, quoted_type(left, vars)}

    {left, params_acc} = escape(left, ltype, params_acc, vars, env)
    {right, params_acc} = escape(right, rtype, params_acc, vars, env)

    # Remove any type wrapper from the right side
    right =
      case right do
        {:{}, [], [:type, [], [right, _]]} -> right
        _ -> right
      end

    {{:{}, [], [:in, [], [left, right]]}, params_acc}
  end

  def escape({:coalesce, _, [left, right]}, type, params_acc, vars, env) do
    {left, params_acc} = escape(left, type, params_acc, vars, env)
    {right, params_acc} = escape(right, type, params_acc, vars, env)
    {{:{}, [], [:coalesce, [], [left, right]]}, params_acc}
  end

  def escape({:=, _, _} = expr, _type, _params_acc, _vars, _env) do
    error!(
      "`#{Macro.to_string(expr)}` is not a valid query expression. " <>
        "The match operator is not supported: `=`. " <>
        "Did you mean to use `==` instead?"
    )
  end

  # TODO: Separate out escaping of search query expressions from all other ecto expressions.

  # first argument needs to be a table binding
  def escape({:parse, _, [bind, parade_ql]}, _type, params_acc, vars, env) do
    bind = escape_bind!(bind, vars, :parse, 2)
    {parade_ql, params_acc} = escape(parade_ql, :string, params_acc, vars, env)

    {{:{}, [], [:parse, [], [bind, parade_ql]]}, params_acc}
  end

  def escape({:all, _, [bind]}, _, params_acc, vars, _) do
    bind = escape_bind!(bind, vars, :all, 1)

    {{:{}, [], [:all, [], [bind]]}, params_acc}
  end

  def escape({:boost, _, [query, boost]}, _, params_acc, vars, env) do
    {query, params_acc} = escape(query, :boolean, params_acc, vars, env)
    {boost, params_acc} = escape(boost, :float, params_acc, vars, env)

    {{:{}, [], [:boost, [], [query, boost]]}, params_acc}
  end

  def escape({:const_score, _, [query, score]}, _, params_acc, vars, env) do
    {query, params_acc} = escape(query, :boolean, params_acc, vars, env)
    {score, params_acc} = escape(score, :float, params_acc, vars, env)

    {{:{}, [], [:const_score, [], [query, score]]}, params_acc}
  end

  def escape({:empty, _, [field]}, _type, params_acc, vars, _env) do
    field = escape_field_param!(field, vars, :empty, 1)

    {{:{}, [], [:empty, [], [field]]}, params_acc}
  end

  def escape({:exists, _, [field]}, _type, params_acc, vars, _env) do
    field = escape_field_param!(field, vars, :exists, 1)

    {{:{}, [], [:exists, [], [field]]}, params_acc}
  end

  def escape({:fuzzy_term, _, [field, value]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :fuzzy_term, 2)
    {value, params_acc} = escape(value, :string, params_acc, vars, env)

    {{:{}, [], [:fuzzy_term, [], [field, value]]}, params_acc}
  end

  @fuzzy_option_types [distance: :integer, transpose_cost_one: :boolean, prefix: :boolean]

  def escape({:fuzzy_term, _, [field, value, opts]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :fuzzy_term, 3)
    {value, params_acc} = escape(value, :string, params_acc, vars, env)
    {opts, params_acc} = escape_options!(opts, @fuzzy_option_types, params_acc, vars, env)

    {{:{}, [], [:fuzzy_term, [], [field, value, opts]]}, params_acc}
  end

  def escape({:phrase, _, [field, phrases]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase, 2)
    {phrases, params_acc} = escape(phrases, {:array, :string}, params_acc, vars, env)

    {{:{}, [], [:phrase, [], [field, phrases]]}, params_acc}
  end

  def escape({:phrase, _, [field, phrases, slop]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase, 3)
    {phrases, params_acc} = escape(phrases, {:array, :string}, params_acc, vars, env)
    {slop, params_acc} = escape(slop, :integer, params_acc, vars, env)

    {{:{}, [], [:phrase, [], [field, phrases, slop]]}, params_acc}
  end

  def escape({:phrase_prefix, _, [field, phrases]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase_prefix, 2)
    {phrases, params_acc} = escape(phrases, {:array, :string}, params_acc, vars, env)

    {{:{}, [], [:phrase_prefix, [], [field, phrases]]}, params_acc}
  end

  def escape({:phrase_prefix, _, [field, phrases, max_expansion]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase_prefix, 3)
    {phrases, params_acc} = escape(phrases, {:array, :string}, params_acc, vars, env)
    {max_expansion, params_acc} = escape(max_expansion, :integer, params_acc, vars, env)

    {{:{}, [], [:phrase_prefix, [], [field, phrases, max_expansion]]}, params_acc}
  end

  def escape({:regex, _, [field, pattern]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :regex, 2)
    {pattern, params_acc} = escape(pattern, :string, params_acc, vars, env)

    {{:{}, [], [:regex, [], [field, pattern]]}, params_acc}
  end

  def escape({:term, _, [field, term]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :term, 2)
    {term, params_acc} = escape(term, :any, params_acc, vars, env)

    {{:{}, [], [:term, [], [field, term]]}, params_acc}
  end

  def escape({:term_set, _, [field, term_set]}, type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :term_set, 2)
    {field, params_acc} = escape(field, type, params_acc, vars, env)
    {term_set, params_acc} = escape(term_set, :boolean, params_acc, vars, env)

    # get schema and validate keys

    {{:{}, [], [:term_set, [], [field, term_set]]}, params_acc}
  end

  def escape({op, _, _}, _type, _params_acc, _vars, _env) when op in ~w(|| && !)a do
    error!(
      "short-circuit operators are not supported: `#{op}`. " <>
        "Instead use boolean operators: `and`, `or`, and `not`"
    )
  end

  # Unnecessary parentheses around an expression
  def escape({:__block__, _, [expr]}, type, params_acc, vars, env) do
    escape(expr, type, params_acc, vars, env)
  end

  # Other functions - no type casting
  def escape({name, _, args} = expr, type, params_acc, vars, env)
      when is_atom(name) and is_list(args) do
    case call_type(name, length(args)) do
      {in_type, out_type} ->
        assert_type!(expr, type, out_type)
        escape_call(expr, in_type, params_acc, vars, env)

      nil ->
        try_expansion(expr, type, params_acc, vars, env)
    end
  end

  # Finally handle vars
  def escape({var, _, context}, _type, params_acc, vars, _env)
      when is_atom(var) and is_atom(context) do
    {escape_var!(var, vars), params_acc}
  end

  # Raise nice error messages for fun calls.
  def escape({fun, _, args} = other, _type, _params_acc, _vars, _env)
      when is_atom(fun) and is_list(args) do
    error!("""
    `#{Macro.to_string(other)}` is not a valid query expression. \
    If you are trying to invoke a function that is not supported by Ecto, \
    you can use fragments:

        fragment("some_function(?, ?, ?)", m.some_field, 1)

    See Ecto.Query.API to learn more about the supported functions and \
    Ecto.Query.API.fragment/1 to learn more about fragments.
    """)
  end

  # Raise nice error message for remote calls
  def escape({{:., _, [_, fun]}, _, _} = other, type, params_acc, vars, env)
      when is_atom(fun) do
    try_expansion(other, type, params_acc, vars, env)
  end

  # For everything else we raise
  def escape(other, _type, _params_acc, _vars, _env) do
    error!("`#{Macro.to_string(other)}` is not a valid query expression")
  end

  defp escape_options!(opts, opt_types, params_acc, vars, env) do
    opts = keyword_literal!(opts)

    opts =
      for {key, type} <- opt_types,
          Keyword.has_key?(opts, key) do
        {key, Keyword.fetch!(opts, key), type}
      end

    Enum.reduce(opts, {[], params_acc}, &escape_option(&1, &2, vars, env))
  end

  defp keyword_literal!(kw_list) when is_list(kw_list) do
    if Keyword.keyword?(kw_list) do
      kw_list
    else
      error!("`#{Macro.to_string(kw_list)}` is not a valid keyword list.")
    end
  end

  defp keyword_literal!(other) do
    error!("`#{Macro.to_string(other)}` must be a keyword list literal.")
  end

  defp escape_option({key, value, type}, {acc, params_acc}, vars, env) do
    {expr, params_acc} = escape(value, type, params_acc, vars, env)
    {[acc ++ {key, expr}], params_acc}
  end

  defp escape_bind!({var, _, context}, vars, _, _) when is_atom(var) and is_atom(context) do
    escape_var!(var, vars)
  end

  defp escape_bind!(bind, _vars, operator, arity) do
    error!("first argument of `#{operator}/#{arity}` must be a source, got: `#{Macro.to_string(bind)}`")
  end

  defp escape_field_param!({{:., _, [{var, _, context} = bind, field]}, _, []}, vars, _, _)
      when is_atom(var) and is_atom(context) and is_atom(field) do
    escape_field!(bind, field, vars)
  end

  defp escape_field_param!({:field, _, [callee, field]}, vars, _, _) do
    escape_field!(callee, field, vars)
  end

  defp escape_field_param!(arg, _, operator, arity) do
    error!("first argument of `#{operator}/#{arity}` must be a bound field, got: `#{Macro.to_string(arg)}`")
  end

  defp get_env({env, _}), do: env
  defp get_env(env), do: env

  defp call_type(_, _), do: nil
end
