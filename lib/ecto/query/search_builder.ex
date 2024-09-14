defmodule Ecto.Query.SearchBuilder do

  alias Ecto.Query.Builder

  import Ecto.Query.Builder, only: [
    error!: 1,
    escape_field!: 3,
    escape_var!: 2,
    find_var!: 2
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
  """
  @type acc :: %{
    optional(:bind) => non_neg_integer,
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
          {list, acc},
          Keyword.t(),
          Macro.Env.t() | {Macro.Env.t(), fun}
        ) :: {Macro.t(), {list, acc}}
  def escape(expr, params_acc, vars, env)

  def escape({:is_nil, _, [field]}, params_acc, vars, _env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :is_nil, 1)

    {{:{}, [], [:is_nil, [], [field]]}, params_acc}
  end

  def escape({:parse, _, [bind, parade_ql]}, params_acc, vars, env) do
    {_bind, params_acc} = escape_bind!(bind, params_acc, vars, :parse, 2)
    {parade_ql, params_acc} = Builder.escape(parade_ql, :string, params_acc, vars, env)

    {{:{}, [], [:parse, [], [parade_ql]]}, params_acc}
  end

  def escape({:all, _, [bind]}, params_acc, vars, _) do
    {_bind, params_acc} = escape_bind!(bind, params_acc, vars, :all, 1)

    {{:{}, [], [:all, [], []]}, params_acc}
  end

  def escape({:boolean, _, [condition, queries]}, params_acc, vars, env)
      when condition in [:must, :must_not, :should] do
    {queries, params_acc} =
      Enum.map_reduce(queries, params_acc, fn expr, params_acc ->
        escape_term!(expr, params_acc, vars, env)
      end)

    {{:{}, [], [:boolean, [], [condition, queries]]}, params_acc}
  end

  def escape({:and, _, [left, right]}, params_acc, vars, env) do
    {left, params_acc} = escape(left, params_acc, vars, env)
    {right, params_acc} = escape(right, params_acc, vars, env)

    {{:{}, [], [:boolean, [], [:must, [left, right]]]}, params_acc}
  end

  def escape({:or, _, [left, right]}, params_acc, vars, env) do
    {left, params_acc} = escape(left, params_acc, vars, env)
    {right, params_acc} = escape(right, params_acc, vars, env)

    {{:{}, [], [:boolean, [], [:should, [left, right]]]}, params_acc}
  end

  def escape({:not, _, [query]}, params_acc, vars, env) do
    {query, params_acc} = escape(query, params_acc, vars, env)

    {{:{}, [], [:boolean, [], [:must_not, [query]]]}, params_acc}
  end

  def escape({:boost, _, [query, boost]}, params_acc, vars, env) do
    {query, params_acc} = escape(query, params_acc, vars, env)
    {boost, params_acc} = Builder.escape(boost, :float, params_acc, vars, env)

    {{:{}, [], [:boost, [], [query, boost]]}, params_acc}
  end

  def escape({:const_score, _, [query, score]}, params_acc, vars, env) do
    {query, params_acc} = escape(query, params_acc, vars, env)
    {score, params_acc} = Builder.escape(score, :float, params_acc, vars, env)

    {{:{}, [], [:const_score, [], [query, score]]}, params_acc}
  end

  def escape({:disjunction_max, _, [disjuncts]}, params_acc, vars, env) do
    {disjuncts, params_acc} =
      Enum.map_reduce(disjuncts, params_acc, fn expr, params_acc ->
        escape(expr, params_acc, vars, env)
      end)

    {{:{}, [], [:disjunction_max, [], [disjuncts]]}, params_acc}
  end

  def escape({:disjunction_max, _, [disjuncts, tie_breaker]}, params_acc, vars, env) do
    {disjuncts, params_acc} =
      Enum.map_reduce(disjuncts, params_acc, fn expr, params_acc ->
        escape(expr, params_acc, vars, env)
      end)
    {tie_breaker, params_acc} = Builder.escape(tie_breaker, :integer, params_acc, vars, env)

    {{:{}, [], [:disjunction_max, [], [disjuncts, tie_breaker]]}, params_acc}
  end

  def escape({:empty, _, [bind]}, params_acc, vars, _) do
    {_bind, params_acc} = escape_bind!(bind, params_acc, vars, :empty, 1)

    {{:{}, [], [:empty, [], []]}, params_acc}
  end

  def escape({:exists, _, [field]}, params_acc, vars, _) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :exists, 1)

    {{:{}, [], [:exists, [], [field]]}, params_acc}
  end

  def escape({:fuzzy_term, _, [field, value]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :fuzzy_term, 2)
    {value, params_acc} = Builder.escape(value, :string, params_acc, vars, env)

    {{:{}, [], [:fuzzy_term, [], [field, value]]}, params_acc}
  end

  @fuzzy_option_types [distance: :integer, transpose_cost_one: :boolean, prefix: :boolean]

  def escape({:fuzzy_term, _, [field, value, opts]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :fuzzy_term, 3)
    {value, params_acc} = Builder.escape(value, :string, params_acc, vars, env)
    {opts, params_acc} = escape_options!(opts, @fuzzy_option_types, params_acc, vars, env)

    {{:{}, [], [:fuzzy_term, [], [field, value, opts]]}, params_acc}
  end

  def escape({:phrase, _, [field, phrases]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :phrase, 2)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)

    {{:{}, [], [:phrase, [], [field, phrases]]}, params_acc}
  end

  def escape({:phrase, _, [field, phrases, slop]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :phrase, 3)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)
    {slop, params_acc} = Builder.escape(slop, :integer, params_acc, vars, env)

    {{:{}, [], [:phrase, [], [field, phrases, slop]]}, params_acc}
  end

  def escape({:phrase_prefix, _, [field, phrases]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :phrase_prefix, 2)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)

    {{:{}, [], [:phrase_prefix, [], [field, phrases]]}, params_acc}
  end

  def escape({:phrase_prefix, _, [field, phrases, max_expansion]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :phrase_prefix, 3)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)
    {max_expansion, params_acc} = Builder.escape(max_expansion, :integer, params_acc, vars, env)

    {{:{}, [], [:phrase_prefix, [], [field, phrases, max_expansion]]}, params_acc}
  end

  def escape({range, _, [field, start, stop, bounds]}, params_acc, vars, env)
      when range in ~w(int4range int8range daterange tsrange tstzrange)a do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, range, 4)
    {start, params_acc} = Builder.escape(start, :any, params_acc, vars, env)
    {stop, params_acc} = Builder.escape(stop, :any, params_acc, vars, env)
    {bounds, params_acc} = Builder.escape(bounds, :string, params_acc, vars, env)

    {{:{}, [], [range, [], [field, start, stop, bounds]]}, params_acc}
  end

  def escape({:regex, _, [field, pattern]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :regex, 2)
    {pattern, params_acc} = Builder.escape(pattern, :string, params_acc, vars, env)

    {{:{}, [], [:regex, [], [field, pattern]]}, params_acc}
  end

  def escape({:term, _, [field, term]}, params_acc, vars, env) do
    {field, params_acc} = escape_field_param!(field, params_acc, vars, :term, 2)
    {term, params_acc} = Builder.escape(term, :any, params_acc, vars, env)

    {{:{}, [], [:term, [], [field, term]]}, params_acc}
  end

  def escape({:term_set, _, [term_set]}, params_acc, vars, env) do
    {term_set, params_acc} =
      Enum.map_reduce(term_set, params_acc, fn expr, params_acc ->
        escape_term!(expr, params_acc, vars, env)
      end)

    {{:{}, [], [:term_set, [], [term_set]]}, params_acc}
  end

  def escape({op, _, _} = expr, _params_acc, _vars, _env) when op in ~w(limit_rows offset_rows stable_sort order_by)a do
    error!("`#{Macro.to_string(expr)}` is only allowed as a top-level expression of a search call.")
  end

  def escape({op, _, _}, _params_acc, _vars, _env) when op in ~w(|| && !)a do
    error!(
      "short-circuit operators are not supported: `#{op}`. " <>
        "Instead use boolean operators: `and`, `or`, and `not`"
    )
  end

  # Unnecessary parentheses around an expression
  def escape({:__block__, _, [expr]}, params_acc, vars, env) do
    escape(expr, params_acc, vars, env)
  end

  def escape(other, _params_acc, _vars, _env) do
    error!("`#{Macro.to_string(other)}` is not a valid search expression")
  end

  defp escape_term!({:term, _, [field, term]}, params_acc, vars, env) do
    field = escape_field_param!(field, params_acc, vars, :term, 2)
    {term, params_acc} = Builder.escape(term, :any, params_acc, vars, env)

    {{:{}, [], [:term, [], [field, term]]}, params_acc}
  end

  defp escape_term!(expr, _, _, _) do
    error!("All values in term_set/2 must be of term/2, got: `#{Macro.to_string(expr)}`")
  end

  # escapes the options specified by opt_types present in opts. Unspecified keys are ignored.
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
    {expr, params_acc} = Builder.escape(value, type, params_acc, vars, env)
    {[acc ++ {key, expr}], params_acc}
  end

  def escape_bind!({var, _, context}, params_acc, vars, operator, arity)
       when is_atom(var) and is_atom(context) do
    params_acc = validate_bind!(var, params_acc, vars, operator, arity)

    {escape_var!(var, vars), params_acc}
  end

  def escape_bind!(arg, _params_acc, _vars, operator, arity) do
    error!("first argument of `#{operator}/#{arity}` `#{Macro.to_string(arg)}` must be a bind.")
  end

  @doc false
  def escape_field_param!({{:., _, [{var, _, context} = bind, field]}, _, []}, params_acc, vars, operator, arity)
       when is_atom(var) and is_atom(context) and is_atom(field) do
    params_acc = validate_bind!(var, params_acc, vars, operator, arity)

    {escape_field!(bind, field, vars), params_acc}
  end

  def escape_field_param!({:field, _, [callee, field]}, params_acc, vars, operator, arity) do
    params_acc = validate_bind!(callee, params_acc, vars, operator, arity)

    {escape_field!(callee, field, vars), params_acc}
  end

  def escape_field_param!(arg, _, _, operator, arity) do
    error!("first argument of `#{operator}/#{arity}` must be a bound field, got: `#{Macro.to_string(arg)}`")
  end

  # no prior binds
  defp validate_bind!(var, {params, %{bind: nil}}, vars, _, _) when is_atom(var) do
    {params, %{bind: {var, find_var!(var, vars)}}}
  end

  # binds match
  defp validate_bind!(var, {_, %{bind: {var, _}}} = params_acc, _, _, _) do
    params_acc
  end

  # binds don't match
  defp validate_bind!(var, {_, %{bind: {bind, _}}}, _, operator, arity) do
    error!("""
    search expressions can only bind to a single source, \
    call to `#{operator}/#{arity}` attempted bind to `#{var}` when `#{bind}` is already bound.
    """)
  end
end
