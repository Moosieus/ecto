defmodule Ecto.Query.SearchBuilder do

  alias Ecto.Query.Builder

  import Ecto.Query.Builder, only: [
    error!: 1,
    escape_field!: 3,
    escape_var!: 2,
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

  def escape({:is_nil, _, [field]}, _type, params_acc, vars, _env) do
    field = escape_field_param!(field, vars, :is_nil, 1)

    {{:{}, [], [:is_nil, [], [field]]}, params_acc}
  end

  def escape({:parse, _, [bind, parade_ql]}, _type, params_acc, vars, env) do
    bind = escape_bind!(bind, vars, :parse, 2)
    {parade_ql, params_acc} = Builder.escape(parade_ql, :string, params_acc, vars, env)

    {{:{}, [], [:parse, [], [bind, parade_ql]]}, params_acc}
  end

  def escape({:all, _, [bind]}, _, params_acc, vars, _) do
    bind = escape_bind!(bind, vars, :all, 1)

    {{:{}, [], [:all, [], [bind]]}, params_acc}
  end

  def escape({:boost, _, [query, boost]}, _, params_acc, vars, env) do
    {query, params_acc} = escape(query, :boolean, params_acc, vars, env)
    {boost, params_acc} = Builder.escape(boost, :float, params_acc, vars, env)

    {{:{}, [], [:boost, [], [query, boost]]}, params_acc}
  end

  def escape({:const_score, _, [query, score]}, _, params_acc, vars, env) do
    {query, params_acc} = escape(query, :boolean, params_acc, vars, env)
    {score, params_acc} = Builder.escape(score, :float, params_acc, vars, env)

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
    {value, params_acc} = Builder.escape(value, :string, params_acc, vars, env)

    {{:{}, [], [:fuzzy_term, [], [field, value]]}, params_acc}
  end

  @fuzzy_option_types [distance: :integer, transpose_cost_one: :boolean, prefix: :boolean]

  def escape({:fuzzy_term, _, [field, value, opts]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :fuzzy_term, 3)
    {value, params_acc} = Builder.escape(value, :string, params_acc, vars, env)
    {opts, params_acc} = escape_options!(opts, @fuzzy_option_types, params_acc, vars, env)

    {{:{}, [], [:fuzzy_term, [], [field, value, opts]]}, params_acc}
  end

  def escape({:phrase, _, [field, phrases]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase, 2)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)

    {{:{}, [], [:phrase, [], [field, phrases]]}, params_acc}
  end

  def escape({:phrase, _, [field, phrases, slop]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase, 3)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)
    {slop, params_acc} = Builder.escape(slop, :integer, params_acc, vars, env)

    {{:{}, [], [:phrase, [], [field, phrases, slop]]}, params_acc}
  end

  def escape({:phrase_prefix, _, [field, phrases]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase_prefix, 2)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)

    {{:{}, [], [:phrase_prefix, [], [field, phrases]]}, params_acc}
  end

  def escape({:phrase_prefix, _, [field, phrases, max_expansion]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :phrase_prefix, 3)
    {phrases, params_acc} = Builder.escape(phrases, {:array, :string}, params_acc, vars, env)
    {max_expansion, params_acc} = Builder.escape(max_expansion, :integer, params_acc, vars, env)

    {{:{}, [], [:phrase_prefix, [], [field, phrases, max_expansion]]}, params_acc}
  end

  def escape({range, _, [field, start, stop, bounds]}, _type, params_acc, vars, env)
      when range in ~w(int4range int8range daterange tsrange tstzrange)a do
    field = escape_field_param!(field, vars, range, 4)
    {start, params_acc} = Builder.escape(start, :any, params_acc, vars, env)
    {stop, params_acc} = Builder.escape(stop, :any, params_acc, vars, env)
    {bounds, params_acc} = Builder.escape(bounds, :string, params_acc, vars, env)

    {{:{}, [], [range, [], [field, start, stop, bounds]]}, params_acc}
  end

  def escape({:regex, _, [field, pattern]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :regex, 2)
    {pattern, params_acc} = Builder.escape(pattern, :string, params_acc, vars, env)

    {{:{}, [], [:regex, [], [field, pattern]]}, params_acc}
  end

  def escape({:term, _, [field, term]}, _type, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :term, 2)
    {term, params_acc} = Builder.escape(term, :any, params_acc, vars, env)

    {{:{}, [], [:term, [], [field, term]]}, params_acc}
  end

  def escape({:term_set, _, [bind, term_set]}, _type, params_acc, vars, env) do
    bind = escape_bind!(bind, vars, :term_set, 2)

    {term_set, params_acc} =
      Enum.map_reduce(term_set, params_acc, fn expr, params_acc ->
        escape_term!(expr, params_acc, vars, env)
      end)

    {{:{}, [], [:term_set, [], [bind, term_set]]}, params_acc}
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

  def escape(other, _type, _params_acc, _vars, _env) do
    error!("`#{Macro.to_string(other)}` is not a valid search expression")
  end

  defp escape_term!({:term, _, [field, term]}, params_acc, vars, env) do
    field = escape_field_param!(field, vars, :term, 2)
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
end
