import Kernel, except: [apply: 3]

defmodule Ecto.Query.Builder.Search do
  @moduledoc false

  alias Ecto.Query.Builder
  alias Ecto.Query.SearchBuilder

  @search_options [
    :limit,
    :offset,
    :stable_sort
  ]

  # SEARCH_TODO: Limit search queries to a single table binding.

  @doc """
  Builds a quoted expression.

  The quoted expression should evaluate to a query at runtime.
  If possible, it does all calculations at compile time to avoid
  runtime work.
  """
  @spec build(:and | :or, Macro.t, [Macro.t], Macro.t, Macro.Env.t) :: Macro.t
  def build(op, query, _binding, {:^, _, [var]}, env) do
    quote do
      Ecto.Query.Builder.Search.search!(unquote(op), unquote(query),
                                        unquote(var), 0, unquote(env.file), unquote(env.line))
    end
  end

  @spec build(:and | :or, Macro.t, [Macro.t], Macro.t, Macro.Env.t) :: Macro.t
  def build(op, query, binding, expr, env) do
    {query, binding} = Builder.escape_binding(query, binding, env)

    {type, {expr, {params, acc}}} = escape(expr, binding, env)

    params = Builder.escape_params(params)

    quoted =
      cond do
        type in @search_options ->
          build_option(type, expr, params, env)
        type == :search ->
          build_query(op, expr, params, acc.bind, env)
      end

    Builder.apply_query(query, __MODULE__, [type, quoted], env)
  end

  ## Escape top-level search options

  def escape({type, _, [{bind, _, nil}, value]} = _expr, vars, env)
      when is_atom(bind) and type in [:limit, :offset] do

    value = Builder.escape(value, :integer, {[], %{}}, vars, env)

    {type, value}
  end

  def escape({:stable_sort, _, [{bind, _, nil}, value]} = _expr, vars, env)
      when is_atom(bind) do

    value = Builder.escape(value, :boolean, {[], %{}}, vars, env)

    {:stable_sort, value}
  end

  ## Escape everything else

  # SEARCH_TODO:
  # * Reject multiple bindings in the same search line.
  # * Add the planned ParadeDB expressions.
  # * Need to account for "subqueries" in a sense...

  def escape(expr, vars, env) do
    expr = SearchBuilder.escape(expr, {[], %{bind: nil}}, vars, env)

    {:search, expr}
  end

  defp build_query(op, expr, params, bind, env) do
    quote do: %Ecto.Query.SearchExpr{
      expr: unquote(expr),
      op: unquote(op),
      params: unquote(params),
      bind: unquote(bind),
      file: unquote(env.file),
      line: unquote(env.line)
    }
  end

  defp build_option(type, expr, params, env) when type in @search_options do
    quote do: %Ecto.Query.QueryExpr{
      expr: unquote(expr),
      params: unquote(params),
      file: unquote(env.file),
      line: unquote(env.line)
    }
  end

  def search!() do
    # I'll need this at some point!
    # Can't store the search as a struct, needs to be flattened into the parent query.
  end

  @doc """
  The callback applied by `build/4` to build the query.
  """
  @spec apply(Ecto.Queryable.t, :search | :limit | :offset | :stable_sort, term) :: Ecto.Query.t
  def apply(%Ecto.Query{} = query, :limit, expr), do:
    %{query | search_limit: expr}

  def apply(%Ecto.Query{} = query, :offset, expr), do:
    %{query | search_offset: expr}

  def apply(%Ecto.Query{} = query, :stable_sort, expr), do:
    %{query | search_stable_sort: expr}

  def apply(%Ecto.Query{searches: searches} = query, :search, expr), do:
    %{query | searches: searches ++ [expr]}

  def apply(query, option, expr) do
    apply(Ecto.Queryable.to_query(query), option, expr)
  end
end
