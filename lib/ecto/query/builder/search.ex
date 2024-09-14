import Kernel, except: [apply: 2]

defmodule Ecto.Query.Builder.Search do
  @moduledoc false

  alias Ecto.Query.Builder
  alias Ecto.Query.SearchBuilder

  import Ecto.Query.SearchBuilder, only: [
    escape_field_param!: 5,
    escape_bind!: 5
  ]

  @search_options [
    :limit_rows,
    :offset_rows,
    :stable_sort,
    :order_by
  ]

  @params_acc {[], %{bind: nil}}

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
          build_option(type, expr, params, acc.bind, env)
        type == :search ->
          build_query(op, expr, params, acc.bind, env)
      end

    Builder.apply_query(query, __MODULE__, [quoted], env)
  end

  ## Search options (only allowed at the top level)

  def escape({type, _, [bind, value]} = _expr, vars, env) when type in [:limit_rows, :offset_rows] do
    {_, params_acc} = escape_bind!(bind, @params_acc, vars, type, 2)

    value = Builder.escape(value, :integer, params_acc, vars, env)

    {type, value}
  end

  def escape({:stable_sort, _, [bind, value]} = _expr, vars, env) do
    {_, params_acc} = escape_bind!(bind, @params_acc, vars, :stable_sort, 2)

    value = Builder.escape(value, :boolean, params_acc, vars, env)

    {:stable_sort, value}
  end

  def escape({:order_by, _, [field, direction]} = _expr, vars, _env)
      when direction in [:asc, :desc] do
    {field, params_acc} = escape_field_param!(field, @params_acc, vars, :order_by, 2)

    {:order_by, {{field, direction}, params_acc}}
  end

  ## Search queries

  def escape(expr, vars, env) do
    expr = SearchBuilder.escape(expr, @params_acc, vars, env)

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

  defp build_option(option, expr, params, bind, env) when option in @search_options do
    quote do: %Ecto.Query.SearchOpt{
      name: unquote(option),
      expr: unquote(expr),
      params: unquote(params),
      bind: unquote(bind),
      file: unquote(env.file),
      line: unquote(env.line)
    }
  end

  def search!() do
    # SEARCH_TODO: Implement this (I think)
  end

  @doc """
  The callback applied by `build/4` to build the query.
  """
  @spec apply(Ecto.Queryable.t, term) :: Ecto.Query.t
  def apply(%Ecto.Query{searches: searches} = query, expr), do:
    %{query | searches: searches ++ [expr]}

  def apply(query, expr) do
    apply(Ecto.Queryable.to_query(query), expr)
  end
end
