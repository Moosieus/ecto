defmodule Ecto.Query.SearchAPI do
  @moduledoc """
  Lists all the functions allowed in the search query API.
  """

  @dialyzer :no_return

  @doc """
  Searches `bind` for the [ParadeQL](https://docs.paradedb.com/api-reference/full-text/bm25#paradeql) query in `pdb_query`.

      from(
        p in Post,
        search: parse(p, "body:elixir")
      )
  """
  def parse(bind, pdb_query), do: doc!([bind, pdb_query])

  @doc """
  Indiscriminately matches every document in the index, assigning a uniform score of 1.0 to each.
  See: [paradedb.all](https://docs.paradedb.com/api-reference/full-text/complex#all).

      from(
        p in Post,
        search: all(p)
      )
  """
  def all(bind), do: doc!([bind])

  @doc """
  Wraps around another query to amplify its scoring impact, without altering the set of matched documents.
  See: [paradedb.boost](https://docs.paradedb.com/api-reference/full-text/complex#boost).

      from(
        p in Post,
        search: boost(parse(p, "body:elixir"), 2.5)
      )
  """
  def boost(query, boost), do: doc!([query, boost])

  @doc """
  Applies a constant score across all documents matched by the underlying query. It can avoid unnecessary score computation on the wrapped query.
  See: [paradedb.const_score](https://docs.paradedb.com/api-reference/full-text/complex#const-score)

      from(
        p in Post,
        search: const_score(parse(p, "body:elixir"), 1.0)
      )
  """
  def const_score(query, score), do: doc!([query, score])

  @doc """
  Serves as a placeholder, matching no documents. It’s useful for testing scenarios or specific edge cases.
  See: [paradedb.empty](https://docs.paradedb.com/api-reference/full-text/complex#empty)

      from(
        p in Post,
        search: empty(p)
      )
  """
  def empty(bind), do: doc!([bind])

  defp doc!(_) do
    raise "the functions in Ecto.Query.SearchAPI should not be invoked directly, " <>
            "they serve for documentation purposes only"
  end
end
