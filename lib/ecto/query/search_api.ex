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
  Indiscriminately matches every document in the index, assigning a uniform score of 1.0 to each ([ref](https://docs.paradedb.com/api-reference/full-text/complex#all)).

      from(
        p in Post,
        search: all(p)
      )
  """
  def all(bind), do: doc!([bind])

  @doc """
  Wraps a query to amplify its scoring impact without altering the set of matched documents ([ref](https://docs.paradedb.com/api-reference/full-text/complex#boost)).

      from(
        p in Post,
        search: boost(parse(p, "body:elixir"), 2.5)
      )
  """
  def boost(query, boost), do: doc!([query, boost])

  @doc """
  Applies a constant score across all documents matched by the underlying query.
  It can avoid unnecessary score computation on the wrapped query ([ref](https://docs.paradedb.com/api-reference/full-text/complex#const-score)).

      from(
        p in Post,
        search: const_score(parse(p, "body:elixir"), 1.0)
      )
  """
  def const_score(query, score), do: doc!([query, score])

  @doc """
  Returns documents that match one or more of the specified subqueries. If a document matches multiple criteria,
  it receives the highest score from those matches, with a slight increase for any additional matches
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#disjunction-max)).

      from(
        f in FoodTruck,
        search: disjunction_max([
          parse(f, "description:coffee"),
          parse(f, "description:vegetarian OR description:vegan")
        ])
      )
  """
  def disjunction_max(disjuncts), do: doc!([disjuncts])

  @doc """
  Serves as a placeholder, matching no documents.
  It’s useful for testing scenarios or specific edge cases ([ref](https://docs.paradedb.com/api-reference/full-text/complex#empty)).

      from(
        p in Post,
        search: empty(p)
      )
  """
  def empty(bind), do: doc!([bind])

  @doc """
  Matches all documents with a non-nil value in the specified field.
  All matched documents get a BM25 score of 1.0 ([ref](https://docs.paradedb.com/api-reference/full-text/complex#exists)).

      from(
        p in Post,
        search: exists(p.rating)
      )
  """
  def exists(field), do: doc!([field])

  @doc """
  Obtains search results that approximately match the query term, accommodating minor typos in the input.
  This enhances the search experience by providing relevant results even when the query is not spelled correctly
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#fuzzy-term)).

      from(
        p in Post,
        search: fuzz_term(p.title, "Eilxir", distance: 1)
      )

  The following options are available:
  * `:distance` - The maximum Levenshtein distance (i.e. single character edits)
  allowed to consider a term in the index as a match for the query term.
  Defaults to `2`, which is the maximum permitted.

  * `:transpose_cost_one` - When set to `true`, transpositions (swapping two adjacent characters)
  as a single edit in the Levenshtein distance calculation, while `false` considers it two separate
  edits (a deletion and an insertion). Defaults to `true`.

  * `prefix` - When set to `true`, the initial substring (prefix) of the query term is exempted from
  the fuzzy edit distance calculation, while `false` includes the entire string in the calculation.
  Defaults to `false`.

  > #### Options must be keyword literals {. :warning}
  > The following is okay:
  > ```elixir
  > x = 1
  > fuzzy_term(p.title, "Eilxir", distance: ^x)
  > ```
  > whereas this will not:
  > ```elixir
  > x = [distance: 1]
  > fuzzy_term(p.title, "Eilxir", ^x)
  """
  def fuzzy_term(bind, string, options \\ []), do: doc!([bind, string, options])

  @doc """
  Searches for documents containing an exact sequence of words, with slop allowing for some
  flexibility in term proximity. This query type also requires position indexing
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#phrase)).

      from(
        p in Post,
        search: phrase(p.body, ["robot", "building", "kit"], 2)
      )
  """
  def phrase(field, phrases, slop \\ 0), do: doc!([field, phrases, slop])

  @doc """
  Identifies documents containing a given sequence of words followed by a term prefix,
  requiring the indexing of positions for the search field
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#phrase)).

      from(
        p in Post,
        search: phrase_prefix(p.body, ["har"])
      )
  """
  def phrase_prefix(field, phrases), do: doc!([field, phrases])

  @doc """
  Same as `phrase_prefix/2` with a max_expansion as an additional argument. Limits the number of
  term variations that the prefix can expand to during the search. This helps in controlling the
  breadth of the search by setting a cap on how many different terms the prefix can match
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#phrase-prefix)).

      from(
        p in Post,
        search: phrase_prefix(p.body, ["har"], 3)
      )
  """
  def phrase_prefix(field, phrases, max_expansion), do: doc!([field, phrases, max_expansion])


  @doc """
  Finds documents containing terms that match a specific regex pattern,
  enabling pattern-based searching ([ref](https://docs.paradedb.com/api-reference/full-text/complex#regex)).

      from(
        p in Post,
        search: regex(p.body, "(foo|bar|baz)")
      )
  """
  def regex(field, pattern), do: doc!([field, pattern])

  @doc """
  Matches documents containing a specified term, with scoring based on term frequency,
  inverse document frequency, and field normalization. The term value is treated as a
  token and is not tokenized further. It is matched directly against terms in the index
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#term)).

      from(
        p in Post,
        search: term(p.category, "travel")
      )
  """
  def term(field, term), do: doc!([field, term])

  @doc """
  Matches documents containing any term from a specified set, offering flexibility in matching criteria
  ([ref](https://docs.paradedb.com/api-reference/full-text/complex#termset)).

      from(
        p in Post,
        search: term_set([
          term(p.rating, 5),
          term(p.category, "travel")
        ])
      )
  """
  def term_set(field, term_set), do: doc!([field, term_set])

  @doc """
  Limits the number of results returned from the search query
  ([ref](https://docs.paradedb.com/api-reference/full-text/bm25#limit-and-offset)).

      from(
        p in Post,
        search: all(p),
        search: limit_rows(p, 10)
      )
  """
  def limit_rows(bind, limit), do: doc!([bind, limit])

  @doc """
  Provides pagination when used with `limit_rows/2`.
  ([ref](https://docs.paradedb.com/api-reference/full-text/bm25#limit-and-offset)).

      from(
        p in Post,
        search: all(p),
        search: limit_rows(p, 10),
        search: offset_rows(p, 3)
      )
  """
  def offset_rows(bind, offset), do: doc!([bind, offset])

  @doc """
  Specifies whether ParadeDB should stabilize the order of equally-scored results,
  at the cost of performance. Defaults to `false`.

      from(
        p in Post,
        search: term(p.category, "travel"),
        search: stable_sort(true)
      )
  """
  def stable_sort(bind, boolean), do: doc!([bind, boolean])

  @doc """
  Shorthand for the `order_by_field` and `order_by_direction` options
  ([ref](https://docs.paradedb.com/api-reference/full-text/bm25#custom-ordering)).

      from(
        p in Post,
        search: all(p),
        search: order_by(p.rating, :desc)
      )
  """
  def order_by(field, direction), do: doc!([field, direction])

  defp doc!(_) do
    raise "the functions in Ecto.Query.SearchAPI should not be invoked directly, " <>
            "they serve for documentation purposes only"
  end
end
