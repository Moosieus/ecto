# v1 Todo:
* Properly handle multiple searches in one query.
* Implement all the query functions:
  * more_like_this
  * disjunction_max/3 (with tie_breaker)
  * boolean
  * `and`/`or`/`not`.
* Support the new sorting options.
* Make searching require an Ecto schema in the from clause.
* Figure out the compile time query building.
* Figure out the search query plan caching.
* Support querying and mapping `score_bm25`.
* Support aggregations and facets.
* Support highlighting.
* Support hybrid search.
* Write documentation and test coverage for everything.
* Get some form of an audit done.
* Find a patch'n'publish method for the respective versions of ecto.

# vN Todo:
* Add a DSL for migrations.
* Allow dynamic schema names instead of enforcing `#{schema}_search_idx`.
* Support comparison operators (`==`, `!=`, `>`, `<`, `>=`, `<=`).
* Make ranges work dynamically.
* Add support for `fragment` in the search queries.
* Allow the schema to specify what fields are indexed.
