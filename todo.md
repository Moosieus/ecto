# v1 Todo:
* Fix the inspection that's broken due to `searches` containing `SearchOpts` now.
* Figure out the compile-time/runtime query building.
* Figure out the search query plan caching.
* Support querying and mapping `score_bm25`.
* Implement `more_like_this` search query.
* Allow atoms in `order_by/2` and `boolean/2` to be variables in addition to atom literals.
* Map search indexes within Ecto schemas:
  * The index name
  * The key field
  * Map what fields are available
  * What type they are (namely for ranges)
* Support aggregations and facets.
* Support highlighting.
* Support hybrid search.
* Write API documentation.
* Write test coverage for everything.
* Get some form of an audit done.
* Find a patch'n'publish method for the respective versions of ecto.

# vN Todo:
* Add a DSL for migrations.
* Allow dynamic schema names instead of enforcing `#{schema}_search_idx`.
* Support comparison operators (`==`, `!=`, `>`, `<`, `>=`, `<=`).
* Make ranges work dynamically.
* Add support for `fragment` in the search queries.
* Allow the schema to specify what fields are indexed.
