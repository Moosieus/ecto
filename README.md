# ParadeDB Ecto
A experimental downstream fork that extends Ecto with support for [ParadeDB](https://www.paradedb.com/).

## Installation

This repo should work as a mostly drop-in replacement for Ecto in any existing project that uses it and Postgresql:

```elixir
def deps do
  [
    {:ecto, git: "https://github.com/moosieus/ecto", branch: "master"}
    {:ecto_sql, git: "https://github.com/moosieus/ecto_sql", branch: "master"}
  ]
end
```

## Usage

Once installed, you'll need to [create a search index](https://docs.paradedb.com/api-reference/full-text/index) within a migration as shown below. Tentatively, all search indexes must follow the `#{schema}_search_idx` naming convention.

```elixir
defmodule Project.Repo.Migrations.MigrationName do
  use Ecto.Migration

  def up do
    execute("""
    CALL paradedb.create_bm25(
      index_name => 'mock_items_search_idx',
      table_name => 'mock_items',
      key_field => 'id',
      text_fields => paradedb.field('description'),
      numeric_fields => paradedb.field('rating')
    );
    """)
  end

  def down do
    execute("""
    CALL paradedb.drop_bm25(
      index_name => 'mock_items_search_idx',
      schema_name => 'public'
    )
    """)
  end
end
```

Afterwards, search queries can be performed via the `search:` and `or_search:` macros:
```elixir
from(
  i in MockItems,
  search: parse(i, "description:shoes")
)
```

Hexdocs for this repo can be generated with `MIX_ENV=docs mix docs`.
