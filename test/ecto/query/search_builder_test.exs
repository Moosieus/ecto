Code.require_file "../../../integration_test/support/types.exs", __DIR__

defmodule Ecto.Query.SearchBuilderTest do
  use ExUnit.Case, async: true

  import Ecto.Query.Builder
  doctest Ecto.Query.Builder

  # todo: test the search query builder functions
end
