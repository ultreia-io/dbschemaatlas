# Offline fixture coverage

The installed fixture used by tests, README examples, vignettes, and pkgdown is
stored under `inst/extdata/example-metadata/` so it remains available after
package installation. It covers multiple schemas and tables, simple and
composite primary keys, simple and composite foreign keys, cross-schema
dependencies, nullable and mandatory columns, isolated tables, missing
optional descriptions, and multilingual descriptions.

`tests/testthat/helper-fixtures.R` provides the same scenarios as in-memory
`data.table` objects for focused unit tests.
