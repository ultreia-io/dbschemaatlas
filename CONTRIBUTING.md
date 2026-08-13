# Contributing to dbschemaatlas

Thank you for considering a contribution to `dbschemaatlas`.

## Before opening a change

- Search the existing issues before opening a new one.
- Use the bug report template for reproducible defects.
- Discuss significant API changes in an issue before implementing them.
- Never include credentials, connection strings, or private database content.

## Development setup

Install R 4.0 or later, clone the repository, and install the development
dependencies:

```r
install.packages("devtools")
devtools::install_dev_deps()
```

Load the package without installing it:

```r
devtools::load_all()
```

## Making changes

- Keep helpers database-schema unless the change explicitly introduces an
  optional backend integration.
- Use two spaces for indentation and keep functions focused.
- Add or update roxygen2 documentation for every user-facing API change.
- Add tests that reproduce defects and cover new behavior.
- Edit `README.Rmd` rather than generated `README.md`.
- Add user-visible changes to `NEWS.md`.

Regenerate and validate the package before submitting a pull request:

```r
devtools::build_readme()
devtools::document()
devtools::test()
devtools::check()
```

The expected check result is:

```text
0 errors | 0 warnings | 0 notes
```

## Coverage

Generate a local interactive coverage report with:

```r
install.packages(c("covr", "DT", "htmltools"))
coverage <- covr::package_coverage()
coverage
covr::report(coverage)
```

Coverage helps identify untested behavior, but meaningful tests are more
important than reaching a particular percentage.

## Pull requests

Keep pull requests small enough to review comfortably. Explain the motivation,
describe the chosen implementation, and include the validation commands you
ran. By participating, you agree to follow the
[Code of Conduct](CODE_OF_CONDUCT.md).
