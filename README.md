<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/allcontributor/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/allcontributor/actions?query=workflow%3AR-CMD-check)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Acknowledge all contributors in your ‘README’. Just an alternative
implementation in R of the original
[`all-contributors`](https://github.com/all-contributors/all-contributors),
but at the moment restricted to automatic addition of direct code
contributors only.

## Installation

Currently only on github, so package must be installed with

``` r
remotes::install_github("mpadge/allcontributor")
```

The package can then be loaded the usual way:

``` r
library (allcontributor)
```

## Why then?

Just so you can do this in R, without otherwise having to install the
`all-contributions` Bot on github, or the javascript client locally.

## Usage

The primary function of the package, `add_contributors()`, adds a table
of all contributors to the main `README.md` file (and `README.Rmd` if
that exists). These are obtained by querying the github API, for which a
local key should be set as an environmental variable containing the name
`"GITHUB"` (either via `Sys.setenv()`, or as an equivalent entry in a
file `~/.Renviron`).

The main `README` file(s) must also contain a markdown section entitled
`"Contributors"`, which must be manually inserted in the desired place
of both `"README.Rmd"` and `"README.md"` files prior to first using this
package.

The package has only two functions, `get_contributors()`, to return a
list of repository contributors, and the primary function,
`add_contributors()`, which renders the results of this function in a
table in the `README.Rmd` and `README.md` files.

``` r
add_contributors()
```

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->

<!-- prettier-ignore-start -->

<!-- markdownlint-disable -->

This project follows the [all-contributors](https://allcontributors.org)
specification. Contributions of any kind are welcome\!

<table>

<tr>

<td align="center">

<a href="https://github.com/mpadge">
<img src="https://avatars1.githubusercontent.com/u/6697851?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/mpadge/allcontributor/commits?author=mpadge">mpadge</a>

</td>

</tr>

</table>

<!-- markdownlint-enable -->

<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
