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

## Usage

The package has only two functions, `get_contributors()`, to return a
list of repository contributors, and the primary function,
`add_contributors()`, which renders the results of this function in a
table in the `README.Rmd` and `README.md` files.

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
