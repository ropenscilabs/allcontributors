<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/allcontributor/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/allcontributor/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/mpadge/allcontributor/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/allcontributor)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

An alternative implementation in R of the original
[`all-contributors`](https://allcontributors.org/) to acknowledge all
contributors in your ‘README’ (or elsewhere).

## Why then?

The original [`all-contributors`](https://allcontributors.org/) is
primarily a bot which responds to commit messages such as `add @user for
<contribution>`, where `<contribution>` is one of the [recognized
types](https://allcontributors.org/docs/en/emoji-key). The relative
advantage of this system lies primarily in the diversity of contribution
types able to be acknowledged, with each type for a given user appearing
as a corresponding
[emoji](https://allcontributors.org/docs/en/emoji-key) below their
github avatar as listed on the README. In comparison, the advantages of
the `allcontributor` package are:

1.  It works locally without any bot integration with a repository
2.  It can add contributors to any file, not just the main README
3.  It offers a variety of formats for listing contributors:
    1)  divided into sections by types of contributions, or as a single
        section
    2)  presented as full grids (like [the
        original](https://github.com/all-contributors/all-contributors/blob/master/README.md#contributors-)),
        numbered lists of github user names only, or single text strings
        of comma-separated names.

## Installation

Not yet on CRAN, so must be installed from remote repository host
systems using any one of the following options:

``` r
# install.packages("remotes")
remotes::install_git("https://git.sr.ht/~mpadge/allcontributor")
remotes::install_bitbucket("mpadge/allcontributor")
remotes::install_gitlab("mpadge/allcontributor")
remotes::install_github("mpadge/allcontributor")
```

The package can then be loaded the usual way:

``` r
library (allcontributor)
```

## Usage

The primary function of the package,
[`add_contributors()`](https://mpadge.github.io/allcontributor/reference/add_contributors.html),
adds a table of all contributors to the main `README.md` file (and
`README.Rmd` if that exists). Tables can be added to other files by
specifying the `files` argument of that function. The appearance of the
contributors table is determined by several parameters in that function,
including:

1.  `type` For the type of contributions to include (code, contributors
    who open issues, contributors who discuss issues).
2.  `num_sections` For whether to present contributors in 1, 2, or 3
    distinct sections, dependent upon which `type`s of contributions are
    to be acknowledged.
3.  `format` Determining whether contributors are presented in a grid
    with associated avatars of each contributor, as in [the
    original](https://github.com/all-contributors/all-contributors/blob/master/README.md#contributors-),
    an enumerated list of github user names only, or a single text
    string of comma-separated names.

Contribution data are obtained by querying the github API, for which a
local key should be set as an environmental variable containing the name
`"GITHUB"` (either via `Sys.setenv()`, or as an equivalent entry in a
file `~/.Renviron`). The data used to construct the contributions table
can be extracted without writing to the `README` file(s) with the
function
[`get_contributors()`](https://mpadge.github.io/allcontributor/reference/get_contributors.html):

``` r
get_contributors(org = "mpadge", repo = "allcontributor")
#>   logins contributions                                               avatar
#> 1 mpadge            49 https://avatars1.githubusercontent.com/u/6697851?v=4
```

If the main `README` file(s) contains a markdown section entitled
`"Contributors"`, the
[`add_contributors()`](https://mpadge.github.io/allcontributor/reference/add_contributors.html)
function will add a table of contributors will there, otherwise it will
be appended to the end of the document(s). If you wish your contributors
table to be somewhere other than at the end of the `README` file(s),
start by adding an empty `"## Contributors` section to the file(s) and
the function will insert the table at that point.

Any time you wish to update your contributor list, simply re-run the
`add_contributors()` function.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->

<!-- prettier-ignore-start -->

<!-- markdownlint-disable -->

This project uses the [`allcontributor`
package](https://github.com/mpadge/allcontributor) following the
[all-contributors](https://allcontributors.org) specification.
Contributions of any kind are welcome\!

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
