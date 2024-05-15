# Development

## Minor changes

- Add a rate limit checker for the GitHub API calls. Will warn if rate limit is exceeded (thanks to @chartgerink via #37).
- Expose `check_urls' parameter in main `add_contributors` function, to allow that to be switched off (thanks to @sbfnk via #38).
- Add error handling to the URL checking function so failures do not cause function errors (also thanks to @sbfnk via #38).

---

# v 0.1.1

- Fix bug in issue contributors when github account no longer exists

---

# v 0.1.0

## Major changes:

- Add new `exclude_labels` and `exclude_issues` parameters to main functions

## Minor changes:

- Add `_pkgdown` file for function reference topics
