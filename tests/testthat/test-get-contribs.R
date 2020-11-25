context("webmockr")

u_base <- "https://api.github.com"
path <- "repos/hypertidy/geodist/contributors"
u <- httr::modify_url (u_base, path = path)
#z <- httr::GET (u)
#saveRDS (httr::content (z), file = "geodist.Rds")
#saveRDS (z$headers, file = "geodist-hdrs.Rds")

library (webmockr)
httr_mock()

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true"))
if (test_all) {
    tok1 <- Sys.getenv ("GITHUB_TOKEN")
    tok2 <- Sys.getenv ("GITHUB_GRAPHQL_TOKEN")
    Sys.unsetenv ("GITHUB_TOKEN")
    Sys.unsetenv ("GITHUB_GRAPHQL_TOKEN")
}


stub_request ("get", uri = u) %>%
    wi_th (
           headers = list ("Accept" =
                           "application/json, text/xml, application/xml, */*")
           ) %>%
    to_return(status = 200,
              body = readRDS ("geodist.Rds"),
              headers = readRDS ("geodist-hdrs.Rds"))

test_that("vcr test", {

    org <- "hypertidy"
    repo <- "geodist"
    x <- get_gh_code_contributors (org, repo)

    expect_is (x, "data.frame")
    expect_equal (ncol (x), 3)
    expect_identical (names (x), c ("logins", "contributions", "avatar"))

             })

if (test_all) {
    Sys.setenv ("GITHUB_TOKEN" = tok1)
    Sys.setenv ("GITHUB_GRAPHQL_TOKEN" = tok2)
}
