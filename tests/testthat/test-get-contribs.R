context("vcr")

#library (vcr)
#library (webmockr)
#u <- "https://api.github.com"
#path <- "repos/hypertidy/geodist/contributors"
#vcr_configure (dir = file.path (here::here (), "tests", "testthat"))
#y <- use_cassette(name = "geodist", {
#             z <- httr::GET(u, path = path)
#})

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))


vcr::insert_cassette ("geodist")

test_that("vcr test", {

    org <- "hypertidy"
    repo <- "geodist"
    x <- get_contributors (org, repo)

    expect_is (x, "data.frame")
    expect_equal (ncol (x), 3)
    expect_identical (names (x), c ("logins", "contributions", "avatar"))

             })

vcr::eject_cassette ("geodist")

