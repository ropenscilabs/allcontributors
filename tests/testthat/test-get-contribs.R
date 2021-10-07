context("get contributors")

test_that("get_contributors", {

    vcr::use_cassette("get_contribs", {

        x <- get_contributors (org = "hypertidy", repo = "geodist")
    })

    expect_is (x, "data.frame")
    expect_equal (ncol (x), 4)
    expect_identical (names (x),
                      c ("logins", "contributions", "avatar", "type"))
    expect_true (all (unique (x$type) %in% c ("code",
                                              "issue_authors",
                                              "issue_contributors")))

})
