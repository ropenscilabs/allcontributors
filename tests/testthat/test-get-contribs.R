context ("get contributors")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

test_that ("get_contributors", {

    x <- with_mock_dir ("getcontribs", {
        get_contributors (org = "hypertidy", repo = "geodist")
    })

    expect_is (x, "data.frame")
    expect_equal (ncol (x), 4)
    expect_identical (
        names (x),
        c ("logins", "contributions", "avatar", "type")
    )
    expect_true (all (unique (x$type) %in% c (
        "code",
        "issue_authors",
        "issue_contributors"
    )))

})
