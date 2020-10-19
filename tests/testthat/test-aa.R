context("initial")

test_that("initial test", {

    u <- get_git_user ()
    tok <- get_gh_token ()
    expect_equal (u, "mpadge")
    expect_true (nchar (tok) > 0)

    ut <- httr::authenticate (u, tok)
    expect_is (ut, "request")
    upwd <- ut$options$userpwd
    expect_true (nchar (upwd) > 0)
    expect_true (grepl (":", upwd))
             })
