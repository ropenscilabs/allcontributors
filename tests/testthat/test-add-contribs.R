
context("add-contribs")

av <- "https://avatars1.githubusercontent.com/u/6697851?v=4"
ctbs <- data.frame (logins = c ("a", "b", "c"),
                    contributions = c (137L, NA_integer_, NA_integer_),
                    avatar = rep (av, 3),
                    type = c ("code", "issue_authors", "issue_contributors"))
attr (ctbs, "num_sections") <- 3
section_names <- c ("Code", "Issue Authors", "Issue Contributors")
ctbs$type_name <- section_names [match (ctbs$type,
                                        c ("code",
                                           "issue_authors",
                                           "issue_contributors"))]

or <- list (org = "hypertidy", repo = "geodist")

test_that ("add-contributors", {

              f <- tempfile (fileext = ".Rmd")
              writeLines ("", f)
              chk <- add_contribs_to_files (ctbs, or, ncols = 7,
                                            format = "grid", files = f,
                                            open_issue = FALSE)

              expect_true (chk)
              chk <- add_contribs_to_files (ctbs, or, ncols = 7,
                                            format = "grid", files = f,
                                            open_issue = FALSE)
              expect_false (chk) # already written, so nothing happens

              x <- readLines (f)
              expect_true (any (x == "## Contributors"))
              expect_true (any (grepl ("<!-- ALL-CONTRIBUTORS-LIST", x)))

              index <- grep ("^###\\s", x)
              expect_equal (length (index), 3)
             })

test_that ("sections", {
               expect_identical (unique (ctbs$type_name),
                                 c ("Code", "Issue Authors",
                                    "Issue Contributors"))

               ctbs2 <- ctbs
               attr (ctbs2, "num_sections") <- 2
               ctbs2 <- rename_default_sections (ctbs2)
               expect_identical (unique (ctbs2$type_name), c ("Code", "Issues"))

               f <- tempfile (fileext = ".Rmd")
               writeLines ("", f)
               chk <- add_contribs_to_files (ctbs2, or, ncols = 7,
                                             format = "grid", files = f,
                                             open_issue = FALSE)
               expect_true (chk)
               x <- readLines (f)
               index <- grep ("^###\\s", x)
               expect_equal (length (index), 2)

               ctbs1 <- ctbs
               attr (ctbs1, "num_sections") <- 1
               ctbs1 <- rename_default_sections (ctbs1)
               expect_identical (unique (ctbs1$type_name), "")

               f <- tempfile (fileext = ".Rmd")
               writeLines ("", f)
               chk <- add_contribs_to_files (ctbs1, or, ncols = 7,
                                             format = "grid", files = f,
                                             open_issue = FALSE)
               expect_true (chk)
               x <- readLines (f)
               index <- grep ("^###\\s", x)
               expect_equal (length (index), 0)
             })

test_that ("ghql-calls", {
               skip_on_cran ()

               # fails on github:

               #x <- get_contributors (org = "hypertidy", repo = "geodist")
               #expect_is (x, "data.frame")
               #expect_identical (names (x),
               #                  c ("logins", "contributions",
               #                     "avatar", "type"))
               #expect_true (all (unique (x$type) %in% c ("code",
               #                                          "issue_authors",
               #                                          "issue_contributors")))
             })
