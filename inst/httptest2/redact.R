function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "https://api.github.com/",
        "gh-api/",
        fixed = TRUE
    )
    resp <- httptest2::gsub_response (
        resp,
        "repos/hypertidy/",
        "/",
        fixed = TRUE
    )

    return (resp)
}
