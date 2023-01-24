function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "https://api.github.com/repos/hypertidy/geodist/",
        "api/",
        fixed = TRUE
    )

    return (resp)
}
