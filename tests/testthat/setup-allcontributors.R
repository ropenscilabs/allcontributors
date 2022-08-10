library ("vcr") # *Required* as vcr is set up on loading
invisible (vcr::vcr_configure (
    dir = vcr::vcr_test_path ("fixtures"),
    filter_request_headers = "Authorization"
))
vcr::check_cassette_names ()
