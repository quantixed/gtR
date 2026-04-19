#you've 'kind of' run tests in some vignettes anyway and we know some query combinations are working.
#This is to test an outstanding type

#test you get results for query where both output and resource are in endpoints[1:7]

testthat::test_that(
  "live API returns expected object types",
  {
    testthat::skip_if(Sys.getenv("GTR_RUN_LIVE_TESTS") != "true")

    #that you get a dataframe, assuming no API errors
    testthat::expect_s3_class(
      get_resources(
        "organisation",
        "project",
        "01F2924C-FFB4-481B-B8F0-31234D33F0FA",
        size = 10,
        page_nums = 1
      ),
      "data.frame"
    )
  }
)
