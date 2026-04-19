#test you get expected error if resource combination is not found ing gtr_combinations------------------
testthat::test_that(
  "unknown resource combination gives error",
  expect_error(
    gtR::query_resource_combination("organisation", "product", "01F2924C-FFB4-481B-B8F0-31234D33F0FA"),
    "'resource' and 'output' combination must be in the list of options as per 'gtr_combinations'",
    fixed = T
  )
)

#test you get expected error if choosing a size outside range------------------------
testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_combination("organisation", "project", "01F2924C-FFB4-481B-B8F0-31234D33F0FA", size = 101),
    "'size' must be an integer >= 10 and <= 100",
    fixed = T
  )
)

#test that you get a data frame or list as expected-----------------------------

testthat::test_that(
  "live API returns expected object types",
  {
    testthat::skip_if(Sys.getenv("GTR_RUN_LIVE_TESTS") != "true")

    #that you get a dataframe, assuming no API errors
    testthat::expect_s3_class(gtR::query_resource_combination("organisation", "project", "01F2924C-FFB4-481B-B8F0-31234D33F0FA"), "data.frame")

    #that you get a list assuming no API errors
    testthat::expect_s3_class(gtR::query_resource_combination("organisation", "project", "01F2924C-FFB4-481B-B8F0-31234D33F0FA", df_only = FALSE), "list")
  }
)

#that picking a non-logical df_only throws an error-----------------------

testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_combination("organisation", "project", "01F2924C-FFB4-481B-B8F0-31234D33F0FA", df_only = "lol"),
    "'df_only` must be logical (TRUE or FALSE)",
    fixed = T
  )
)
