#test you get expected error for invalid funder input------------------------
testthat::test_that(
  "empty funder gives error",
  expect_error(
    gtR::query_projects_by_funder(""),
    "'funder' must be a single non-empty character string",
    fixed = T
  )
)

#test you get expected error if choosing a size outside range------------------------
testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_projects_by_funder("BBSRC", size = 101),
    "'size' must be an integer >= 10 and <= 100",
    fixed = T
  )
)

testthat::test_that(
  "invalid chunk_size gives error",
  expect_error(
    gtR::query_projects_by_funder_chunked("BBSRC", chunk_size = 0),
    "'chunk_size' must be a positive integer",
    fixed = T
  )
)

testthat::test_that(
  "invalid show_progress gives error",
  expect_error(
    gtR::query_projects_by_funder_chunked("BBSRC", show_progress = "yes"),
    "'show_progress' must be logical (TRUE or FALSE)",
    fixed = T
  )
)

testthat::test_that(
  "invalid request_pause gives error",
  expect_error(
    gtR::query_projects_by_funder_chunked("BBSRC", request_pause = -1),
    "'request_pause' must be a non-negative number",
    fixed = T
  )
)

testthat::test_that(
  "invalid max_page_retries gives error",
  expect_error(
    gtR::query_projects_by_funder_chunked("BBSRC", max_page_retries = 0),
    "'max_page_retries' must be a positive integer",
    fixed = T
  )
)

testthat::test_that(
  "invalid skip_failed_pages gives error",
  expect_error(
    gtR::query_projects_by_funder_chunked("BBSRC", skip_failed_pages = "yes"),
    "'skip_failed_pages' must be logical (TRUE or FALSE)",
    fixed = T
  )
)

#optional live API tests (can be rate-limited by UKRI)-----------------------------
testthat::test_that(
  "live API returns expected object types",
  {
    testthat::skip_if(Sys.getenv("GTR_RUN_LIVE_TESTS") != "true")

    testthat::expect_s3_class(
      gtR::query_projects_by_funder("BBSRC", page_nums = 1),
      "data.frame"
    )

    testthat::expect_s3_class(
      gtR::query_projects_by_funder("BBSRC", page_nums = 1, df_only = F),
      "list"
    )

    testthat::expect_s3_class(
      gtR::query_projects_by_funder_chunked("BBSRC", page_nums = 1, chunk_size = 1, show_progress = FALSE),
      "data.frame"
    )

    testthat::expect_s3_class(
      gtR::query_projects_by_funder_chunked("BBSRC", page_nums = 1, chunk_size = 1, show_progress = FALSE, df_only = F),
      "list"
    )
  }
)
