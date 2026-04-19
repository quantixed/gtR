#' Query projects by funder string in chunks.
#'
#' Convenience wrapper around `query_projects_by_funder()` for large pulls.
#' It resolves matching project IDs once, then scans project pages in chunks and
#' reports progress to reduce the chance of long silent runs.
#' @param funder Funder string to search for in the funds endpoint (for example, "BBSRC").
#' @param size The number of results to return per page (min 10, max 100).
#' @param page_nums Optional page numbers of project results to scan. If omitted,
#' all project pages are scanned.
#' @param chunk_size Number of pages to process per chunk (must be a positive integer).
#' @param show_progress Logical indicating whether chunk progress messages should be shown.
#' @param request_pause Base pause in seconds between page requests.
#' @param max_page_retries Maximum retry attempts for a single throttled page request.
#' @param skip_failed_pages Logical indicating whether pages that keep returning
#' server errors after retries should be skipped.
#' @param df_only Choose whether you only want a dataframe of matched projects (T)
#' or a list containing matched projects and metadata (F).
#' @return A dataframe of projects linked to funds matching the supplied funder
#' string, or a list with projects plus metadata.
#' @export

query_projects_by_funder_chunked <- function(
  funder,
  size = 100,
  page_nums,
  chunk_size = 25,
  show_progress = TRUE,
  request_pause = 1,
  max_page_retries = 6,
  skip_failed_pages = TRUE,
  df_only = T
) {

  if(!is.numeric(chunk_size) || length(chunk_size) != 1 || is.na(chunk_size) || chunk_size < 1 || chunk_size != round(chunk_size)) {
    stop("'chunk_size' must be a positive integer")
  }

  if(!is.logical(show_progress) || length(show_progress) != 1 || is.na(show_progress)) {
    stop("'show_progress' must be logical (TRUE or FALSE)")
  }

  if(!is.numeric(request_pause) || length(request_pause) != 1 || is.na(request_pause) || request_pause < 0) {
    stop("'request_pause' must be a non-negative number")
  }

  if(!is.numeric(max_page_retries) || length(max_page_retries) != 1 || is.na(max_page_retries) || max_page_retries < 1 || max_page_retries != round(max_page_retries)) {
    stop("'max_page_retries' must be a positive integer")
  }

  if(!is.logical(skip_failed_pages) || length(skip_failed_pages) != 1 || is.na(skip_failed_pages)) {
    stop("'skip_failed_pages' must be logical (TRUE or FALSE)")
  }

  get_retry_wait <- function(attempt_i) {
    min(60, request_pause + 2^attempt_i)
  }

  fetch_project_page <- function(page_i) {

    last_err <- NULL

    for(attempt_i in seq_len(as.integer(max_page_retries))) {

      if(request_pause > 0) {
        Sys.sleep(request_pause)
      }

      page_result <- tryCatch(
        gtR::query_resource_all(
          resource = "project",
          size = size,
          page_num = page_i,
          df_only = TRUE
        ),
        error = function(e) e
      )

      if(!inherits(page_result, "error")) {
        return(page_result)
      }

      last_err <- page_result
      err_msg <- conditionMessage(page_result)
      is_rate_limited <- grepl("429|Too Many Requests", err_msg, ignore.case = TRUE)
      is_server_error <- grepl("500|Internal Server Error", err_msg, ignore.case = TRUE)

      if(attempt_i == as.integer(max_page_retries) && is_server_error && skip_failed_pages) {
        if(show_progress) {
          message(
            sprintf(
              "Skipping page %s after %s failed attempts due to repeated server errors.",
              page_i,
              as.integer(max_page_retries)
            )
          )
        }
        return(data.frame())
      }

      if(!is_rate_limited && !is_server_error) {
        stop(last_err)
      }

      if(attempt_i == as.integer(max_page_retries)) {
        stop(last_err)
      }

      wait_seconds <- get_retry_wait(attempt_i)

      if(show_progress) {
        message(
          sprintf(
            "Rate-limited on page %s. Retry %s/%s in %.1f seconds.",
            page_i,
            attempt_i,
            as.integer(max_page_retries),
            wait_seconds
          )
        )
      }

      Sys.sleep(wait_seconds)
    }

    stop(last_err)
  }

  # Resolve linked project IDs once; these are used to filter chunked pages.
  seed_result <- gtR::query_projects_by_funder(
    funder = funder,
    size = size,
    page_nums = 1,
    df_only = FALSE
  )

  project_ids <- seed_result$project_ids
  fund_ids <- seed_result$fund_ids

  project_page_1 <- gtR::query_resource_all(
    resource = "project",
    size = size,
    page_num = 1,
    df_only = FALSE
  )

  if(missing(page_nums)) {
    pages_to_scan <- seq_len(project_page_1$totalPages)
  } else {
    if(!is.numeric(page_nums) || any(is.na(page_nums)) || any(page_nums < 1) || any(page_nums != round(page_nums))) {
      stop("'page_nums' must be a vector of positive integers")
    }
    pages_to_scan <- unique(as.integer(page_nums))
    valid_pages <- pages_to_scan[pages_to_scan <= project_page_1$totalPages]

    if(length(valid_pages) < length(pages_to_scan) && show_progress) {
      message(
        sprintf(
          "Ignoring page numbers beyond available range 1-%s.",
          project_page_1$totalPages
        )
      )
    }

    pages_to_scan <- valid_pages

    if(length(pages_to_scan) == 0) {
      stop("No valid 'page_nums' remain after limiting to available project pages")
    }
  }

  page_chunks <- split(
    pages_to_scan,
    ceiling(seq_along(pages_to_scan) / as.integer(chunk_size))
  )

  chunk_results <- vector("list", length(page_chunks))

  for(chunk_i in seq_along(page_chunks)) {

    pages_this_chunk <- page_chunks[[chunk_i]]

    if(show_progress) {
      message(
        sprintf(
          "Processing chunk %s/%s (pages %s-%s)",
          chunk_i,
          length(page_chunks),
          min(pages_this_chunk),
          max(pages_this_chunk)
        )
      )
    }

    chunk_pages <- lapply(pages_this_chunk, function(page_i) {
      fetch_project_page(page_i)
    })

    chunk_df <- dplyr::bind_rows(chunk_pages)

    if("id" %in% names(chunk_df)) {
      chunk_df <- chunk_df[chunk_df$id %in% project_ids, ]
    } else {
      chunk_df <- data.frame()
    }

    chunk_results[[chunk_i]] <- chunk_df
  }

  projects_df <- dplyr::bind_rows(chunk_results)

  if("id" %in% names(projects_df)) {
    projects_df <- projects_df[!duplicated(projects_df$id), ]
  }

  if(df_only == TRUE) {
    return(as.data.frame(projects_df))
  }

  list(
    "project" = as.data.frame(projects_df),
    "fund_ids" = fund_ids,
    "project_ids" = project_ids,
    "project_pages_scanned" = pages_to_scan,
    "chunk_size" = as.integer(chunk_size),
    "request_pause" = request_pause,
    "max_page_retries" = as.integer(max_page_retries),
    "chunks_processed" = length(page_chunks)
  )
}
