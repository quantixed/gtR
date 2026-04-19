#' Query projects by funder string.
#'
#' Finds funds matching a funder search string, then scans projects and returns
#' those that are linked to any matched fund IDs.
#' @param funder Funder string to search for in the funds endpoint (for example, "BBSRC").
#' @param size The number of results to return per page (min 10, max 100).
#' @param page_nums Optional page numbers of project results to scan. If omitted,
#' all project pages are scanned.
#' @param df_only Choose whether you only want a dataframe of matched projects (T)
#' or a list containing matched projects and metadata (F).
#' @details For large result sets, consider using `query_projects_by_funder_chunked()`
#' to process project pages in chunks with progress messages.
#' @return A dataframe of projects linked to funds matching the supplied funder
#' string, or a list with projects plus metadata.
#' @export

query_projects_by_funder <- function(
  funder,
  size = 100,
  page_nums,
  df_only = T
) {

  #error handling---------------------------------------

  if(!is.character(funder) || length(funder) != 1 || is.na(funder) || trimws(funder) == "") {
    stop("'funder' must be a single non-empty character string")
  }

  if(size < 10 || size > 100 || size != round(size)) {
    stop("'size' must be an integer >= 10 and <= 100")
  }

  if(!missing(page_nums)) {
    if(!is.numeric(page_nums) || any(is.na(page_nums)) || any(page_nums < 1) || any(page_nums != round(page_nums))) {
      stop("'page_nums' must be a vector of positive integers")
    }
  }

  if(!is.logical(df_only)) {
    stop("'df_only` must be logical (TRUE or FALSE)")
  }

  #helpers---------------------------------------

  get_raw_response <- function(.url, .query_settings) {

    Sys.sleep(0.5)

    for(attempt_i in seq_len(4)) {

      result <- httr::GET(
        url = .url,
        query = .query_settings,
        timeout = httr::timeout(15)
      )

      if(!(result$status_code %in% c(429, 500, 502, 503, 504))) break

      if(attempt_i < 4) {
        Sys.sleep(2^attempt_i)
      }

    }

    if(result$status_code >= 400) {
      err_msg = httr::http_status(result)
      stop(err_msg)
    }

    jsonlite::fromJSON(
      httr::content(result, "text"),
      simplifyDataFrame = FALSE
    )
  }

  extract_fund_ids <- function(project_record) {

    if(is.null(project_record$links) || is.null(project_record$links$link)) return(character(0))

    link_list <- project_record$links$link

    # Normalise a singleton link object to a list for consistent iteration.
    if(!is.null(link_list$href)) {
      link_list <- list(link_list)
    }

    ids <- vapply(
      link_list,
      FUN.VALUE = character(1),
      FUN = function(link_item) {
        if(!is.null(link_item$rel) & link_item$rel == "FUND" & !is.null(link_item$href)) {
          sub("^.*/", "", link_item$href)
        } else {
          NA_character_
        }
      }
    )

    ids[!is.na(ids)]
  }

  extract_project_ids <- function(fund_record) {

    if(is.null(fund_record$links) || is.null(fund_record$links$link)) return(character(0))

    link_list <- fund_record$links$link

    if(!is.null(link_list$href)) {
      link_list <- list(link_list)
    }

    ids <- vapply(
      link_list,
      FUN.VALUE = character(1),
      FUN = function(link_item) {
        if(!is.null(link_item$rel) & link_item$rel == "FUNDED" & !is.null(link_item$href)) {
          sub("^.*/", "", link_item$href)
        } else {
          NA_character_
        }
      }
    )

    ids[!is.na(ids)]
  }

  projects_to_df <- function(project_list) {

    if(length(project_list) == 0) return(data.frame())

    jsonlite::fromJSON(
      jsonlite::toJSON(project_list, auto_unbox = TRUE),
      flatten = TRUE
    )
  }

  #find matching funds---------------------------------------

  funds_url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['fund']]}")

  funds_page_1 <- get_raw_response(
    .url = funds_url,
    .query_settings = list(
      q = funder,
      s = size,
      p = 1
    )
  )

  if(is.null(funds_page_1$fund) || length(funds_page_1$fund) == 0) {
    stop("No funds found matching this 'funder' search term")
  }

  all_funds <- funds_page_1$fund

  if(funds_page_1$totalPages > 1) {
    for(page_i in seq(2, funds_page_1$totalPages)) {
      tmp_funds <- get_raw_response(
        .url = funds_url,
        .query_settings = list(
          q = funder,
          s = size,
          p = page_i
        )
      )

      if(!is.null(tmp_funds$fund) && length(tmp_funds$fund) > 0) {
        all_funds <- c(all_funds, tmp_funds$fund)
      }
    }
  }

  fund_ids <- unique(
    vapply(
      all_funds,
      FUN.VALUE = character(1),
      FUN = function(fund_rec) {
        if(!is.null(fund_rec$id) & length(fund_rec$id) == 1 & is.character(fund_rec$id)) {
          fund_rec$id
        } else {
          NA_character_
        }
      }
    )
  )

  fund_ids <- fund_ids[!is.na(fund_ids) & fund_ids != ""]

  if(length(fund_ids) == 0) {
    stop("No fund IDs were returned for this 'funder' search term")
  }

  project_ids_from_funds <- unique(unlist(lapply(all_funds, extract_project_ids)))

  if(length(project_ids_from_funds) == 0) {
    stop("No linked projects were found for funds matching this 'funder' search term")
  }

  #determine project pages---------------------------------------

  projects_url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['project']]}")

  project_page_1 <- get_raw_response(
    .url = projects_url,
    .query_settings = list(
      s = size,
      p = 1
    )
  )

  if(missing(page_nums)) {
    pages_to_scan <- seq_len(project_page_1$totalPages)
  } else {
    pages_to_scan <- unique(as.integer(page_nums))
  }

  #filter projects by FUND links---------------------------------------

  matched_projects <- list()

  page_to_response <- function(page_i) {
    if(page_i == 1) {
      project_page_1
    } else {
      get_raw_response(
        .url = projects_url,
        .query_settings = list(
          s = size,
          p = page_i
        )
      )
    }
  }

  for(page_i in pages_to_scan) {

    page_result <- page_to_response(page_i)

    if(is.null(page_result$project) || length(page_result$project) == 0) next

    page_projects <- page_result$project

    keep_project <- vapply(
      page_projects,
      FUN.VALUE = logical(1),
      FUN = function(project_rec) {
        project_id_match <- !is.null(project_rec$id) & project_rec$id %in% project_ids_from_funds
        linked_funds <- extract_fund_ids(project_rec)
        project_id_match | (length(linked_funds) > 0 & any(linked_funds %in% fund_ids))
      }
    )

    if(any(keep_project)) {
      matched_projects[[length(matched_projects) + 1]] <- projects_to_df(page_projects[keep_project])
    }
  }

  projects_df <- if(length(matched_projects) > 0) {
    dplyr::bind_rows(matched_projects)
  } else {
    data.frame()
  }

  if("id" %in% names(projects_df)) {
    projects_df <- projects_df[!duplicated(projects_df$id), ]
  }

  if(df_only == TRUE) {
    as.data.frame(projects_df)
  } else {
    list(
      "project" = as.data.frame(projects_df),
      "fund_ids" = fund_ids,
      "project_ids" = project_ids_from_funds,
      "project_pages_scanned" = pages_to_scan
    )
  }
}
