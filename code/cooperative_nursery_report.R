## Cooperative Nursery Report -- T3/Wheat section
##
## Opens the nursery registry (REGISTRY_FILE in .Renviron), finds the nurseries
## that are held on T3/Wheat (column D, header "Wheat"), and for each one looks
## up the T3/Wheat trials whose trial name contains the nursery's "T3 Acronym"
## (column P).  Each nursery becomes a section of the Cooperative Nursery
## Report: one line per year, running consecutively from the first year of
## trials to the last, with a comma separated list of that year's locations.
## A year with no trials gets a line naming the year and nothing else.

library(tidyverse)

here::i_am("code/cooperative_nursery_report.R")

## ---------------------------------------------------------------------------
## Configuration
## ---------------------------------------------------------------------------

T3_WHEAT_BASE  <- "https://wheat.triticeaetoolbox.org"
STUDY_CACHE    <- here::here("data", "t3_wheat_studies.rds")
REPORT_FILE    <- here::here("output", "Cooperative_Nursery_Report.md")

## Column positions given in the registry spec, used only if the header text
## cannot be matched.
COL_WHEAT   <- 4L   # column D
COL_ACRONYM <- 16L  # column P

## Values in the "Wheat" column that mean "this nursery is NOT on T3/Wheat".
NOT_ON_T3 <- c("", "no", "n", "false", "f", "0", "-", "na", "none")

## Separator between locations on a year line.  Many T3 location names are
## themselves "Town, ST", so "; " reads far better than the comma asked for.
LOCATION_SEP <- "; "

## ---------------------------------------------------------------------------
## Environment
## ---------------------------------------------------------------------------

#' Read .Renviron from the project root if the variables are not already set.
#' R only picks up a project .Renviron when it starts in the project directory,
#' so this makes the script safe to source from anywhere.
ensure_renviron <- function(vars = c("REGISTRY_FILE", "T3_USERNAME", "WHEAT_PASSWORD")) {
  if (all(nzchar(Sys.getenv(vars)))) return(invisible(TRUE))
  renviron <- here::here(".Renviron")
  if (file.exists(renviron)) readRenviron(renviron)
  invisible(TRUE)
}

#' Expand ~ and check the registry file is really there.
registry_path <- function() {
  raw <- Sys.getenv("REGISTRY_FILE")
  if (!nzchar(raw)) {
    stop("REGISTRY_FILE is not set -- add it to .Renviron", call. = FALSE)
  }
  ## Strip any quoting that survived the .Renviron parse.
  raw <- stringr::str_remove_all(raw, '^"|"$')
  path <- path.expand(raw)
  if (!file.exists(path)) {
    stop("Registry file not found:\n  ", path,
         "\nCheck the REGISTRY_FILE entry in .Renviron.", call. = FALSE)
  }
  path
}

## ---------------------------------------------------------------------------
## The registry
## ---------------------------------------------------------------------------

#' Find a column by its header text, falling back to a fixed position.
pick_column <- function(df, headers, position) {
  clean <- stringr::str_squish(names(df))
  hit <- which(tolower(clean) %in% tolower(headers))
  if (length(hit) >= 1) return(names(df)[hit[1]])
  if (position > ncol(df)) {
    stop("Cannot find a column named ", paste(headers, collapse = "/"),
         " and the registry has fewer than ", position, " columns.", call. = FALSE)
  }
  names(df)[position]
}

read_registry <- function(path = registry_path(), sheet = 1) {
  raw <- readxl::read_excel(path, sheet = sheet, col_types = "text")

  wheat_col   <- pick_column(raw, "Wheat", COL_WHEAT)
  acronym_col <- pick_column(raw, c("T3 Acronym", "T3Acronym", "Acronym"), COL_ACRONYM)
  name_col    <- pick_column(raw, c("Nursery", "Nursery Name", "Name"), 1L)

  raw |>
    transmute(
      nursery = stringr::str_squish(.data[[name_col]]),
      acronym = stringr::str_squish(.data[[acronym_col]]),
      wheat   = stringr::str_squish(.data[[wheat_col]])
    ) |>
    mutate(
      on_wheat = !is.na(wheat) & !(tolower(replace_na(wheat, "")) %in% NOT_ON_T3)
    )
}

#' The nurseries we will look up on T3/Wheat: flagged in column D and carrying
#' a usable acronym in column P.  A few acronyms are shared by two registry
#' rows, so collapse on the acronym and keep every nursery name against it
#' rather than dropping a row.
wheat_nurseries <- function(registry = read_registry()) {
  registry |>
    filter(on_wheat, !is.na(acronym), acronym != "") |>
    summarise(nursery = paste(unique(nursery), collapse = " / "), .by = acronym) |>
    arrange(acronym) |>
    select(nursery, acronym)
}

## ---------------------------------------------------------------------------
## T3/Wheat (BrAPI v2)
## ---------------------------------------------------------------------------

#' Exchange the T3 credentials for a BrAPI bearer token.  Public trial data is
#' readable without one, so a failure here is a warning rather than an error.
t3_token <- function(base_url = T3_WHEAT_BASE,
                     username = Sys.getenv("T3_USERNAME"),
                     password = Sys.getenv("WHEAT_PASSWORD")) {
  if (!nzchar(username) || !nzchar(password)) {
    warning("T3_USERNAME / WHEAT_PASSWORD not set -- querying T3/Wheat anonymously",
            call. = FALSE)
    return("")
  }
  resp <- httr2::request(base_url) |>
    httr2::req_url_path_append("brapi", "v2", "token") |>
    httr2::req_body_json(list(
      username   = username,
      password   = password,
      grant_type = "password"
    )) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) >= 300) {
    warning("T3/Wheat login failed (HTTP ", httr2::resp_status(resp),
            ") -- continuing anonymously", call. = FALSE)
    return("")
  }

  body <- httr2::resp_body_json(resp)
  token <- body$access_token %||% body$metadata$access_token %||% ""
  if (!nzchar(token)) {
    warning("T3/Wheat login returned no access token -- continuing anonymously",
            call. = FALSE)
  }
  as.character(token)
}

#' One page of /brapi/v2/studies as a tibble.
studies_page <- function(base_url, token, page, page_size) {
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("brapi", "v2", "studies") |>
    httr2::req_url_query(page = page, pageSize = page_size) |>
    httr2::req_retry(max_tries = 3)
  if (nzchar(token)) req <- httr2::req_auth_bearer_token(req, token)

  body <- req |> httr2::req_perform() |> httr2::resp_body_json()
  data <- body$result$data %||% list()

  tibble::tibble(
    study_name = purrr::map_chr(data, \(x) as.character(x$studyName %||% NA_character_)),
    location   = purrr::map_chr(data, \(x) as.character(x$locationName %||% NA_character_)),
    season     = purrr::map_chr(data, \(x) {
      s <- unlist(x$seasons)
      if (length(s) == 0) NA_character_ else paste(s, collapse = "|")
    }),
    start_date = purrr::map_chr(data, \(x) as.character(x$startDate %||% NA_character_))
  ) |>
    mutate(total_pages = body$metadata$pagination$totalPages %||% NA_integer_)
}

#' Every trial on T3/Wheat, cached to disk because the acronym match is a
#' substring test that the BrAPI filters cannot express.
t3_studies <- function(base_url = T3_WHEAT_BASE,
                       cache = STUDY_CACHE,
                       refresh = FALSE,
                       page_size = 1000L) {
  if (!refresh && file.exists(cache)) return(readr::read_rds(cache))

  token <- t3_token(base_url)
  page <- 0L
  pages <- list()
  repeat {
    this <- studies_page(base_url, token, page, page_size)
    if (nrow(this) == 0) break
    pages[[length(pages) + 1]] <- select(this, -total_pages)
    total <- this$total_pages[1]
    page <- page + 1L
    if (!is.na(total) && page >= total) break
  }

  studies <- pages |>
    bind_rows() |>
    mutate(year = study_year(season, start_date, study_name)) |>
    filter(!is.na(study_name))

  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  readr::write_rds(studies, cache)
  studies
}

#' Year of a trial: the season if BrAPI gives one, otherwise the start date,
#' otherwise a four digit number in the trial name.
study_year <- function(season, start_date, study_name) {
  from_season <- stringr::str_extract(season, "\\d{4}")
  from_date   <- stringr::str_extract(start_date, "\\d{4}")
  from_name   <- stringr::str_extract(study_name, "(19|20)\\d{2}")
  coalesce(from_season, from_date, from_name) |> as.integer()
}

## ---------------------------------------------------------------------------
## Report sections
## ---------------------------------------------------------------------------

#' The trials on T3/Wheat whose name contains this acronym.
trials_for_acronym <- function(acronym, studies) {
  studies |>
    filter(stringr::str_detect(study_name,
                               stringr::fixed(acronym, ignore_case = TRUE)))
}

#' One line per year from the first to the last year of trials.  Years with
#' trials carry a comma separated list of locations; years without carry only
#' the year.
year_lines <- function(trials) {
  trials <- filter(trials, !is.na(year))
  if (nrow(trials) == 0) return(character(0))

  by_year <- trials |>
    mutate(location = if_else(is.na(location) | location == "",
                              "Unknown location", location)) |>
    distinct(year, location) |>
    arrange(year, location) |>
    summarise(locations = paste(location, collapse = LOCATION_SEP), .by = year)

  tibble::tibble(year = seq(min(by_year$year), max(by_year$year))) |>
    left_join(by_year, by = "year") |>
    mutate(line = if_else(is.na(locations),
                          paste0("- ", year),
                          paste0("- ", year, ": ", locations))) |>
    pull(line)
}

#' The markdown section for one nursery, or NULL when T3/Wheat has no trials
#' carrying its acronym.
nursery_section <- function(nursery, acronym, studies) {
  trials <- trials_for_acronym(acronym, studies)
  lines <- year_lines(trials)
  if (length(lines) == 0) return(NULL)

  heading <- if (is.na(nursery) || nursery == "") acronym else
    paste0(nursery, " (", acronym, ")")

  c(paste("##", heading), "", lines, "")
}

#' Build the T3/Wheat part of the Cooperative Nursery Report and write it out.
build_report <- function(report_file = REPORT_FILE, refresh = FALSE) {
  ensure_renviron()

  nurseries <- wheat_nurseries()
  message("Registry: ", nrow(nurseries), " nurseries flagged on T3/Wheat")

  studies <- t3_studies(refresh = refresh)
  message("T3/Wheat: ", nrow(studies), " trials available")

  sections <- purrr::pmap(
    list(nurseries$nursery, nurseries$acronym),
    \(nursery, acronym) nursery_section(nursery, acronym, studies)
  )
  names(sections) <- nurseries$acronym
  unmatched <- names(sections)[purrr::map_lgl(sections, is.null)]
  sections <- purrr::compact(sections)

  message("Report: ", length(sections), " nurseries have matching trials")
  if (length(unmatched) > 0) {
    message("No T3/Wheat trial names contain these acronyms, so they get no section:\n  ",
            paste(unmatched, collapse = "\n  "))
  }

  body <- c(
    "# Cooperative Nursery Report",
    "",
    paste0("T3/Wheat trials, generated ", format(Sys.Date())),
    "",
    unlist(sections)
  )

  dir.create(dirname(report_file), showWarnings = FALSE, recursive = TRUE)
  readr::write_lines(body, report_file)
  message("Wrote ", report_file)

  invisible(body)
}

if (!interactive()) build_report()
