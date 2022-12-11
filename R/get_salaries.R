#' Helperfunction: lookup type for temporal resolution of salary table
#'
#' @return list with
#' @keywords internal
#' @noMd
#' @noRd
lookup_type <-  function() {
  list(
    monthly = "1",
    annual = "12",
    quarterly = "3",
    weekly = "w",
    weekdays = "v",
    daily = "t"
  )
}

#' Helper function: get temporal resolution
#'
#' @param type type
#'
#' @return name of temporal resolution from lookup
#' @keywords internal
#' @noMd
#' @noRd
get_temporal_resolution <- function (type) {
  lookup <- lookup_type()
  names(lookup)[unlist(lookup) == as.character(type)]
}

#' Get Salary
#'
#' @param year year of tariff (default: 2009)
#' @param union_rate default: "tv-l"
#' @param area default: "west"
#' @param type 1: Monatswerte, 12: Jahreswerte, 3: Quartalswerte, "w": Wochenwerte,
#' "t": Tageswerte, "v": werktagswerte (default: 1)
#' @return tibble in list format with salaries
#' @export
#' @examples
#' get_salary()
#' @importFrom rlang .data
#' @importFrom rvest read_html html_element html_text html_table
#' @importFrom stringr str_extract_all str_detect str_remove
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
#'
get_salary <- function(
    year = 2009,
    union_rate = "tv-l",
    area = "west",
    type = 1
)
{
  url <- sprintf(
    "https://oeffentlicher-dienst.info/c/t/rechner/%s/%s/%d?id=%s-%d&matrix=%s",
    union_rate,
    area,
    as.integer(year),
    union_rate,
    as.integer(year),
    as.character(type)
  )

  date_from_to <- rvest::read_html(url) %>%
    rvest::html_element("div#incenter") %>%
    rvest::html_element("div#links") %>%
    rvest::html_text() %>%
    stringr::str_extract_all("[0-3][0-9]\\.[0-1][0-9]\\.[0-9]{4}") %>%
    unlist() %>%
    as.Date(format = "%d.%m.%Y")

  stopifnot(length(date_from_to) > 0L)

  suppressWarnings(
    tv_tbl <- rvest::read_html(url) %>%
      rvest::html_element("div#incenter") %>%
      rvest::html_table()
  )

  title <- names(tv_tbl)[1L]

  unit <- as.character(tv_tbl[1L, 1L])

  pay_step <- paste0("step_", as.character(tv_tbl[1L, -1L]))

  tv_tbl <- tv_tbl[-1L, ]

  names(tv_tbl) <- c("pay_group", pay_step)

  tv_tbl %>%
    dplyr::filter(
      stringr::str_detect(.data$pay_group, "[A-Z]\\s?[0-9]+")
    ) %>%
    dplyr::mutate(
      pay_group = stringr::str_replace(.data$pay_group, "\u00A0", " ")
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("step_"),
      names_to = "step",
      values_to = "salary"
    ) %>%
    dplyr::mutate(
      union_rate = union_rate,
      step = as.integer(stringr::str_remove(.data$step, "step_")),
      unit = unit,
      title = title,
      area = tolower(area),
      year = stringr::str_extract(.data$title, "[0-9]{4}"),
      date_from = date_from_to[1L],
      date_to = date_from_to[2L]
    )
}

#' Get Salaries
#' @description wrapper around [get_salary()] for multiple years
#' @param years years of tariffs to try to download (default: 2008:2021)
#' @inheritParams get_salary
#' @param dbg print debug messages (default: TRUE)
#' @return tibble with salaries for multiple years
#' @export
#' @examples
#' salaries <- get_salaries(years = 2008:2021)
#' salaries
#' @importFrom kwb.utils catAndRun
#' @importFrom dplyr bind_rows
get_salaries <- function(
    years = 2008:2021,
    union_rate = "tv-l",
    area = "west",
    type = 1,
    dbg = TRUE
)
{
  salaries <- lapply(years, function(year) {
    kwb.utils::catAndRun(
      messageText = sprintf(
        "Getting '%s' salary table for '%s' ('%s') for year %d",
        get_temporal_resolution(type),
        union_rate,
        area,
        as.integer(year)
      ),
      expr = try(get_salary(
        year = year,
        union_rate = union_rate,
        area = area,
        type = type
      )),
      dbg = dbg
    )
  })

  salaries[!sapply(salaries, kwb.utils::isTryError)] %>%
    dplyr::bind_rows()
}
