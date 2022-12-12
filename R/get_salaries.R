#' Helperfunction: lookup type for temporal resolution of salary table
#'
#' @param resolution one of "monthly", "annual", "quarterly", "weekly",
#'   "weekdays", "daily"
#' @return type string as required as part of URL
#' @keywords internal
#' @noMd
#' @noRd
resolution_to_type <- function(resolution)
{
  kwb.utils::selectElements(elements = resolution, list(
    monthly = "1",
    annual = "12",
    quarterly = "3",
    weekly = "w",
    weekdays = "v",
    daily = "t"
  ))
}

#' Get Salary
#'
#' @param year year of tariff (default: 2009)
#' @param union_rate default: "tv-l"
#' @param area default: "west"
#' @param resolution "monthly": Monatswerte, "annual": Jahreswerte, "quarterly":
#'   Quartalswerte, "weekly": Wochenwerte, "weekdays": werktagswerte, "daily":
#'   Tageswerte. Default: "monthly"
#' @return tibble in list format with salaries
#' @export
#' @examples
#' get_salary()
#' @importFrom rlang .data
#' @importFrom kwb.utils revertListAssignments
#' @importFrom rvest read_html html_element html_text html_table
#' @importFrom stringr str_extract_all str_detect str_remove
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
#'
get_salary <- function(
    year = 2009L,
    union_rate = "tv-l",
    area = "west",
    resolution = "monthly"
)
{
  url <- sprintf(
    "https://oeffentlicher-dienst.info/c/t/rechner/%s/%s/%d?id=%s-%d&matrix=%s",
    union_rate,
    area,
    as.integer(year),
    union_rate,
    as.integer(year),
    resolution_to_type(resolution)
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
      values_to = paste0("salary_", resolution)
    ) %>%
    dplyr::mutate(
      union_rate = union_rate,
      step = as.integer(stringr::str_remove(.data$step, "step_")),
      unit = unit,
      title = title,
      area = tolower(area),
      year = as.integer(stringr::str_extract(.data$title, "[0-9]{4}")),
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
#' @importFrom kwb.utils catAndRun moveColumnsToFront
#' @importFrom dplyr bind_rows
get_salaries <- function(
    years = 2008:2021,
    union_rate = "tv-l",
    area = "west",
    resolution = "monthly",
    dbg = TRUE
)
{
  salaries <- lapply(years, function(year) {
    kwb.utils::catAndRun(
      messageText = sprintf(
        "Getting '%s' salary table for '%s' ('%s') for year %d",
        resolution,
        union_rate,
        area,
        as.integer(year)
      ),
      expr = try(get_salary(
        year = year,
        union_rate = union_rate,
        area = area,
        resolution = resolution
      )),
      dbg = dbg
    )
  })

  salaries[!sapply(salaries, kwb.utils::isTryError)] %>%
    dplyr::bind_rows() %>%
    kwb.utils::moveColumnsToFront(c(
      "union_rate", "area", "title", "year", "date_from", "date_to",
      "pay_group", "step"
    ))
}
