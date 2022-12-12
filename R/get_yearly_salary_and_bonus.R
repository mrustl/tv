# get_yearly_salary_and_bonus --------------------------------------------------

#' Get Yearly Salary and Bonus
#'
#' @inheritParams get_salary
#' @importFrom kwb.utils selectColumns
#' @importFrom tibble tibble
#' @data optional. Data frame with columns "union_rate", "area", "year",
#'  "pay_group", "step". If given, the function is called for each row of this
#'  data frame
#' @export
#'
get_yearly_salary_and_bonus <- function(
    union_rate,
    area, year,
    pay_group,
    step,
    data = NULL
)
{
  # If data is given, call this function for each row in data
  if (!is.null(data)) {

    # Select columns that are required by get_yearly_salary_and_bonus()
    data <- kwb.utils::selectColumns(data, c(
      "union_rate", "area", "year", "pay_group", "step"
    ))

    # Call get_yearly_salary_and_bonus() for each row in tvl_salaries_monthly
    result_rows <- lapply(seq_len(nrow(data)), function(i) {
      do.call(get_yearly_salary_and_bonus, as.list(data[i, ]))
    })

    return(dplyr::bind_rows(result_rows))
  }

  url <-   sprintf(
    "https://oeffentlicher-dienst.info/c/t/rechner/%s/%s/%d?id=%s-%d&g=%s&s=%s",
    union_rate,
    area,
    year,
    union_rate,
    year,
    kwb.utils::multiSubstitute(pay_group, list(
      " " = "_",
      "Ü" = "%DC"
    )),
    step
  )

  print(url)

  xpath_1 <- '//*[@id="incenter"]/table/tr[4]/td[1]/pre'
  xpath_2 <- '//*[@id="incenter"]/table/tr[3]/td[1]/pre'

  html <- rvest::read_html(url)

  base_args <- list(
    union_rate = union_rate,
    area = area,
    year = year,
    pay_group = pay_group,
    step = step
  )

  result <- extract_yearly_salary_and_bonus(html, base_args, xpath = xpath_1)

  if (nrow(result) == 0L) {
    message("Could not extract yearly salary and bonus. Returning NULL")
    return(NULL)
  }

  if (!is.na(kwb.utils::selectColumns(result, "salary_yearly"))) {
    return(result)
  }

  # Try alternative path
  extract_yearly_salary_and_bonus(html, base_args, xpath = xpath_2)
}

# extract_yearly_salary_and_bonus ----------------------------------------------
extract_yearly_salary_and_bonus <- function(html, base_args, xpath)
{
  raw_rows <- html %>%
    rvest::html_elements(xpath = xpath) %>%
    rvest::html_text()

  rows <- strsplit(raw_rows, split = "\n")[[1L]]

  keywords <- c(
    salary_yearly = "Grundgehalt",
    extra_yearly = "Jahressonderzahlung"
  )

  patterns <- paste0(unname(keywords), ":\\s+(\\d+\\.\\d+)")

  values <- sapply(patterns, USE.NAMES = FALSE, FUN = function(pattern) {
    pattern %>%
      kwb.utils::extractSubstring(rows, 1L) %>%
      kwb.utils::removeEmpty() %>%
      as.numeric()
  })

  value_args <- stats::setNames(as.list(values), names(keywords))

  do.call(tibble::tibble, c(base_args, value_args))
}
