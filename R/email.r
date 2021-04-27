#' Email Form Processing Results
#'
#' Send email containing form processing report.
#'
#' @param results An outcome tibble, i.e. output of `process_form()`.
#' @param template.path The YAML template to use.
#' @param log File to log results to.
#' @return `TRUE` if SMTP server returned successful status code,
#'   `FALSE` otherwise.
#'
#' @importFrom rlang .data
#' @importFrom dplyr arrange mutate case_when if_else
#' @importFrom kableExtra kable column_spec
#' @importFrom glue glue
#' @importFrom yaml read_yaml
#' @importFrom emayili envelope to from subject html server
#'
#' @keywords internal
mail_forms = function(results, template.path, log = "") {
  if (!file.exists(template.path)) {
    stop("Could not find template file ", template.path)
  }
  # format results table
  table.format = "cellpadding=\"7\""
  results.table = arrange(results, .data$outcome, .data$form)
  results.table = mutate(results.table,
    outcome = sprintf("<font style=\"color:%s\">%s</font>",
      case_when(
        .data$outcome == "ARCHIVED" ~ "green",
        .data$outcome == "INCOMPLETE" ~ "red",
        .data$outcome == "INVALID" ~ "red",
        .data$outcome == "ERRORED" ~ "purple",
        .data$outcome == "FAILED" ~ "purple",
        .data$outcome == "SKIPPED" ~ "gray",
        TRUE ~ "black"
      ),
      .data$outcome
    ),
    moved = sprintf("<font style=\"color:%s\">%s</font>",
      if_else(.data$moved, "green", "red", "gray"), .data$moved),
    logged = sprintf("<font style=\"color:%s\">%s</font>",
      if_else(.data$logged, "green", "red", "gray"), .data$logged)
  )
  results.table = kable(results.table, format = "html",
    escape = FALSE, table.attr = table.format)
  if (nrow(results) > 0L) {
  results.table = column_spec(results.table, 1:ncol(results),
    border_left = FALSE, border_right = TRUE)
  }
  # construct email
  template = read_yaml(template.path)
  template$Subject = glue(template$Subject)
  template$Body = glue(template$Body)
  msg = envelope()
  msg = from(msg, template$Sender)
  msg = to(msg, template$Recipients)
  msg = subject(msg, template$Subject)
  msg = html(msg, template$Body)
  # send email
  smtp = server(template$Host, insecure = TRUE, reuse = FALSE)
  response = smtp(msg, verbose = FALSE)
  if (response$status_code != 250L) {
    warning(glue("Email may not have succeeded, SMTP server ",
        "returned status code {response$status_code}"))
  }
  # return value
  isTRUE(response$status_code == 250L)
}

#' Email QAQC Review
#'
#' Send email containing QAQC review report.
#'
#' @param results An outcome tibble, i.e. output of `check_qaqc()`.
#' @param template.path The YAML template to use.
#' @param log File to log results to.
#' @return `TRUE` if SMTP server returned successful status code,
#'   `FALSE` otherwise.
#'
#' @importFrom rlang .data
#' @importFrom dplyr arrange mutate case_when if_else
#' @importFrom kableExtra kable column_spec
#' @importFrom glue glue
#' @importFrom yaml read_yaml
#' @importFrom emayili envelope to from subject html server
#'
#' @keywords internal
mail_qaqc = function(results, from.date, to.date, template.path,
  log = "") {
  if (!file.exists(template.path)) {
    stop("Could not find template file ", template.path)
  }
  # table fields
  format.fields = setdiff(names(results), "Station")
  # formatting
  table.format = "cellpadding=\"7\""
  qaqc.table = mutate(results, across(all_of(format.fields),
    ~ sprintf("<font style=\"color:%s\">%s</font>",
      case_when(
        is.na(.x) ~ "gray",
        .x > 2500L ~ "red",
        TRUE ~ "green"
      ),
      .x
    )))
  qaqc.table = kable(qaqc.table, format = "html",
    escape = FALSE, align = c("l", rep("c", nrow(results) - 1)),
    table.attr = table.format)
  qaqc.table = column_spec(qaqc.table, 1:ncol(results),
    border_left = FALSE, border_right = TRUE)
  # construct email
  template = read_yaml(template.path)
  template$Subject = glue(template$Subject)
  template$Body = glue(template$Body)
  msg = envelope()
  msg = from(msg, template$Sender)
  msg = to(msg, template$Recipients)
  msg = subject(msg, template$Subject)
  msg = html(msg, template$Body)
  # send email
  smtp = server(template$Host, insecure = TRUE, reuse = FALSE)
  response = smtp(msg, verbose = FALSE)
  if (response$status_code != 250L) {
    warning(glue("Email may not have succeeded, SMTP server ",
        "returned status code {response$status_code}"))
  }
  # return value
  isTRUE(response$status_code == 250L)
}

#' Email Action Review
#'
#' Send email containing action review report.
#'
#' @param station.results An outcome tibble, i.e. output of
#'  `check_actions()`.
#' @param template.path The YAML template to use.
#' @param log File to log results to.
#' @return `TRUE` if SMTP server returned successful status code,
#'   `FALSE` otherwise.
#'
#' @importFrom rlang .data
#' @importFrom kableExtra kable column_spec
#' @importFrom glue glue
#' @importFrom yaml read_yaml
#' @importFrom emayili envelope to from subject html server
#'
#' @keywords internal
mail_actions = function(station.results, sonde.results,
  from.date, to.date, template.path, log = "") {
  if (!file.exists(template.path)) {
    stop("Could not find template file ", template.path)
  }
  # format results table
  table.format = "cellpadding=\"7\""
  # station results
  station.table = mutate(station.results,
    consistent = sprintf("<font style=\"color:%s\">%s</font>",
      case_when(
        is.na(.data$consistent) ~ "gray",
        !(.data$consistent) ~ "red",
        .data$consistent ~ "green"
      ),
      .data$consistent
    )
  )
  station.table = kable(station.table, format = "html",
    escape = FALSE, table.attr = table.format)
  if (nrow(station.results) > 0L) {
  station.table = column_spec(station.table, 1:ncol(station.results),
    border_left = FALSE, border_right = TRUE)
  }
  # sonde results
  sonde.table = mutate(sonde.results,
    consistent = sprintf("<font style=\"color:%s\">%s</font>",
      case_when(
        is.na(.data$consistent) ~ "gray",
        !(.data$consistent) ~ "red",
        .data$consistent ~ "green"
      ),
      .data$consistent
    )
  )
  sonde.table = kable(sonde.table, format = "html",
    escape = FALSE, table.attr = table.format)
  if (nrow(sonde.results) > 0L) {
  sonde.table = column_spec(sonde.table, 1:ncol(sonde.results),
    border_left = FALSE, border_right = TRUE)
  }
  # construct email
  template = read_yaml(template.path)
  template$Subject = glue(template$Subject)
  template$Body = glue(template$Body)
  msg = envelope()
  msg = from(msg, template$Sender)
  msg = to(msg, template$Recipients)
  msg = subject(msg, template$Subject)
  msg = html(msg, template$Body)
  # send email
  smtp = server(template$Host, insecure = TRUE, reuse = FALSE)
  response = smtp(msg, verbose = FALSE)
  if (response$status_code != 250L) {
    warning(glue("Email may not have succeeded, SMTP server ",
        "returned status code {response$status_code}"))
  }
  # return value
  isTRUE(response$status_code == 250L)
}
