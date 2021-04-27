#' Check Actions Consistency
#'
#' Check the history of actions by station or sonde for consistency.
#'
#' @param actions A vector of action types to check. Default is to
#'  only check field actions, i.e., "Removed" and "Installed".
#' @param from The starting date to check actions.
#' @param to The ending date to check actions.
#' @param group.by The grouping to use. Can be either "station" or
#'   "sonde".
#' @inheritParams wqpr::wqp_use_program
#' @inheritParams wqpr::wqp_use_database
#' @return A table of actions. The field "consistent" lists whether
#'   given action is consistent with the previous action (`TRUE`)
#'   or not (`FALSE`).
#'
#' @importFrom wqpr wqp_action_details wqp_event_details wqp_tz
#' @importFrom dplyr left_join select filter arrange group_by ungroup
#'   mutate lag case_when between
#' @importFrom rlang .data .env
#' @export
check_actions = function(actions = c("Removed", "Installed"), from, to,
  group.by = c("station", "sonde"), program, database) {
  event.field = "event_id"
  time.field = "arrival_time"
  station.field = "station_name"
  action.field = "action_name"
  sonde.field = "sonde_name"
  location.field = "location_name"
  group.by = match.arg(group.by, c("station", "sonde"))
  from = as.Date(from)
  to = as.Date(to) + 1L
  if (is.na(from) || is.na(to)) {
    stop("could not parse date format.")
  }
  # combine action and event data
  action.data = wqp_action_details(program, database)
  event.data = wqp_event_details(program, database)
  join.data = left_join(
    select(action.data, .env$event.field, .env$sonde.field,
      .env$location.field, .env$action.field),
    select(event.data, .env$event.field, .env$time.field,
      .env$station.field),
    by = event.field
  )
  join.data = filter(join.data, .data[[action.field]] %in% .env$actions)
  join.data[action.field] = factor(join.data[[action.field]], actions)
  # group by sonde or station
  if (group.by == "station") {
    join.data = arrange(join.data, .data[[station.field]],
      .data[[time.field]], .data[[action.field]], .data[[sonde.field]])
    join.data = group_by(join.data, .data[[station.field]],
      .data[[location.field]])
    join.data = ungroup(mutate(join.data,
      previous_action = lag(.data[[action.field]]),
      previous_station = lag(.data[[station.field]]),
      previous_sonde = lag(.data[[sonde.field]]),
      previous_location = lag(.data[[location.field]])
    ))
  } else {
    join.data = arrange(join.data, .data[[sonde.field]],
      .data[[time.field]], .data[[action.field]],
      .data[[station.field]])
    join.data = group_by(join.data, .data[[sonde.field]],
      .data[[location.field]])
    join.data = ungroup(mutate(join.data,
      previous_action = lag(.data[[action.field]]),
      previous_station = lag(.data[[station.field]]),
      previous_sonde = lag(.data[[sonde.field]]),
      previous_location = lag(.data[[location.field]])
    ))
  }
  # check for action consistency
  join.data = mutate(join.data,
      consistent = case_when(
        .data$action_name == .env$actions[2] &
          is.na(.data$previous_action) ~ NA_character_,
        .data$action_name == .env$actions[2]  &
        .data$previous_action == .env$actions[1] ~ "PASS",
        action_name == "Removed" & previous_action == "Installed" &
          sonde_name == previous_sonde &
          location_name == previous_location ~ "PASS",
        TRUE ~ "FAIL"
      )
  )
  join.data = mutate(join.data,
    consistent = case_when(
      is.na(.data$consistent) ~ NA,
      .data$consistent == "PASS" ~ TRUE,
      .data$consistent == "FAIL" ~ FALSE
    )
  )
  # filter by date range
  join.data = filter(join.data, between(as.Date(.data[[time.field]]),
    from, to))
  # sort and return
  if (group.by == "sonde") {
    select(join.data,
      .data[[event.field]], .data[[time.field]],
      .data[[sonde.field]], .data[[action.field]],
      .data[[location.field]], .data[[station.field]],
      .data$consistent)
  } else {
    select(join.data,
      .data[[event.field]], .data[[time.field]],
      .data[[station.field]], .data[[action.field]],
      .data[[location.field]], .data[[sonde.field]],
      .data$consistent)
  }
}

#' Check Visual QAQC Progress
#'
#' Count the visual QAQC flags for the specified stations and analytes
#' over a given period.
#'
#' @param cdec.code A vector of station CDEC codes. Regular expressions
#' Can also be used.
#' @param analyte.name A vector of analyte names. Regular expressions
#'   Can also be used.
#' @param from The starting date to query QAQC flags.
#' @param to The ending date to query QAQC flags.
#' @inheritParams wqpr::wqp_use_program
#' @inheritParams wqpr::wqp_use_database
#'
#' @importFrom wqpr wqp_result_details wqp_result_data
#' @importFrom dplyr filter mutate select count all_of matches across
#' @importFrom tidyr unnest pivot_wider pivot_longer
#' @importFrom stringr str_c str_detect
#' @importFrom glue glue
#' @importFrom rlang .data .env
#' @export
check_qaqc = function(cdec.code, analyte.name, from, to,
  program, database) {
  # fields
  station.var = "cdec_code"
  analyte.var = "analyte_name"
  reading.type.var = "reading_type_name"
  result.id.var = "result_id"
  version.var = "version"
  qaqc.var = "qaqc_flag_id"
  from = as.Date(from)
  to = as.Date(to)
  if (is.na(from) || is.na(to)) {
    stop("could not parse date format.")
  }
  # vector to regex
  cdec.code = str_c(glue("({cdec.code})"), collapse = "|")
  analyte.name = str_c(glue("({analyte.name})"), collapse = "|")
  # get data
  results = filter(wqp_result_details(program, database),
    .data[[reading.type.var]] == "Time Series",
    str_detect(.data[[station.var]], .env$cdec.code),
    str_detect(.data[[analyte.var]], .env$analyte.name))
  results = mutate(results,
    data = wqp_result_data(.data[[result.id.var]],
      start.date = .env$from, end.date = .env$to,
      version = .data[[version.var]], program = program,
      database = database))
  results = select(results,
    -all_of(c(.env$result.id.var, .env$version.var)))
  results = unnest(results, cols = "data")
  # summarize
  results.count = count(results, .data[[station.var]],
    .data[[analyte.var]], .data[[qaqc.var]],
    name = "count")
  # workaround for issues with nesting and complete
  # see https://github.com/tidyverse/tidyr/issues/971
   results.count = pivot_wider(results.count, values_fill = 0L,
     names_from = .data[[qaqc.var]], values_from = .data$count)
   results.count = pivot_longer(results.count, matches("^[A-Z]{1}$"),
     names_to = qaqc.var, values_to = "count")
  # return only "U" flags
  pivot_wider(
    select(filter(results.count, .data[[qaqc.var]] == "U"),
      Station = .data[[station.var]], .data[[analyte.var]],
      .data$count),
    names_from = .data[[analyte.var]],
    values_from = .data$count
  )
}
