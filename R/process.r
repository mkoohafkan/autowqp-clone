
#' Auto Try-Catch
#'
#' Streamlined version of tryCatch
#'
#' @inherit base::try
#' @param logfile File to append messages to.
#' @return `FALSE` if error encountered, `TRUE` otherwise.
#'
#' @keywords internal
auto_try = function(expr, logfile) {
  result = try(expr, silent = TRUE)
  if ("try-error" %in% class(result)) {
    append_log(attr(result, "condition")$message, file = logfile)
    FALSE
  } else {
    TRUE
  }
}

#' Outcome Tibble
#'
#' Formatted `tibble` of processing results.
#'
#' @param outcome Character value specifying outcome.
#' @param moved `TRUE` if file was moved, `FALSE` file move failed,
#'   `NA` if move was not attempted.
#' @param logged `TRUE` if log was created, `FALSE` if logging
#'   failed , `NA` if logging was not attempted.
#' @return A tibble.
#'
#' @importFrom tibble tibble
#'
#' @keywords internal
outcome_tibble = function(outcome, moved = NA, logged = NA) {
  tibble(outcome = outcome, moved = moved, logged = logged)
}

#' Process Form
#'
#' Process a form.
#'
#' @param form The form to process.
#' @param overwrite If `TRUE`, attempt to overwrite existing values
#'   in WQP.
#' @param process.log Log file path.
#' @param staged.dir The directory containing "staged" forms.
#' @param invalid.dir The directory to store forms that could not
#'   be validated.
#' @param errored.dir The directory to store forms that encountered
#'   errors during processing.
#' @param archived.dir The directory to store forms that were
#'   processed successfully.
#' @inheritParams wqpr::wqp_use_program
#' @inheritParams wqpr::wqp_use_database
#' @param token The token string. If `NULL`, form insert will be
#'   skipped.
#' @return An outcome tibble, i.e. output of  outcome_tibble()`.
#'
#' @importFrom wqpr frm_read frm_format frm_check frm_insert
#' @importFrom stringr str_replace
#'
#' @export
process_form = function(form, overwrite = FALSE, process.log = "",
  staged.dir, invalid.dir, errored.dir, archived.dir,
  program, database, token) {
  on.exit(append_log("\n\n", file = process.log))
  append_log("Processing", form, file = process.log)
  # read form
  success = auto_try(
    form.data <- frm_read(form),
    logfile = process.log
  )
  if (!success) {
    append_log("... Error reading form", file = process.log)
    return(outcome_tibble("FAILED"))
  }
  # handle incomplete form
  if (!form.data$completed) {
    append_log("... form is incomplete", file = process.log)
    # move file
    incomplete.path = str_replace(form, staged.dir, invalid.dir)
    moved = move_file(form, incomplete.path)
    if (!moved) {
      append_log("... Could not move file", file = process.log)
    } else {
      append_log("... Moved file to", incomplete.path,
        file = process.log)
    }
    # write form log
    log.path = str_replace(incomplete.path, ".xlsx$", "_LOG.txt")
    logged = write_file(log.path, form.data$log)
    if (!logged) {
      append_log("... Could not write log", file = process.log)
    } else {
      append_log("... Wrote log to", log.path, file = process.log)
    }
    return(outcome_tibble("INCOMPLETE", moved, logged))
  }
  ### TEMP: form translation
#  if (form.data$event[[1]]$event_type_name != "Visit") {
#    form.data = translate_metadata(form.data)
#  }
  # validate form
  append_log("Validating", form, file = process.log)
  success = auto_try(
    validated.form <- frm_format(form.data, na.omit = TRUE,
      program = program, database = database),
    logfile = process.log
  )
  if (!success) {
    append_log("... Error validating form", file = process.log)
    return(outcome_tibble("FAILED"))
  }
  # handle invalid form
  if (!validated.form$validated) {
    append_log("... form has validation errors", file = process.log)
    # move file
    invalid.path = str_replace(form, staged.dir, invalid.dir)
    moved = move_file(form, invalid.path)
    if (!moved) {
      append_log("... Could not move file", file = process.log)
    } else {
      append_log("... Moved file to", invalid.path, file = process.log)
    }
    # write form log
    log.path = str_replace(invalid.path, ".xlsx$", "_LOG.txt")
    logged = write_file(log.path, validated.form$log)
    if (!logged) {
      append_log("... Could not write log", file = process.log)
    } else {
      append_log("... Wrote log to", log.path, file = process.log)
    }
    return(outcome_tibble("INVALID", moved, logged))
  } else {
    append_log("... Validated form", file = process.log)
  }
  # check form
  append_log("Checking for data matching form", form,
    file = process.log)
  success = auto_try(
      checked.form <- frm_check(validated.form, program = program,
        database = database),
      logfile = process.log
    )
  if (!success) {
    append_log("... Error checking form", file = process.log)
    return(outcome_tibble("FAILED"))
  }
  if (checked.form$unique) {
    append_log("... Form contains new data", file = process.log)
  } else if (overwrite) {
    append_log("... Form contains data flagged for overwrite",
      file = process.log)
  } else {
    # catch accidental duplicates
    append_log("... Conflicting data already exists into WQP",
      file = process.log)
    # move file
    invalid.path = str_replace(form, staged.dir, invalid.dir)
    moved = move_file(form, invalid.path)
    if (!moved) {
      append_log("... Could not move file", file = process.log)
    } else {
      append_log("... Moved file to", invalid.path, file = process.log)
    }
    # write form log
    log.path = str_replace(invalid.path, ".xlsx$", "_LOG.txt")
    logged = write_file(log.path, checked.form$log)
    if (!logged) {
      append_log("... Could not write log", file = process.log)
    } else {
      append_log("... Wrote log to", log.path, file = process.log)
    }
    return(outcome_tibble("INVALID", moved, logged))
  }
  # skip if token is NULL
  if (is.null(token)) {
    append_log("... skipping insert", file = process.log)
    return(outcome_tibble("SKIPPED"))
  }
  # insert form
  success = auto_try(
    inserted.form <- frm_insert(checked.form, overwrite = overwrite,
      program = program, database = database, token = token),
    logfile = process.log
  )
  if (!success) {
    append_log("... Error inserting form", file = process.log)
    return(outcome_tibble("FAILED"))
  }
  # handle insertion error
  if (!inserted.form$inserted) {
    append_log("... Could not insert data into WQP", file = process.log)
    # move errored form
    errored.path = str_replace(form, staged.dir, errored.dir)
    moved = move_file(form, errored.path)
    if (!moved) {
      append_log("... Could not move file", file = process.log)
    } else {
      append_log("... Moved file to", errored.path, file = process.log)
    }
    # write form log
    log.path = str_replace(errored.path, ".xlsx$", "_LOG.txt")
    logged = write_file(log.path, inserted.form$log)
    if (!logged) {
      append_log("... Could not write log", file = process.log)
    } else {
      append_log("... Wrote log to", log.path, file = process.log)
    }
    return(outcome_tibble("ERRORED", moved, logged))
  } else {
    append_log("... Inserted data into WQP", file = process.log)
  }
  # archive form
  append_log("... Archiving", form, file = process.log)
  archived.path = str_replace(form, staged.dir, archived.dir)
  moved = move_file(form, archived.path)
  if (!moved) {
    append_log("... Could not move file", file = process.log)
  } else {
    append_log("... Moved file to", archived.path, file = process.log)
  }
  return(outcome_tibble("ARCHIVED", moved))
}

#' Bind Results
#'
#' Helper function to bind results and reformat file paths.
#'
#' @param result.list List of outcome tibbles, i.e. outputs of
#'   `process_form()`.
#' @param source.dir The common source directory of forms,
#'   typically the "Staged" or "Corrected" directory.
#' @param archived.dir The directory containg "archived" forms.
#' @param invalid.dir The directory containing "invalid" forms.
#' @param errored.dir The directory containing "errored" forms.
#' @param local.path The local part of the path to the SharePoint
#'   directory.
#' @return A tibble of formatted results.
#'
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows mutate case_when
#' @importFrom stringr str_replace
#'
#' @keywords internal
bind_results = function(result.list, source.dir, archived.dir,
  invalid.dir, errored.dir, local.path = "") {
  # bind results
  if (length(result.list) < 1L) {
    result.list = list(outcome_tibble(character(0)))
  }
  results = bind_rows(result.list, .id = "form")
  # fix file paths
  results = mutate(results, form = case_when(
    !.data$moved ~ .data$form,
    .data$outcome == "ARCHIVED" ~ str_replace(.data$form,
      source.dir, archived.dir),
    .data$outcome == "INVALID"  ~ str_replace(.data$form,
      source.dir, invalid.dir),
    .data$outcome == "INCOMPLETE" ~ str_replace(.data$form,
      source.dir, invalid.dir),
    .data$outcome == "ERRORED" ~ str_replace(.data$form,
      source.dir, errored.dir),
    TRUE ~ .data$form
  ))
  # remove local part of file path
  mutate(results, form = str_replace(.data$form, local.path, ""))
}
