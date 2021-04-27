#' Automatic Form Processing
#'
#' Automatically process forms, log errors, and send report via email.
#'
#' @param local.path The local path to the SharePoint directory
#'   containing the form directories.
#' @param forms.dir SharePoint directory containing forms.
#' @param workspace The processing workspace. The log file will
#'   be saved here.
#' @param pattern A regex string to filter the forms listing.
#'   Default is to list all modern Excel files, e.g. `".xlsx"`.
#' @param limit Upper limit to number of forms to process. Limit
#'   applies separately to staged forms and corrected forms.
#' @inheritParams process_form
#' @param email.template The email template used when sending reports
#'   to staff.
#' @return The path to the generated log file.
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter bind_rows case_when
#' @importFrom stringr str_replace str_detect
#' @importFrom utils head
#' @export
auto_forms = function(local.path, forms.dir,
  workspace = ".", pattern = ".xlsx$", limit = 25L,
  program = "marsh", database = "production", token = NULL,
  email.template) {
  # email template
  if (missing(email.template)) {
    email.template = system.file("templates/forms-template.yaml",
      package = "autowqp")
  } else {
    email.template = normalizePath(email.template, "/")
  }
  if (!file.exists(email.template)) {
    stop("Could not find email template ", email.template)
  }
  # directories
  workspace = gsub("\\", "/", workspace, fixed = TRUE)
  local.path = gsub("\\", "/", local.path, fixed = TRUE)
  sharepoint.dir = file.path(local.path, forms.dir)
  if (!dir.exists(sharepoint.dir)) {
    stop("Could not find directory ", sharepoint.dir)
  }
  # set workspace
  if (!dir.exists(workspace)) {
    stop("Could not find workspace ", workspace)
  }
  cwd = getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(workspace)
  # create log file
  process.log = paste0("autowqp_", Sys.Date(), "_LOG.txt")
  if (!file.exists(process.log)) {
    if (!file.create(process.log)) {
      stop("could not create log file ", process.log)
    }
  } else {
    if (file.access(process.log, mode = 2) < 0) {
      stop("could not write to existing log file ", process.log)
    }
  }
  on.exit(append_log("\n\n", file = process.log), add = TRUE)
  # form directories
  staged.dir = file.path(sharepoint.dir, "STAGED")
  invalid.dir = file.path(sharepoint.dir, "INVALID")
  errored.dir = file.path(sharepoint.dir, "ERRORED")
  archived.dir = file.path(sharepoint.dir, "ARCHIVED")
  corrected.dir = file.path(sharepoint.dir, "CORRECTED")
  # get all forms
  staged.forms = dir(staged.dir, full.names = TRUE,
    recursive = TRUE)
  corrected.forms = dir(corrected.dir, full.names = TRUE,
    recursive = TRUE)
  # filter by pattern
  staged.forms = staged.forms[grep(pattern, staged.forms)]
  corrected.forms = corrected.forms[grep(pattern, corrected.forms)]
  # apply limit
  staged.forms = head(staged.forms, limit)
  corrected.forms = head(corrected.forms, limit)
  # process new forms
  staged.result.list = list()
  for (form in staged.forms) {
    staged.result.list[[form]] = process_form(form, FALSE, process.log,
      staged.dir, invalid.dir, errored.dir, archived.dir,
      program, database, token)
  }
  # process corrected forms
  corrected.result.list = list()
  for (form in corrected.forms) {
    corrected.result.list[[form]] = process_form(form, TRUE,
      process.log,
      corrected.dir, invalid.dir, errored.dir, archived.dir,
      program, database, token)
  }
  # format file paths
  staged.results = bind_results(staged.result.list, staged.dir,
    archived.dir, invalid.dir, errored.dir, local.path)
  corrected.results = bind_results(corrected.result.list, corrected.dir,
    archived.dir, invalid.dir, errored.dir, local.path)
  # combine results
  all.results = bind_rows(staged.results, corrected.results)
  all.results = mutate(all.results, form = str_replace(.data$form,
    sharepoint.dir, forms.dir))
  # send emails
  mail_forms(all.results, email.template)
  invisible(process.log)
}


#' Automatic QAQC Processing
#'
#' Automatically process QAQC flags and send report via email.
#'
#' @inheritParams check_qaqc
#' @inheritParams auto_forms
#' @return The path to the generated log file.
#'
#' @export
auto_qaqc = function(workspace = ".", cdec.code, analyte.name, from,
  to, program = "marsh", database = "production", email.template) {
  # email template
  if (missing(email.template)) {
    email.template = system.file("templates/qaqc-template.yaml",
      package = "autowqp")
  } else {
    email.template = normalizePath(email.template, "/")
  }
  if (!file.exists(email.template)) {
    stop("Could not find email template ", email.template)
  }
  # set workspace
  workspace = gsub("\\", "/", workspace, fixed = TRUE)
  if (!dir.exists(workspace)) {
    stop("Could not find workspace ", workspace)
  }
  cwd = getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(workspace)
  # create log file
  process.log = paste0("autoqaqc_", Sys.Date(), "_LOG.txt")
  if (!file.exists(process.log)) {
    if (!file.create(process.log)) {
      stop("could not create log file ", process.log)
    }
  } else {
    if (file.access(process.log, mode = 2) < 0) {
      stop("could not write to existing log file ", process.log)
    }
  }
  on.exit(append_log("\n\n", file = process.log), add = TRUE)
  append_log("Processing QAQC flags",
    paste(from, "-", to),
    paste(cdec.code, collapse = ", "),
    paste(analyte.name, collapse = ", "),
    file = process.log)
  # check QAQC
  success = auto_try(
    results <- check_qaqc(cdec.code, analyte.name,
      from, to, program, database),
    logfile = process.log
  )
  if (!success) {
    append_log("... Error checking QAQC flags", file = process.log)
  } else {
    mail_qaqc(results, from, to, email.template, log = process.log)
  }
  invisible(process.log)
}

#' Automatic Action Consistency Checking
#'
#' Automatically check actions for consistency and send report
#'   via email.
#'
#' @inheritParams check_actions
#' @inheritParams auto_forms
#' @return The path to the generated log file.
#'
#' @importFrom dplyr group_by ungroup filter
#' @export
auto_actions = function(workspace = ".",
  actions = c("Removed", "Installed"), from, to,
  program = "marsh", database = "production", email.template) {
  station.field = "station_name"
  sonde.field = "sonde_name"
  consistent.field = "consistent"
  # email template
  if (missing(email.template)) {
    email.template = system.file("templates/action-template.yaml",
      package = "autowqp")
  } else {
    email.template = normalizePath(email.template, "/")
  }
  if (!file.exists(email.template)) {
    stop("Could not find email template ", email.template)
  }
  # set workspace
  workspace = gsub("\\", "/", workspace, fixed = TRUE)
  if (!dir.exists(workspace)) {
    stop("Could not find workspace ", workspace)
  }
  cwd = getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(workspace)
  # create log file
  process.log = paste0("autoactions_", Sys.Date(), "_LOG.txt")
  if (!file.exists(process.log)) {
    if (!file.create(process.log)) {
      stop("could not create log file ", process.log)
    }
  } else {
    if (file.access(process.log, mode = 2) < 0) {
      stop("could not write to existing log file ", process.log)
    }
  }
  on.exit(append_log("\n\n", file = process.log), add = TRUE)
  append_log("Processing actions",
    paste(from, "-", to),
    file = process.log)
  # check actions
  sonde.success = auto_try(
    sonde.results <- check_actions(actions, from, to,
      group.by = "sonde", program, database),
    logfile = process.log
  )
  if (!sonde.success) {
    append_log("... Error checking sonde consistency",
      file = process.log)
  }
  station.success = auto_try(
    station.results <- check_actions(actions, from, to,
      group.by = "station", program, database),
    logfile = process.log
  )
  if (!station.success) {
    append_log("... Error checking station consistency",
      file = process.log)
  }
  if (sonde.success && station.success) {
    station.filtered = ungroup(filter(
      group_by(station.results, .data[[station.field]]),
      any(!.data[[consistent.field]])
    ))
    sonde.filtered = ungroup(filter(
      group_by(sonde.results, .data[[sonde.field]]),
      any(!.data[[consistent.field]])
    ))
    mail_actions(station.filtered, sonde.filtered, from, to,
      email.template, log = process.log)
  }
  invisible(process.log)
}
