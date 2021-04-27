#' Move File
#'
#' Wrapper for `file.rename()` that creates destination directory if
#'   it does not exist.
#'
#' @param from Source directory.
#' @param to Destination directory.
#'
#' @keywords internal
move_file = function(from, to) {
  to.dir = dirname(to)
  if (!dir.exists(to.dir)) {
    dir.create(to.dir, recursive = TRUE)
  }
  file.rename(from, to)
}

#' Write File
#'
#' Wrapper for `file.create()` and `writeLines()` that creates
#'   destination directory if it does not exist.
#'
#' @inheritParams move_file
#' @param lines Vector of lines to write.
#'
#' @keywords internal
write_file = function(to, lines) {
  to.dir = dirname(to)
  if (!dir.exists(to.dir)) {
    dir.create(to.dir, recursive = TRUE)
  }
  file.create(to)
  f = try(writeLines(unlist(lines), to))
  is.null(f)
}


#' Append To Log
#'
#' Append lines to file. Essentially a wrapper for `cat()` with
#'  specific argument values.
#'
#' @param ... Content to append to file.
#' @param file A connection, or a character string naming the file to
#'   print to. If "" (the default), cat prints to the standard output
#'   connection, the console unless redirected by sink.
#' @param sep A character vector of strings to append after each
#'   element.
#' @param fill A logical or (positive) numeric controlling how the
#'   output is broken into successive lines. If FALSE (default), only
#'   newlines created explicitly by "\\n" are printed. Otherwise,
#'   the output is broken into lines with print width equal to the
#'   option width if fill is TRUE, or the value of fill if this is
#'   numeric. Non-positive fill values are ignored, with a warning.
#' @param labels Character vector of labels for the lines printed.
#'   Ignored if fill is FALSE.
#' @param append Logical. Only used if the argument file is the name
#'   of file (and not a connection or "|cmd") . If TRUE output will
#'  be appended to file; otherwise, it will overwrite the contents
#'  of file.
#'
#' @keywords internal
append_log = function(..., file, sep = "\n", fill = FALSE,
  labels = NULL, append = TRUE) {
  cat(..., file = file, sep = sep, fill = fill, labels = labels,
    append = append)
  invisible()
}
