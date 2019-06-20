#' Source all R scripts under a directory
#'
#' @param path
#' @param pattern
#' @param trace should list of files be outputted?
#' @param ...
#'
#' @return nothing
#' @export
#'
#' @examples
sourceDir <- function(path, pattern = "[.][Rr]$", trace = TRUE, recursive = FALSE,
                      ...) {
  for (nm in list.files(path, pattern = pattern, recursive = recursive, ...)) {
    source(file.path(path, nm), ...)
    if(trace) cat(nm,"\n")
  }
}
