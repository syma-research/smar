#' Subsetting a distance object by indicators
#'
#' @param dist an n x n distance object
#' @param ind length-n subsetting indicator. Must be either character (sample names) or
#' logical (TRUE/FALSE indicator).
#'
#' @return the subsetted distance object
#' @export
subset_distance <- function(dist, ind) {
  if(class(dist) != "dist")
    stop("dist must be a distance class!")
  if(!(class(ind) %in% c("character", "logical")))
    stop("ind must be a name/logical vector!")

  dist_matrix <- as.matrix(dist)
  if(class(ind) == "logical" & nrow(dist_matrix) != length(ind))
    stop("Dimensions of dist and ind must match!")
  if(class(ind) == "character" & !all(ind %in% rownames(dist_matrix)))
    stop("ind must be a subset of the row/col names of dist!")

  return(as.dist(dist_matrix[ind, ind]))
}
