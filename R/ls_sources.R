#' List objects in R soucre files.
#'
#' ls.soucres. 
#'
#' The *.R files in a folder (defaults to the current working
#' directory) are Source'd and the names of the objects listed.
#' This does not add anything to the global environment (see
#' `Source`). 
#'
#' @param DIR name of the directory, defaults to "."
#' @param use.basename Whether to use basename (TRUE) or full name for list names
#' 
#' @return a list with one component per each *.R file in the folder 
#' @seealso \code{\link{Source}}
#' 
#' @details This is useful to get an overview of what's where e.g in 
#' an R/ subfolder of a package. 
#' @export
ls.sources <- function (DIR = ".", use.basename = TRUE) 
{
  Rfiles <- dir(DIR, pattern = "\\.[Rr]$", full.names = TRUE, recursive=TRUE)
  if (length(Rfiles) < 1) stop("No R source files in the directory ", DIR)
  BN <- if (use.basename) basename   else force
  allSourced <- lapply(setNames(Rfiles, BN(Rfiles)), function(x) try(Source(x, to.list=FALSE)))
  lapply(allSourced, ls)
}
