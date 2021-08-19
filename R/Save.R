#' Save-as-you-go 
#'
#' Saves any objects, not just named ones
#' 
#' @param ... objs (should be named - othewise funny manes will be used)
#' @param file filename - must be there!
#' @return nothing useful ?
#' @seealso \code{\link{load}}
#' 
#' @details Save
#' 
#' \code{Save(bla = matrix(sample(1:10),2,4), foo = A[1:5, 2:7]) }
#'
#' @export
Save <- function(..., file)  {
  ..env <- as.environment(List(...))
  save(list=ls(..env, all.names=TRUE), envir=..env, file=file)
}
