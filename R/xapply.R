#' Apply an expression over a list of "arguments"
#'
#' "applies" an expression (.) to a list of "arguments". 
#' Analogous to mapply but works with expressions rather than functions.\cr
#' Examples:\cr
#' mxapply(a+b, a=1:5, b=2:6)\cr
#' mxapply(c(a+b, a-b), a=1:5, b=2:6)\cr
#' mxapply(c(a+b, a-b), a=1:5, b=2:6, FUN=rbind)\cr
#' mxapply(c(a+b, a-b), a=1:5, b=2:6, FUN=data.frame)\cr
#'
#' @param . an expression
#' @param ... named args (must be named!)
#' @param SIMPLIFY to return a simple vector or matrix if possible?
#' @param FUN function to be called to simplify the result (e.g rbind, cbind, data.frame)
#' @return a value
#' @export
xapply <- function (., ..., SIMPLIFY=TRUE, FUN = cbind) {
  . <- substitute(.)
  .. <- list(...)
  .L <- sapply(.., length)
  if(any(.L != max(.L))) stop("... args must be of the same length")
  .RES <- vector("list", max(.L)) 
  for(.iii in 1:max(.L)) .RES[[.iii]] <- eval(., envir=lapply(.., "[", .iii))
  lenx <- sapply(.RES, length)
  if(SIMPLIFY) {
    if(all(lenx==1)) .RES <- unlist(.RES) else
      if(all(lenx==max(lenx))) .RES <- do.call(FUN, .RES)
  }
  .RES
}
