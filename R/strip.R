#' Make a function to return its own environment
#'
#' Modifies a function so that it returns its own environment.
#' The new function returns an environment with all intermediate
#' variables calculated in the original function. The function's
#' value is assigned to `result..`
#' Doesn't work with S3 generics and primitive.
#'
#' @param fun. a function
#' @return a modified function
#' @export
strip <- function(fun.){
  curly <- function(e, f=NULL){
    ecb <- quote({})
    ecb[[2]] <- e
    if(!is.null(f)) ecb[[3]] <- f
    ecb
  }
  bf1 <- curly(body(fun.))
  arrow <- quote(result.. <- b)
  arrow[[3]] <- bf1
  body(fun.) <- curly(arrow, quote(environment()))
  fun.
}
