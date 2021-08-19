#' Rounding operator 
#'
#' Rounding as usual but using an infix operator
#' 
#' @param x a numeric vector
#' @param digits a number
#' @return Rounded value
#' @description Rounds the values in its first argument to the specified number of decimal places
#' 
#' 
#' @rdname rounding
#' @export
`%R%` <- 
  function(x, digits=0) 
    {
    if(is.na(digits)) x else round(x,digits) 
  }


#' Rbind operator 
#'
#' Rbind as usual but using an infix operator
#' 
#' @param x a vector, mtx, or data frame
#' @param y a vector, mtx, or data frame
#' @return rbind ( x,y)
#' @description shorthand for rbind
#' 
#' 
#' @rdname rbinding
#' @export
`%=%` <- 
  function(x, y) 
  {
    rbind(x,y)
  }

#' Cbind operator 
#'
#' Cbind as usual but using an infix operator
#' 
#' @param x a vector, mtx, or data frame
#' @param y a vector, mtx, or data frame
#' @return cbind ( x,y)
#' @description shorthand for cbind
#' 
#' 
#' @rdname cbinding
#' @export
"%''%" <- 
  function(x, y) 
  {
    cbind(x,y)
  }
