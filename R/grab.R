#' Select columns from a data frame using regular expressions
#'
#' Returns the set of columns in a data frame whose names
#' correspond to a specific pattern 
#'
#' @aliases grab grabn
#' @param x an object
#' @param pattern a pattern
#' @param ... ...
#' @return something
#' 
#' @rdname grab
#' @export
grab <- function(x, pattern, ...){
  UseMethod("grab")
}

#' @return \code{NULL}
#' 
#' @rdname grab
#' @method grab list
#' @export
grab.list <- function(x, pattern, ...){
  foo <- names(x)[grep(pattern, names(x))]  
  x[,foo]
}

#' @return \code{NULL}
#' 
#' @rdname grab
#' @method grab data.frame
#' @export
grab.data.frame <- function(x, pattern, ...){
  foo <- names(x)[grep(pattern, names(x))]  
  x[,foo]
}


#' @return \code{NULL}
#' 
#' @rdname grab
#' @method grab character
#' @export
grab.character <- function(x, pattern, ...) x[grep(pattern,x)]
