#' Get or set lengend and info on a Df 
#'
#' Sets or gets attributes .... .... 
#' 
#'
#' @aliases labs info
#' @param x an object
#' @param value a value
#' @return Invisible x
#' 
#' @rdname labs
#' @export
labs <- function(x){
  UseMethod("labs")
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @method labs Df
#' @export
labs.Df <- function(x){
   a <- sapply(x, attributes)
   n <- sapply(a, "[[", "NAME")
   l <- sapply(a, "[[", "LAB")
   setNames(l, n)
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @export
"labs<-" <- function(x, value){
  UseMethod("labs<-")
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @method labs<- Df
#' @export
"labs<-.Df" <- function(x, value){
  attr(x, "labs") <- value
  x
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @export
info <- function(x){
  UseMethod("info")
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @method labs Df
#' @export
info.Df <- function(x){
  attr(x, "info")
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @export
"info<-" <- function(x, value){
  UseMethod("info<-")
}

#' @return \code{NULL}
#' 
#' @rdname labs
#' @method "info<-" Df
#' @export
"info<-.Df" <- function(x, value){
  attr(x, "info") <- value
  x
}

