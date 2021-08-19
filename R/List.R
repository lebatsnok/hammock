#' Make a named list
#'
#' Make a named list. List(a,b,c) is equivalent to list(a=a, b=b, c=c);
#' List(o=a, b) is equivalent to list(o=a, b=b). Unnamed list elements are used
#' to create names: either using make.names or force. 
#'
#' @param ...  elements of list
#' @param ..umn use make.names?
#' @return a named list
#' @export
List <- function(..., ..umn=TRUE){
  # make a named list e.g. List(a,b,c) === list(a=a,b=b,c=c)
  # List(o=a, b) === list(o=b, b=b)
  # named args starting with dot are converted to attributes
  # if ..umn %in% TRUE then make.names is used to make names
  
  nf <- if(..umn) make.names else force
  a <- list(...)
  sa <- sapply(substitute(list(...)), deparse)[-1]
  dottish <- grep("^\\.", names(a))
  attic <- a[dottish]
  names(attic) <- sub("^\\.", "", names(attic))
  if(length(dottish)){
    a<- a[-dottish]
    sa <- sa[-dottish]
  }
  sa <- unname(sa)
  sa[which(names(a)!="")] <- names(a)[which(names(a)!="")]
  L <- structure(a, names=nf(sa))
  attributes(L) <- c(attributes(L), attic)
  L
}
