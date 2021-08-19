#' Read.spss to Df 
#'
#' SPSS to Df
#' 
#'
#' @param x a file name
#' @param usv use value labels? passed to read.spss
#' @param browse FALSE
#' @param ... additional parameters passed to read.spss
#' @return a Df 
#' 
#' @export
read.Spss <- function(x, info="", usv=FALSE, browse = FALSE, ...){
  D <- foreign::read.spss(x, to.data.frame=TRUE, use.value.labels = usv, ...)
  Df <- D
  attr(Df, "variable.labels") <- NULL
  VLD <- attributes(D)$variable.labels
  VLD <- data.frame(NAME = names(VLD), TAG = VLD, stringsAsFactors = FALSE)
  VL2 <- sapply(D, function(x) attr(x, "value.labels"))
  if(browse) browser()
  VL3 <- sapply(D, function(x) attr(x, "levels"))
  class(Df) <- c("Df", "data.frame")
  tags(Df) <- VLD
  info(Df) <- info
  Df
}


#' Value labels from SPSS file 
#'
#' SPSS value labels
#' 
#'
#' @param x
#' 
#' @export
vlabs <- function(x){
  svl <- attr(x, "value.labels")
  l2 <- attr(x, "levels")
  res <- data.frame(Code=svl, Label=names(svl))
  res <- res[order(res$Code),]
  rownames(res) <- NULL
  res
}