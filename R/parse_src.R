#' Parse source 
#'
#' Parse source
#' 
#' @param file a file name
#' @param keep either 'all' or 'fun'
#' @return value**
#' @description Parses an R source file to find objects (experimental)
#' 
#' 
#' @export
parse_source <- function(file, keep = "all", browse=FALSE){
  EXP <- parse(file)
  mtx <- matrix("", ncol=4, nrow=length(EXP))
  for(iii in seq_along(EXP)){
    jmax <- length(EXP[[iii]])
    if(jmax>3) jmax<-3
    for(jjj in 1:jmax) mtx[iii, jjj] <- deparse(EXP[[iii]][[jjj]])[1]
    EXPIIJJ <- EXP[[iii]][[jjj]]
    if(jmax==3) {
      TRY <- try(EXPIIJJ1 <- EXPIIJJ[[1]])
      mtx[iii,4] <- deparse(EXPIIJJ1)
    }
    # & !is.symbol(EXPIIJJ)) mtx[iii, 4] <- deparse(EXP[[iii]][[jjj]][[1]])
  }
  if(keep=="fun"){
    mtx <- as.data.frame(mtx)
    names(mtx) <- c("E", "T", "K", "N")
    mtx <- subset(mtx, N=="function")
    mtx$K <- substr(mtx$K, 1, 20)
    mtx <- mtx[, c("T", "K")]
  }
  if(browse) browser()
  mtx  
}

#' Parse sources 
#'
#' Parse sources
#' 
#' @param DIR a file name
#' @param SORT boolean
#' @return value**asd functions 
#' 
#' 
#' @export
parse_sources <- function(DIR=".", SORT=TRUE){
  FILES <- dir(DIR, pattern = "\\.R$", full.names=TRUE)
  RES <- setNames(lapply(FILES, parse_source, keep="fun"), FILES)
  RES <- RES[sapply(RES, NROW)>0]
  RES <- do.call(rbind, RES)
  RES$File <- rownames(RES)
  rownames(RES) <- NULL
  names(RES) <- c("Name", "Head", "File")
  RES[] <- lapply(RES, as.character)
  if(SORT) RES <- RES[order(RES$Name),]
  # browser()
  RES
}