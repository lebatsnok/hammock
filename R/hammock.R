#' \tabular{ll}{
#' Package: \tab hammock\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2012-08-02\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Functions for lazy people
#'
#' \code{\link{Load}} loads \code{file} and returns a value 
#' rather than creating the objects in Global Env.
#'
#' @name hammock-package
#' @aliases hammock
#' @docType package
#' @title Hammock package
#' @author Kenn Konstabel \email{kenn.konstabel AT tai.ee}
#' @references
#' \url{http://en.wikipedia.org/wiki/hammock}
#' @keywords package
#' @seealso \code{\link{Load}}
#' @examples NULL
#' 
#'
NULL


getArch <- function(){
  OS <- .Platform$OS.type
  Arch <- R.Version()$arch
  X64 <- grepl("64", Arch)
  list(OS=OS, X64=X64)
}

'combinePDF <- function(f1, f2, outfile){
  A <- getArch()
  progname <- if(A$OS=="windows") paste0("gswin", if(A$X64) 64 else 32) else "gs"
  shellstr <- sprintf("%s -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=%s %s %s", progname, outfile, f1, f2)
  shell <- if(A$OS=="Windows") shell else system
  # for compressing pdf use shellstr <- sprintf("gswin64 -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile=%s %s", outfile, infile)
  shell(shellstr)
}'


#' Split a string into a character vector
#'
#' Splits a string: c_("a b c") is equivalent to c("a", "b", "c")
#' A wrapper around strsplit
#'
#' @param str a string
#' @param sep separator
#' @return a vector
#' @export
c_ <- function(str, sep=" "){
     strsplit(str, sep)[[1]]
     }

    
#' Partial function application
#'
#' Partial function application using do.call
#' by Byron Ellis, https://stat.ethz.ch/pipermail/r-devel/2007-November/047318.html
#' But see also discussion in R-devel list (Hadley Wickham, Luke Tierney et al)
#'
#' @param FUN a function
#' @param ... dot dot dot
#' @return a function
#' @export
Curry <- function(FUN,...) { 
  .orig <- list(...)
  function(...) do.call(FUN,c(.orig,list(...))) 
  }


#' Simple merging of data frames
#'
#' Merges two data frame in a simple and safe way. 
#' The result will have all the rows from the first data frame 
#' exactly in the same order. The new rows from the second data
#' frame are added to the end.
#'
#' 
#'
#' @param x a data frame
#' @param y a data frame
#' @param by character: name of the variable to be used for merging
#' @param finally function to be applied to the two data frames after they're made to match
#' @param unrowname logical: delete row names?
#' @param ... parameters to be passed to `finally`
#' @return the 2 data frames made to match row by row, combined as specified by `finally`
#' @export
Smerge <- function(x, y, by, finally = data.frame, unrowname = TRUE, ...){
    if(is.vector(x)) x<- setNames(data.frame(foo=x), by)
    BYX <- as.character(x[[by]])   # to avoid factors
    BYY <- as.character(y[[by]])
    if(any(duplicated(BYX)) || any(duplicated(BYY)) ) stop("Need unique index for smerging")
    BY <- c(BYX, BYY[ ! BYY %in% BYX])
    x <-  x[match(BY, x[[by]]), drop=FALSE]
    x[[by]] <- BY
    y <-  y[match(BY, y[[by]]), drop=FALSE]
    y[[by]] <- BY
    if(unrowname) rownames(x) <- rownames(y) <- NULL
    finally(x, y, ...)
    }


#' Sort a data frame using one of its variabes as index
#'
#' Sorts a data frame using a variable within that data frame as an index.
#' it's basically just x[order(x[[by]])]
#' Could be written as a method of `sort` for data frame but need 2 figure out how to do it
#'
#' 
#'
#' @param x a data frame
#' @param decreasing passed to order
#' @param by name of the 
#' @param ... additional args passed to next method if they get a chance
#' @return a sorted data frame
#' @method sort data.frame
#' @export sort.data.frame
sort.data.frame <- function(x, decreasing = FALSE, by, ...){
    ORD <- order(x[[by]], decreasing = decreasing)
    x[ORD,]
    }


#' Source and attach a set of functions from a file as a pseudo package
#'
#' A set of functions or other objects is read in and attached
#' to the search path. This will be reworked: adding just an
#' xtra parameter to Source.
#'
#' 
#'
#' @param what a name
#' @param www read from www?
#' @param DIR name of the folder where the files are located
#' @return nothing useful
#' @export
nload <- function(what, www=FALSE, DIR = "Rfns") {
   do_require <- require
   nimi <- paste(what, ".txt", sep="")
   nimi2<- paste(DIR, nimi, sep="/")
   if(file.exists(nimi2) && !www) nimi<-nimi2 
   if(what=="score" || what=="fns") do_require("RODBC")
   if(substr(what,1,3)=="acc") do_require("zoo")
   if(www) {
      URL <- paste("http://psych.ut.ee/~nek/R/", nimi, sep="")
      if(file.exists(DIR) && !file.info(DIR)["isdir"]) stop("Cannot create directory *", DIR, "*")
      if(!file.exists(DIR)) dir.create(DIR)
      download.file(URL, nimi2, "auto")
      nimi <- nimi2
      }
   ENV <- attach(NULL, name=what)
   sys.source(nimi, ENV, keep.source=TRUE)
   rm(ENV)
}

#' Write a csv file in a convenient way
#'
#' A  wrapper around write.csv: you just need to tell it
#' the object name, file name is made from it by adding the appropriate extension.
#'
#' 
#'
#' @param x file name 
#' @export
Csv <- function(x) {
  write.csv(x, paste(deparse(substitute(x)), ".csv", sep=""))
}

#' Select objects to be removed 
#'
#' Select objects to be removed 
#'
#' 
#'
#' @param all defaults FALSE
#' @export
rm2 <- function (all=FALSE) {
  L <- select.list(ls(pos=1, all.names=all), multiple=TRUE)
  rm(list=L, pos=1)
}
