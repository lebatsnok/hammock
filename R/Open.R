#' Open a file using windows or linux default file associations 
#'
#' Open a file using windows default file associations (call with 
#' no arguments to open file explorer in the current working directory)
#' Don't know how to do it on Mac. 
#'
#' @param x the file to load
#' @return NULL; used for side effect 
#' @export
Open <- function(x = "") {
  os <- getArch()$OS
  fp <- file.path(getwd(), x)
  if (os == "windows") 
    shell.exec(fp)
  else {
    fp <- sub(" ", "\\\\ ", fp)
    fp <- sub("/$", "", fp)
    system(paste("xdg-open", fp))
  }
  # or linux : system(paste("mimeopen -n", getwd()) )
}
