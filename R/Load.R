#' Load save'd objects and return them as a list 
#'
#' Obects in `file` are loaded and returned as a value (instead of 
#' creating these objects in the global environment). The value can be
#' a list (default) or an environment (if to.list is FALSE); if there
#' is just one object in the file to be read, it is by default unlisted
#' and just the objects' value is returned.
#'
#' @param file the file to load
#' @param to.list if TRUE, a list is returned, otherwise an environment
#' @param Unlist whether to "unlist" length 1 lists (default is TRUE)
#' @param spray whether to create these objects in the global environment (default is FALSE)
#' @param delete.parent if an environment is returned, whether to set its parent env to an empty environment (to make a smaller object)
#' @param Attach to attach the loaded objects to the search path
#' @return a list or environment (depending on to.list) or other object (if there is just one object saved and Unlist equals TRUE) or invisibly NULL if Attach==TRUE
#' @seealso \code{\link{load}}
#' 
#' @details This is useful to avoid overwriting objects in your workspace, 
#' for using only parts of a file, and inspecting the contents of files 
#' before you load them. You can set your own names to the objects instead
#' of using the ones from the file. E.g., 
#'
#' \code{new.name <- Load("manyobjects.rda")$old.name }
#' \code{ls(Load("manyobjects.rda"))}
#'
#' @export
Load <- function(file, to.list=TRUE, Unlist = TRUE, spray = FALSE, delete.parent = FALSE, Attach=FALSE){
  
  attachYN <- (is.logical(Attach) & isTRUE(Attach)) | is.character(Attach)
  if(attachYN) {
    attachNAME <- if(is.logical(Attach)) date() else Attach[1]
  }
  NE <- if(spray) .GlobalEnv else if(attachYN) attach(NULL, name=attachNAME) else new.env()
  load(file, NE)
  if(attachYN) return(invisible(NULL))
  if(to.list){
    NE <- as.list(NE)
    if(Unlist & length(NE) == 1) NE <- NE[[1]]
  }
  if(delete.parent) parent.env(NE)<-emptyenv()
  NE
}
