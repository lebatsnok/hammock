#' Source a file and return all the created objects as a list or environment.
#'
#' The parameters are analogous to Load. 
#'
#' The `file` is sourced but evaluated in a special environment, and
#' the contents of that environment are then returned as a list or
#' environment. If just one object is created in the sourced code, 
#' then by default just that object's value is returned (if Unlist is TRUE).
#'
#' @param file the file to load
#' @param to.list if TRUE, a list is returned, otherwise an environment
#' @param Unlist whether to "unlist" length 1 lists (default is TRUE)
#' @param spray whether to create these objects in the global environment (default is FALSE)
#' @param delete.parent if an environment is returned, whether to set its parent env to an empty environment (to make a smaller object)
#' @param Attach to attach the Source'd environment
#' @param First a list of values to be included in the output (that the Source'd script can possibly use)
#' @return a list or environment (depending on to.list) or other object (if there is just one object saved and Unlist equals TRUE)
#' @seealso \code{\link{source}}
#' 
#' @details This is useful to avoid overwriting objects in your workspace, 
#' for using only parts of a file, and inspecting the contents of files 
#' before you load them. You can set your own names to the objects instead
#' of using the ones from the file. E.g., new.name <- Load("manyobjects.rda")$old.name,
#' or ls(Load("manyobjects.rda"))
#' @export
Source <- function(file, to.list=TRUE, Unlist = TRUE, spray=FALSE, delete.parent=FALSE, Attach=FALSE, First = list(), Setwd = NULL){
  if(!is.null(Setwd)){
    if(!is.character(Setwd)) stop("Setwd is not character")
    if(!file.exists(Setwd)) stop("Folder does not exist")
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(Setwd)
  }
    keep.env <- FALSE # for lists
  attachYN <- (is.logical(Attach) & isTRUE(Attach)) | is.character(Attach)
  if(attachYN) {
    attachNAME <- if(is.logical(Attach)) date() else Attach[1]
  }
  NE <- if(spray) .GlobalEnv else if(attachYN) attach(NULL, name=attachNAME) else new.env()
  list2env(First, NE)
  # if(!any(names(First)%in%ls())) list2env(First, environment())
  sys.source(file, NE)
  if(attachYN && !spray) return(invisible(NULL))
  if(to.list){
    NE <- as.list(NE)
    if(!keep.env && !spray) NE <- lapply(NE, function(x) {environment(x)<- parent.frame(3); x})
    if(Unlist & length(NE) == 1) NE <- NE[[1]]
  }
  if(delete.parent) parent.env(NE)<-emptyenv()
  NE
}
