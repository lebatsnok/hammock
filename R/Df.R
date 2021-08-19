#' Nice data frame 
#'
#' A version of data.frame with stringsAsFactors and check.names defaulting to FALSE.
#' 
#'
#' @param ... these arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#' @param vars A data.frame with meta-data on variables
#' @param info An informative title of the data frame
#' @param row.names	NULL or a single integer or character string specifying a column to be used as row names, or a character or integer vector giving the row names for the data frame.
#' @param check.rows	if TRUE then the rows are checked for consistency of length and names.
#' @param check.names	logical; defaults to FALSE. If TRUE then the names of the variables in the data frame are checked to ensure that they are syntactically valid variable names and are not duplicated. If necessary they are adjusted (by make.names) so that they are.
#' @param stringsAsFactors	logical: should character vectors be converted to factors? Defaults (of course!!) to FALSE. 
#' @param emptyStringsAsNA logical: should empty strings (within character vectors) be converted to NA? Defaults to TRUE.
#' @return a data frame 
#' @seealso \code{\link{data.frame}}
#' 
#' @export
Df <- function(..., vars = NULL, info = NULL, row.names=NULL, check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE, emptyStringsAsNA = TRUE){
  res <- data.frame(..., row.names = row.names, check.rows = check.rows, check.names=check.names,
                    stringsAsFactors = stringsAsFactors)
  if(emptyStringsAsNA){
    for(iii in seq_len(NCOL(res))) if(is.character(res[,iii])) {
      resii <- res[,iii]
      resii[resii==""] <- NA
      res[,iii] <- resii
    }
  }
  if(NROW(vars)!=NCOL(res)) stop("NROW of vars not equal to NCOL of DF!")
  if(is.vector(vars)) vars <- data.frame(NAME = names(res), LAB = vars, stringsAsFactors = FALSE)
  vars[, sapply(vars, is.factor)] <- lapply(vars[, sapply(vars, is.factor)], as.character)
  vars <- subset(vars, NAME %in% names(res))
  vars <- vars[!duplicated(vars),]
  vars <- vars[match(names(res), vars$NAME), ]
  if(!all.equal(vars$NAME, names(res))) stop("must have one row for each var!")
  for(iii in seq_len(NCOL(res))) {
    attr(res[[iii]], "labs") <- vars[iii,]
  }
  # attr(res, "vars") <- vars
  attr(res, "info") <- info
  attr(res, "template") <- names(vars)
  class(res) <- c("Df", "data.frame")
  res
}

bucksarrow <- function(x, name, value){
  cl <- oldClass(x)
  class(x) <- NULL
  nrows <- .row_names_info(x, 2L)
  if (!is.null(value)) {
    N <- NROW(value)
    if (N > nrows) 
      stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                            "replacement has %d rows, data has %d"), N, nrows), 
           domain = NA)
    if (N < nrows) 
      if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 
          1L) 
        value <- rep(value, length.out = nrows)
      else stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                                 "replacement has %d rows, data has %d"), N, nrows), 
                domain = NA)
      if (is.atomic(value) && !is.null(names(value))) 
        names(value) <- NULL
  }
  x[[name]] <- value
  class(x) <- cl
  x
}

#' @return \code{NULL}
#' 
#' @rdname Df
#' @export
`$<-.Df` <- function(x, name, value){
  if(! name %in% names(x)){
    template <- attr(x, "template")
    leg <- as.data.frame(as.list(rep(NA, length(template))))
    names(leg) <- template
    if("NAME" %in% template) leg$NAME <- name
    attr(value, "labs") <- leg
  }
  x <- bucksarrow(x, name, value)
  x
}

#' @return \code{NULL}
#' 
#' @rdname Df
#' @export
`[[<-.Df` <- function(x, i, j, value){
  if(! name %in% names(x)){
    template <- attr(x, "template")
    leg <- as.data.frame(as.list(rep(NA, length(template))))
    names(leg) <- template
    if("NAME" %in% template) leg$NAME <- name
    attr(value, "labs") <- leg
  }
  x <- `[[<-.data.frame`(x, i, j, value)
  x
}
