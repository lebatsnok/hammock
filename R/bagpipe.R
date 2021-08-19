has_pipe <- function(x) {
  if(is.name(x) | is.function(x)) FALSE else identical(x[[1]], quote(`:=`))
}

has_fun <- function(x){
  if(!(inherits(x, "{") | inherits(x, "("))) return(FALSE)
  if(identical(x[[2]][[1]], as.name("function"))) TRUE else FALSE
}

no_fun <- function(x){
  if(!inherits(x, "{")) return(FALSE)
  if(!has_fun(x)) TRUE else FALSE
}

pipe_collector <- function(x){
  if(has_pipe(x)) c(x[[2]], Recall(x[[3]])) else x
}

jointcall <- function(lhs, rhs, env = parent.frame()) {
  if(is.name(rhs)) rhs <- as.call(list(rhs))
  if(has_fun(rhs)) rhs <- as.call(list(rhs)) ### 
  if(no_fun(rhs)) {
    rhs <- eval(call("function", as.pairlist(alist(.=)), rhs), env, env)
    rhs <- as.call(list(rhs))
  }
  exp <- rhs
  nle <- length(exp)+1
  if(nle>2) for(iii in nle:3) {
    exp[[iii]] <- exp[[iii-1]]
    names(exp)[iii] <- names(exp)[iii-1]
  }
  exp[[2]] <- lhs
  names(exp)[2] <- ""
  exp
}


#' Bagpipe operator 
#'
#' Bagpipe
#' 
#' @param ... params 
#' @return A value 
#' @description Ceci n'est pas une cornemuse
#'  
#' 
#' @rdname bagpipe
#' @export
":=" <- function(...){
  pf <- parent.frame()
  ml <- pipe_collector(match.call())
  sequence <- Reduce(jointcall, ml)
  mlend <- ml[[length(ml)]]
  if(mlend=="BR") return(browser())
  if(mlend=="PR") return(Reduce(jointcall, head(ml, -1)))
  if(mlend=="SE") return(head(ml,-1))
  eval(sequence, pf)
}

