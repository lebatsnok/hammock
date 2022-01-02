#' Chain
#' 
#' chain (as in https://github.com/jkrumbiegel/Chain.jl)
#'
#' @param x an object (e.g data frame)
#' @param expr an expression
#'
#' @rdname chain
#' @return
#' @export
"%>>%" <- function(x, expr){
  ..x <- x
  se <- substitute(expr)
  le <- length(se)
  fu <- function(...) {}
  if(is.name(se)){
    return(eval(call(deparse(se), x)))
  }
  start <- if(se[[1]] == as.name("{")) 2 else 1
  if(le > 1) for(iii in start:le){
    fg <- all.names(se[[iii]])
    if("." %in% fg){
      # tricky quote/substitute + double-eval (is there a simpler way to do it?)
      bang <- quote(substitute(NA, list(.=as.name("..x"))))
      bang[[2]] <- se[[iii]]
      ..x <- eval(eval(bang))
      next
    }
    if(is.name(se[[iii]])){
      ..x <- eval(call(deparse(se[[iii]]), ..x))
      next
    }
    if(se[[iii]][[1]] == as.name("!") && se[[iii]][[2]][[1]] == as.name("(")){
      eval(se[[iii]][[2]][[2]])
      next
    }
    if(se[[iii]][[1]] == as.name("<-")){
      eval(se[[iii]])
      next
    }
    
    if(is.call(se[[iii]])){
      sel <- as.list(se[[iii]])
      sel[[1]] <- quote(..x)
      ..x <- do.call(deparse(se[[iii]][[1]]), sel)
    }
  }
..x
}


