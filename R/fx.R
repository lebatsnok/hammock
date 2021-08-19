fx <- function(exp){
  ca <- call("function", as.pairlist(alist(x=)), substitute(exp))
  eval(ca, parent.frame())
}
