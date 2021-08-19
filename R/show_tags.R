#' Show tags of a Df in lower-right part of Rstudio screen 
#'
#' Table of tags
#' 
#'
#' @param x a Df
#' @param cols Column numbers to show
#' @param NW width of leftmost column
#' @param filename file name
#' @param showit TRUE or FALSE
#' @return table
#' 
#' @export
show_tags <- function(x, cols = NULL, NW = "30%", TW = "100%", filename=NULL, showit=TRUE) {
  varname <- deparse(substitute(x))
  LEG <- info(x)
  if(is.null(cols)) cols <- c(1, NCOL(x))
  BGN <- paste("<!DOCTYPE html>
               <html>
               <head>
               <title></title>
               <meta charset=\"UTF-8\">
               <style type=\"text/css\">
               td { font: 14px Arial,sans-serif}
               </style>
               </head>
               <body>
               <h3><a href='javascript:window.history.go(-1)'><</a>|<a href='javascript:window.history.go(1)'>></a>", varname, ": ", LEG, ", cols", cols[1], "-", cols[2], ("</h3>\n"))
  
  END <- "
  </body>
  </html>
  "
  n <- names(x)
  t <- unlist(tags(x))
  res <- data.frame(Name=n, Label=t)
  
  if(showit){
    tab <- paste0("<table width='", TW, "', border=1 cellspacing=0 cellpadding=2 style='table-layout: fixed; word-break: break-all;'>")
    tab <- c(tab, "<tr><td width=25 bgcolor='gray'></td>")
    tab <- c(tab, paste0(" <td width=", NW, " bgcolor='gray'><b>", names(res)[1],"</b></td>" ) )
    for(jjj in names(res)[-1]) tab <- c(tab, paste0(" <td bgcolor='gray'><b>", jjj,"</b></td>" ) )
    tab <- c(tab, "</tr>")
                                    
    for(iii in cols[1]:cols[2]){
      tab <- c(tab, paste0("  <tr><td><span style='font: 10px sans-serif'>",iii,"</span></td>"))
      for(jjj in 1:NCOL(res))
        tab <- c(tab, paste0("     <td>", res[iii,jjj], "</td>" ))
      tab <- c(tab, "  </tr>")
    }    
    tab <- c(tab, "</table>")
    temp <- if(is.null(filename)) tempfile(fileext=".html") else filename
    LINES <- c(BGN, tab, END)
    conn <- file(temp)
    writeLines(LINES, conn)
    close(conn)
    #browser()
    rstudio::viewer(temp)
    invisible(res)
  } else res
}