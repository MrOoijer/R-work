#' @title pretty_start
#' @description start writing plot output to a file
#' @param destfile output file name
#' @param sizex the horizontal size in px
#' @param sizey the vertical size in px
#' @param palet optional:palet to use
#' @return NA
#' @examples
#' # pretty_start("this.png", 500,500)
#' # pretty_end() # closes the file

pretty_start <-function(destfile, sizex, sizey, palet=1){
  if(palet >3) palet=3
  cpalet<<-list(cpalet1, cpalet2, cpalet3)[[palet]]
  png(filename = destfile,
      width = sizex, height = sizey, units = "px", pointsize = 16
      #,type = c("quartz")
  )
}

#' @title pretty_end
#' @description closes current plot output to a file


pretty_end <-function(){ dev.off()}
