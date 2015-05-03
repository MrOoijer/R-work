#' @title pretty_legend
#' @description put legend in upperleft corner of plot
#' @param kleur a vector with numbers of palet colours
#' @param ... arguments for legend function.
#' @return NA
#' @examples
#' # pretty_legend(kleur=c(1,2), legend=c("a", "b"))

pretty_legend <-function(
  kleur=c(1,2)
  , ...){
  if (hasArg(lwd)){
    legend("topleft" , inset=c(-0.01,-0.01), xpd=TRUE # moves - this is for pt 16 h=350
           , bty="n" 
           , horiz=TRUE
           , cex=0.8
           , text.col=cpalet$titel
           , col=cpalet$lijnkleur[kleur]
           , ...
    )
    
  } else {
    legend("topleft" , inset=c(-0.01,-0.01), xpd=TRUE # moves - this is for pt 16 h=350
           , bty="n" 
           , horiz=TRUE
           , cex=0.8
           , text.col=cpalet$titel
           , fill=cpalet$vulkleur[kleur]
           , border=cpalet$lijnkleur[kleur]
           , ...
    )
  }
}
