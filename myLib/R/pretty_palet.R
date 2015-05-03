#' @title pretty_palet
#' @description put a palet wrapper around a standard plot function
#' @param kleur a vector with numbers of palet colours
#' @param FUN the fucntion to call.
#' @return NA
#' @examples
#' # pretty_palet(kleur=1, FUN=lines, x, y)
#' # pretty_abline # shorthand for pretty_palet(FUN=abline)
#' # pretty_text # idem


pretty_palet <- function(FUN, kleur=0,...){
  kleur <-ifelse(kleur==0, cpalet$box, cpalet$lijnkleur[kleur])
  FUN(col=kleur, ...)
}

#' @title pretty_abline
#' @description put a palet wrapper around abline


pretty_abline <- function(...){
  pretty_palet(abline,...)
}

#' @title pretty_text
#' @description put a palet wrapper around text

pretty_text <- function(kleur=0, cex=0.8, ...){
  kleur <-ifelse(kleur==0, cpalet$titel, cpalet$lijnkleur[kleur])
  arguments<-list(...)
  arguments<-append(arguments, list(cex=cex, col=kleur))
  do.call(text, arguments)
}
