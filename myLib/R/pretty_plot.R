#' @title pretty_plot
#' @description put a palet wrapper around the standard plot (function) with extra's
#' @param df a 2 column data frame or a time series
#' @param kleur the palet colour
#' @param ccloc the location of the Creative Common License within the plot
#' @param add if TRUE, add content to existing plot, otherwise produce a new one
#' @param source small note in lower right bottom for source of data
#' @param palet the palet to choose (1-4)
#' @param type extension of styandard plot type. "e" is empty, "f"is a fanplot, "v" is an area below the line plot, "a" is a polygon with lower side in df[,3]
#' @return NA
#' @examples
#' # pretty_plot(hc4, type="f")

pretty_plot<-function(df
                      , xlim=NULL
                      , ylim=NULL
                      , type="l" 
                      , main=""
                      , xlab=""
                      , ylab=""
                      , lwd=1
                      , lty=1
                      , cex=0.5
                      , kleur=1
                      , transparent=FALSE
                      , ccloc=1
                      , pch=19
                      , add=FALSE
                      , source=NULL
                      , xat = NULL, yat=NULL
                      , mai=NULL
                      , palet=2
                      , ...
                      
){if( ! exists("cpalet")) pretty_init(palet)
  if(class(df) %in% c("ts", "zoo", "xts"))df<-ts_to_df(df)
  if(add==FALSE){
    if(is.null(mai)){
      a = 0.6
      b = 0.55
      if (xlab == "") a = 0.35
      if (ylab == "") b = 0.45
      par(mai= c(a, b, 0.5, 0.2), bg=cpalet$rand)
    } else {par(mai= mai, bg=cpalet$rand)}
    a <- xlim
    if (is.null(xlim)){
      # eps<-(max(df[,1])-min(df[,1]))/100
      a<-c(min(df[,1]), max(df[,1]))
    }
    b <- ylim
    if (is.null(ylim)){
      eps<-(max(df[,2])-min(df[,2]))/100
      b<-c(min(df[,2])-eps, max(df[,2])+eps)
    }
    plot.new()
    plot.window( xlim=a
                 , ylim=b
                 , xaxs= "i"
                 , yaxs= "i" 
                 , main= ""
                 , xlab=""
                 , ylab=""
                 , bg=cpalet$binnen
                 , ...
    )
    #no box(), but
    r= par("usr")
    rect(r[1], r[3], r[2], r[4], col=cpalet$binnen, border=cpalet$box)
    
    axis(1
         , xaxs= "i"
         , tck = -0.02
         , line = 0
         , labels = NA # no labels yet
         , col= cpalet$box
         , at=xat
         
         
    )
    axis(side = 1
         , lwd = 0
         , line = -.7
         , cex.axis= 0.8 
         , adj = 0
         , col.axis= cpalet$as
         , at=xat
         
    )
    axis(2
         , yaxs= "i"
         , tck = -0.02
         , line = 0
         , labels = NA # no labels yet
         , col=cpalet$box
         , at=yat
    )
    axis(side = 2
         , las =1
         , lwd = 0
         , line = -.85
         , cex.axis= 0.8 
         , adj = 0
         , col.axis=cpalet$as
         , at=yat
    )
    
    title(main=main 
          , adj=0
          , cex.main= 0.95
          , font=1 
          , col.main=cpalet$titel
    )
    
    title(xlab=xlab
          , adj=0.5
          , col.lab=cpalet$as
          , cex.lab= 0.8  
          , line=1.2
    )
    
    title( ylab=ylab
           , adj=0.5
           , col.lab=cpalet$as
           , cex.lab= 0.8  
           , line=1.2
    )
    if (is.null(yat)) yat=axTicks(2)
    for (i in yat) abline(h=i, col=cpalet$box)
    pretty_cc(ccloc=ccloc)  
    if (! is.null(source)) 
      mtext(source, col=cpalet$as
                  , side=1, line=1.2, adj=0.98, cex=0.65)
    
  }
  if (type == "e") return
  if (type == "p" | type == "h") {
    t_col= cpalet$lijnkleur[kleur]
    if (transparent == TRUE) t_col= cpalet$vulkleur[kleur]
    points(df[,c(1,2)]
           , pch=pch
           , cex=cex
           , lwd=lwd
           , type=type
           , col=t_col)
  }
  if (type == "f") fan_plot(df
                            , lwd = lwd
                            , kleur = kleur
                            , pch = pch
                            , cex = cex)
  if (type=="v"){
    r= par("usr") # for minimum y
    r2=tail(df[,1],1)
    r1=head(df[,1], 1)
    cu<-rbind(as.matrix(df), c(r2, r[3]))
    cu<-rbind(cu, c(r1,r[3]))
    polygon(cu, col=cpalet$vulkleur[kleur], border=cpalet$vulkleur[kleur])
    box(col=cpalet$box)  
  }
  if (type == 'a'){
    # area polygon when dim(df)[2]>2
    # print(dim(df))
    polygon(c(df[,1],rev(df[,1])),c(df[,2],rev(df[,3])), col=cpalet$vulkleur[kleur], border=cpalet$vulkleur[kleur])
    box(col=cpalet$box)  
  }
  
  if (add==TRUE){
    if (type == "l" | type == "s") lines(df, lwd=lwd, col=cpalet$lijnkleur[kleur], type=type, lty=lty)
  }
  else{
    if (lwd > 0 & (type == "l" | type == "s")) lines(df, lwd=lwd, col=cpalet$lijnkleur[kleur], type=type, lty=lty) 
  }  
}
