pretty.init<-function(pal.nr=3){
  
  cpalet1= list( 
    rand= rgb(0.05,0.05,0.05)
    , titel=rgb(1,0.8,0)
    , as= rgb(0.5,0.5,0.5)
    , box= rgb(0.3,0.3,0.3)
    , binnen= rgb(0.2,0.2,0.2)
    , lijnkleur=c("red", "blue", "white", "pink2", "pink4")
    , vulkleur= c(rgb(1,0,0,0.15)
                  ,rgb(0,0,1,0.2)
                  ,rgb(1,1,1,0.3)
                  ,rgb(165,42,42,40, max=255))
  )
  
  cpalet2= list( 
    rand="#F3F3F3"
    , titel="#222277"
    , as="#777777"
    , box= "#CCCCCC"
    , binnen="white"
    , lijnkleur=c("red", "blue", "brown", "darkgrey", "lightgrey")
    , vulkleur= c(rgb(1,0,0,0.15)
                  ,rgb(0,0,1,0.2)
                  ,rgb(165,42,42,40, max=255)
                  ,rgb(0.5,0.5,0.5, 0.2)
                  ,rgb(0.5,0.5,0.5, 0.1))
  )
  
  cpalet3= list( 
    rand="#FDDBC7"
    , titel="#67001F"
    , as="#4D4D4D"
    , box= "#BABABA"
    , binnen="#FFFFFF"
    , lijnkleur=c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7")
    , vulkleur= c("#67001F28"
                  , "#B2182B28"
                  , "#D6604D28"
                  , "#F4A58228"
                  , "#FDDBC728")
  )
  
  cpalet4= list( 
    rand="#cad4e8"
    , titel="#8b0000"
    , as="#d84467"
    , box= "#ffccdc"
    , binnen="#fffafa"
    , lijnkleur=c('#8b0000','#b71c39','#d84467','#f07092','#ff9cb8','#ffccdc',
                  '#fffafa',
                  '#cad4e8','#a5abd6','#8481c3','#6359b1','#40319e','#00008b')
    , vulkleur= c('#8b000028','#b71c3928','#d8446728','#f0709228','#ff9cb828','#ffccdc28',
                  '#fffafa28',
                  '#cad4e828','#a5abd628','#8481c328','#6359b128','#40319e28','#00008b28')
  )
  
  if (exists("cpalet")) remove(cpalet, pos=".GlobalEnv")
  if (pal.nr %in% c(1,2,3,4)) cpalet<<- list(cpalet1, cpalet2, cpalet3, cpalet4)[[pal.nr]]
}

pretty.legend <-function(
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

pretty.plot<-function(df
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
                      
){if( ! exists("cpalet")) pretty.init(palet)
  if(class(df) %in% c("ts", "zoo", "xts"))df<-ts.to.df(df)
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
    pretty.cc(ccloc=ccloc)  
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
  if (type == "f") fan.plot(df
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

fan.plot <- function(df
                     , lwd=1
                     , cex=0.4
                     , kleur=1
                     , pch=19
){
  d1<-dim(df)[1]
  d2<-dim(df)[2]
  a<-rep(mean(df[,2]), d1)
  if (d2 >2 ) a<-df[,3]
  pretty.plot(cbind(df[,1], a),kleur=cpalet$lijnkleur[kleur], lwd=lwd, type="l", add=TRUE)
  for (i in 1:d1){
    c = rgb(1,0,0,0.5)
    if (df[i,2]<a[i]) c = rgb(0,0,1,0.5)
    if(cex >0.1) points(df[i,c(1,2)], col=c, pch=pch, cex=cex)
    lines(rbind(c(df[i,1],df[i,2]),c(df[i,1],a[i]))
          , col=c
          , lwd=lwd)
  }
}

pretty.palet <- function(FUN, kleur=0,...){
  kleur <-ifelse(kleur==0, cpalet$box, cpalet$lijnkleur[kleur])
  FUN(col=kleur, ...)
  
}

pretty.abline <- function(...){
  pretty.palet(abline,...)
}

pretty.text <- function(kleur=0, cex=0.8, ...){
  kleur <-ifelse(kleur==0, cpalet$titel, cpalet$lijnkleur[kleur])
  arguments<-list(...)
  arguments<-append(arguments, list(cex=cex, col=kleur))
  do.call(text, arguments)
}

pretty.cc <- function(ccloc=0){
  if (ccloc %in% c(1,3)){
    loc= ccloc
    l = -1.6
    a = 0.95
  } else {
    loc= ccloc-1
    l = -1.6
    a = 0.05
  }
  if (ccloc == 0) return
  mtext("CC Jan van Rongen 2015"
        , col=cpalet$as
        , side=loc, line=l, adj=a, cex=0.65)
}

pretty.start <-function(destfile, sizex, sizey, palet=1){
  if(palet >3) palet=3
  cpalet<<-list(cpalet1, cpalet2, cpalet3)[[palet]]
  png(filename = destfile,
      width = sizex, height = sizey, units = "px", pointsize = 16
      #,type = c("quartz")
  )
}
pretty.end <-function(){ dev.off()}

pretty.density <- function(data
              , xlab="value"
              , ylab=""
              , main="Density plot with 95% confidence interval"
              , kleur = 1
              , ccloc = 0
              , mid = NA
              , ...){
  dens=density(data, bw="SJ")
  if (is.na(mid)) mid=mean(data)
  p=round(c(quantile(data, prob=c(0.025, 0.975)), mid),2)
  pretty.plot(data.frame(dens$x, dens$y), main=main, xlab=xlab, ylab=ylab, kleur=kleur, ccloc=ccloc, xat=p, ...)
  pretty.abline(kleur=kleur, lty=2, v=p)
}

ts.to.df <- function(x){
  z<- data.frame(time(x), as.numeric(x))
  names(z)<-c("Jaar", deparse(substitute(x)) )
  return(z)
}
