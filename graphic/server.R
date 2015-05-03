# server.R
source("helpers.R")

shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      data.keuze <- which (input$data.keuze == c("GISS", "NOAA",
                                                 "HADcrut", "JMA", "C&W", "Combined", "CRUTEM (land)", "GISS (land)"
                                                 , "RSS (satellite)", "UAH (satellite)", "BEST", "Hadcrut-3"
                                                 , "UAH (beta 6.0)"))
      start.year=input$start.year[1]
      end.year=input$start.year[2]
      x.lim=input$x.lim
      smooth.keuze=input$smooth.keuze
      smooth.keuze.b=input$smooth.keuze.b
      seas.keuze=input$cb02
      co2.lag=input$co2.lag
      y.lim=input$y.lim
      ssp.lag=-1
      if (input$cba004) ssp.lag=input$ssp.lag
      amo.lag= -1
      if (input$cba002) amo.lag=input$amo.lag
      ens.lag= -1
      if (input$cba001) ens.lag=input$ens.lag
      lod.lag=-1
      if (input$cba006)lod.lag=input$lod.lag
      vol.lag=-1
      if (input$cba005)vol.lag=input$vol.lag
      pdo.lag=-1
      if (input$cba003)pdo.lag=input$pdo.lag
      show.se=input$cb00b
      show.reg=input$cb00a
      if (show.reg == FALSE) show.se=FALSE
      model.keuze= which(input$trend == c("co2", "linear", "spline"))
      smooth.way=which(input$smoother == c("Golay", "Loess"))
      deseas.way=which(input$deseas == c("sine", "stl"))
      cel.keuze=as.numeric(input$boxID)
      doen(data.keuze=data.keuze
           , model.keuze=model.keuze
           , start.year=start.year
           , end.year= end.year
           , smooth.way=smooth.way
           , smooth.keuze=smooth.keuze
           , smooth.keuze.b=smooth.keuze.b
           , co2.lag=co2.lag
           , ssp.lag=ssp.lag
           , amo.lag=amo.lag
           , soi.lag=ens.lag
           , lod.lag=lod.lag
           , vol.lag=vol.lag
           , pdo.lag=pdo.lag
           , cel.keuze= cel.keuze
           , seas.keuze=seas.keuze
           , deseas.way=deseas.way
           , y.lim=y.lim
           , x.lim=x.lim
           , show.reg=show.reg
           , show.se=show.se
      )
    }, height = 533, width=800)
    output$map2<-renderPlot({
      data.keuze <- which (input$data.keuze2 == c("GISS", "NOAA",
                                                  "HADcrut", "JMA", "C&W", "Combined", "CRUTEM (land)", "GISS (land)"
                                                  , "RSS (satellite)", "UAH (satellite)", "BEST", "Hadcrut-3"
                                                  , "UAH (beta 6.0)"))
      smooth.keuze=input$smooth.keuze2
      x.lim=input$x.lim2
      y.lim=input$y.lim2
      
      doen(show.reg=FALSE #, plot.keuze=plot.keuze
           , x.lim=x.lim, y.lim=y.lim
           , show.se=FALSE
           , model.keuze=1
           , smooth.keuze=smooth.keuze, data.keuze=data.keuze)
    }, height = 400, width=640)
    output$map3<-renderPlot({
      model.keuze= which(input$trend2 == c("co2", "linear"))
      start.year=input$start.year2[1]
      end.year=input$start.year2[2]
      smooth.keuze.b=input$smooth.keuze.b2
      
      doen( start.year=start.year
            , end.year= end.year
            , smooth.keuze.b=smooth.keuze.b
            , smooth.keuze=smooth.keuze.b
            , model.keuze=model.keuze
            , co2.lag=0
            , ssp.lag=-1
            , amo.lag=-1
            , soi.lag=-1
            , lod.lag=-1
            , vol.lag=-1
            , pdo.lag=-1
            , seas.keuze=FALSE
            , show.se=TRUE
            
      )
    }, height = 400, width=640)

    output$bmap3<-renderPlot({
      co2.lag=ifelse(input$bcb3, 0,-1)
      if(co2.lag > -1) co2.lag=input$bco3
      start.year=input$bstart.year2[1]
      end.year=input$bstart.year2[2]
      smooth.keuze.b=input$bsmooth.keuze.b2
      show.se=input$bcb3b
      
      doen( start.year=start.year
            , end.year= end.year
            , smooth.keuze.b=smooth.keuze.b
            , smooth.keuze=smooth.keuze.b
            , co2.lag=co2.lag
            , ssp.lag=-1
            , amo.lag=-1
            , soi.lag=-1
            , lod.lag=-1
            , vol.lag=-1
            , pdo.lag=-1
            , cel.keuze= FALSE
            , seas.keuze=FALSE
            , show.se=show.se
            
      )
    }, height = 400, width=640)
    
    
    output$map4<-renderPlot({
      ssp.lag=ifelse(input$cb31, input$sl31, -1)
      vol.lag=ifelse(input$cb32, input$sl32, -1)
      lod.lag=ifelse(input$cb33, input$sl33, -1)
      doen( start.year=1850
            , end.year= 2014
            , smooth.keuze=0
            , co2.lag=0
            , ssp.lag=ssp.lag
            , amo.lag=-1
            , soi.lag=-1
            , lod.lag=lod.lag
            , vol.lag=vol.lag
            , pdo.lag=-1
            , seas.keuze=FALSE
            , show.se=TRUE
      )
      
    }, height = 400, width=640)
    
    output$map5<-renderPlot({
      soi.lag=ifelse(input$cb41, input$sl41, -1)
      amo.lag=ifelse(input$cb42, input$sl42, -1)
      pdo.lag=ifelse(input$cb43, input$sl43, -1)
      doen( start.year=1850
            , end.year= 2014
            , show.se=TRUE
            , smooth.keuze=0
            , co2.lag=0
            , amo.lag=amo.lag
            , soi.lag=soi.lag
            , pdo.lag=pdo.lag
            , cel.keuze= FALSE
            , seas.keuze=FALSE
      )
      
    }, height = 400, width=640)
    
    output$map6<-renderPlot({
      cel.keuze=as.numeric(input$boxID2)
      doen(  start.year=1850
            , end.year= 2014
            , smooth.keuze=0
            , co2.lag=0
            , ssp.lag=3
            , amo.lag=0
            , soi.lag=3
            , lod.lag=84
            , vol.lag=7
            , pdo.lag=-1
            , show.se=TRUE
            , cel.keuze= cel.keuze
            , seas.keuze=input$cb52
      )
      
    }, height = 400, width=640)
    
    output$map7<-renderPlot({
      x.lim=input$x.lim6
      y.lim=input$y.lim6
      ssp.lag=ifelse(input$cb61, input$sl61, -1)
      vol.lag=ifelse(input$cb62, input$sl62, -1)
      lod.lag=ifelse(input$cb63, input$sl63, -1)
      soi.lag=ifelse(input$cb64, input$sl64, -1)
      model.keuze= which(input$trend6 == c("co2", "linear"))
      
      data.keuze <- which (input$data.keuze6 == c("GISS", "NOAA",
                                                  "HADcrut", "JMA", "C&W", "Combined", "CRUTEM (land)", "GISS (land)"
                                                  , "RSS (satellite)", "UAH (satellite)", "BEST", "Hadcrut-3"
                                                  , "UAH (beta 6.0)"))
      
       doen(start.year=input$start.year6[1]
            , end.year= input$start.year6[2]          
            , model.keuze=model.keuze
            , data.keuze=data.keuze
            , smooth.keuze=input$smooth.keuze6
            , smooth.keuze.b=input$smooth.keuze.b6
            , co2.lag=0
            , ssp.lag=ssp.lag
            , soi.lag=soi.lag
            , lod.lag=lod.lag
            , vol.lag=vol.lag
            , amo.lag=-1
            , pdo.lag=-1
            , cel.keuze= FALSE
            , show.se=TRUE
            , seas.keuze=FALSE
            , y.lim=y.lim
            , x.lim=x.lim
      )
    }, height = 400, width=640)
    
      output$map.fc<-renderPlot({
        data.keuze <- which (input$data.keuze.fc == c("GISS", "NOAA",
                                                      "HADcrut", "JMA", "C&W", "Combined", "CRUTEM (land)", "GISS (land)"
                                                      , "RSS (satellite)", "UAH (satellite)", "BEST", "Hadcrut-3"
                                                      ,  "UAH (beta 6.0)"))
        
        doen( start.year=input$start.year.fc[1]
              , end.year= input$start.year.fc[2]
              , data.keuze=data.keuze
              , smooth.keuze=input$smooth.keuze.b.fc
              , smooth.keuze.b=input$smooth.keuze.b.fc
              , co2.lag=0
              , ssp.lag=3
              , soi.lag=3
              , lod.lag=78
              , vol.lag=8
              , amo.lag=-1
              , pdo.lag=-1
              , cel.keuze= c(1,2,3,4,5)
              , show.se=TRUE
              , seas.keuze=FALSE
              , y.lim=input$y.lim.fc
              , x.lim=input$x.lim.fc
              , predict = TRUE
        )     

    }, height = 533, width=800)
    
    
    output$Sources<-renderTable(source_table)
    
    output$down <-downloadHandler(
      
      filename= function(){
        f.name <- "thisplot.png"
        if (input$fname != "") f.name=paste0(input$fname, ".png")
        return(f.name)
      }
      , content= function(file){
        w.default=800
        h.default=533
        if (input$size006=="450 x 300"){w.default=450; h.default=300}
        if (input$size006=="1200 x 800"){w.default=1200; h.default=800}
        png(file,width = w.default,height = h.default)      
        
        # plotting function as main
        data.keuze <- which (input$data.keuze == c("GISS", "NOAA",
                                                    "HADcrut", "JMA", "C&W", "Combined", "CRUTEM (land)", "GISS (land)"
                                                    , "RSS (satellite)", "UAH (satellite)", "BEST", "Hadcrut-3"
                                                    ,  "UAH (beta 6.0)"))
        start.year=input$start.year[1]
        end.year=input$start.year[2]
        x.lim=input$x.lim
        smooth.keuze=input$smooth.keuze
        smooth.keuze.b=input$smooth.keuze.b
        seas.keuze=input$cb02
        co2.lag=input$co2.lag
        y.lim=input$y.lim
        ssp.lag=-1
        if (input$cba004) ssp.lag=input$ssp.lag
        amo.lag= -1
        if (input$cba002) amo.lag=input$amo.lag
        ens.lag= -1
        if (input$cba001) ens.lag=input$ens.lag
        lod.lag=-1
        if (input$cba006)lod.lag=input$lod.lag
        vol.lag=-1
        if (input$cba005)vol.lag=input$vol.lag
        pdo.lag=-1
        if (input$cba003)pdo.lag=input$pdo.lag
        show.se=input$cb00b
        show.reg=input$cb00a
        if (show.reg == FALSE) show.se=FALSE
        model.keuze= which(input$trend == c("co2", "linear", "spline"))
        smooth.way=which(input$smoother == c("Golay", "Loess"))
        deseas.way=which(input$deseas == c("sine", "stl"))
        cel.keuze=as.numeric(input$boxID)
        doen(data.keuze=data.keuze
             , model.keuze=model.keuze
             , start.year=start.year
             , end.year= end.year
             , smooth.way=smooth.way
             , smooth.keuze=smooth.keuze
             , smooth.keuze.b=smooth.keuze.b
             , co2.lag=co2.lag
             , ssp.lag=ssp.lag
             , amo.lag=amo.lag
             , soi.lag=ens.lag
             , lod.lag=lod.lag
             , vol.lag=vol.lag
             , pdo.lag=pdo.lag
             , cel.keuze= cel.keuze
             , seas.keuze=seas.keuze
             , deseas.way=deseas.way
             , y.lim=y.lim
             , x.lim=x.lim
             , show.reg=show.reg
             , show.se=show.se
        )
        
        
        dev.off()
        
      } 
    )
    output$down2 <-downloadHandler(
      
      filename= function(){
        f.name <- "thisforecastplot.png"
        return(f.name)
      }
      , content= function(file){
        w.default=800
        h.default=533
        #if (input$size006=="450 x 300"){w.default=450; h.default=300}
        png(file,width = w.default,height = h.default)      
        data.keuze <- which (input$data.keuze.fc == c("GISS", "NOAA",
                                                      "HADcrut", "JMA", "C&W", "Combined", "CRUTEM (land)", "GISS (land)"
                                                      , "RSS (satellite)", "UAH (satellite)", "BEST", "Hadcrut-3"
                                                      ,  "UAH (beta 6.0)") )       
        doen( start.year=input$start.year.fc[1]
              , end.year= input$start.year.fc[2]
              , data.keuze=data.keuze
              , smooth.keuze=input$smooth.keuze.b.fc
              , smooth.keuze.b=input$smooth.keuze.b.fc
              , co2.lag=0
              , ssp.lag=3
              , soi.lag=3
              , lod.lag=78
              , vol.lag=8
              , amo.lag=-1
              , pdo.lag=-1
              , cel.keuze= c(1,2,3,4,5)
              , show.se=TRUE
              , seas.keuze=FALSE
              , y.lim=input$y.lim.fc
              , x.lim=input$x.lim.fc
              , predict = TRUE
        )     
        dev.off()
      }
    )
  }
)