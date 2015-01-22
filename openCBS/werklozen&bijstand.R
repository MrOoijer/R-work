source("open_feed_api_CBS.R")
source("../library/pretty.plot.R")
ww<-open_feed_api_CBS("37506wwm", new=FALSE)
bs<-open_feed_api_CBS("37789ksz", new=FALSE)
maand_bs<-bs[substr(bs$Perioden,5,6) == "MM",]

ww.m<-ww[ww$Geslacht=="0"
         & ww$Leeftijd=="0"
         & substr(ww$Perioden,5,6) == "MM"
         , c(24, 5)]
ww.ts<-ts(ww.m[,2]/1000, start=1998, freq=12)
ts.end<- end(ww.ts)[1] + (end(ww.ts)[2]-1)/12
bs.ts <- ts(maand_bs[,12], start=1998,freq=12)
bs.ts <- window(bs.ts, end=end(ww.ts))
all.ts<-ww.ts+bs.ts

jpeg(filename = sprintf("./afbeelding/w&b_%s.jpg", format(Sys.time(), "%Y_%b_%d")),
     width = 650, height = 450, units = "px", pointsize = 16,
     quality = 100,
     type = "windows")


pretty.plot(ts_to_df(bs.ts)[,1:2]
            , type="v"
            , kleur=4
            , lwd=2
            , ylim=c(150,900)
            , xlim=c(1998,ts.end)
            , xat = seq(1998,2014,2)
            , yat=c(150,300,450,600, 750, 900)
            , main=paste0("WW en bijstand vanaf 1998\nt/m ", tail(ww.m$Perioden_C,1))
            , source="Bron: Cbs Open Data Portal 2014"
            , ylab="duizenden"
            
            )
pretty.plot(ts_to_df(bs.ts)[,1:2], type="l", kleur=4, lwd=2, add=TRUE)
pretty.plot(ts_to_df(all.ts)[,1:2], type="v", kleur=3, lwd=2, add=TRUE)
pretty.plot(ts_to_df(all.ts)[,1:2], type="l", kleur=3, lwd=2, add=TRUE)
pretty.legend(kleur=c(3,4), lwd=3, 
              c("WW + bijstand (* 1000) ", "Bijstand (* 1000)")
)

junk <- dev.off()

