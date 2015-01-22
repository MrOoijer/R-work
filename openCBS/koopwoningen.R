source("open_feed_api_CBS.R")
source("../library/pretty.plot.R")
library(forecast)
testVlag=F

cpalet=cpalet2

koopwoningen_tabel<-open_feed_api_CBS("81884NED", new=T)
maand_koopwoningen_tabel<-koopwoningen_tabel[substr(koopwoningen_tabel$Perioden,5,6) == "MM",]

b<-ts(maand_koopwoningen_tabel$PrijsindexBestaandeKoopwoningen_1, start=1995, frequency=12)
c<-ts(maand_koopwoningen_tabel$AantalVerkochteWoningen_4, start=1995, frequency=12)

hvm<-stl(c, s.window="periodic", t.window=5)
trend.hvm<-hvm$time.series
trend.hvm<-trend.hvm[,2]

p=end(c)[1]+end(c)[2]/12

# plot

if(!testVlag) pretty.start(sprintf("./afbeelding/woningen_01_%s.png", format(Sys.time(), "%Y_%b_%d")),
    650, 450, palet=2)


pretty.plot(ts_to_df(b)
            , xlim=c(1994.95,p)
            , xat=c(1995,2000,2005,2010,2014)
            , ylim=c(30,120)
            , kleur=2
            , lwd=3
            , type="l"
            , main=paste0("Huizen prijsindex (2010 = 100 proc.); maandelijks\nt/m ", 
                          tail(maand_koopwoningen_tabel$Perioden_C, 1))
            , source="Bron: Cbs Open Data Portal 2014")

# ----------------------------------------------------------------
if(!testVlag) pretty.end()
if(!testVlag) pretty.start(sprintf("./afbeelding/woningen_02_%s.jpg", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=2)

pretty.plot(ts_to_df(c)
            , xlim=c(2008,2017)
            , xat=c(1995,2000,2005,2008,2009,2010,2011,2012,2013,2014,2016)
            , ylab=" "
            , kleur=5
            , lwd=3
            , type="l"
            , ccloc=2
            , main= paste0("Huizenverkoop maandelijks aantal \nt/m ",
                           tail(maand_koopwoningen_tabel$Perioden_C, 1)) 
            , source="Bron: Cbs Open Data Portal 2014")

pretty.plot(ts_to_df(trend.hvm)
            , add=TRUE
            , kleur = 2
            , lwd=3)
aa<-forecast(c, h=24)
pretty.plot(ts_to_df(aa$mean), lwd=3, kleur=4, add=TRUE)
pretty.legend(kleur=c(5,4,2), lwd=3, 
              c("Maandelijkse\nVerkoop", "Forecast\nVan verkoop", "Vekooptrend\nseizoensgecorr.")
)





if(!testVlag) pretty.end()

