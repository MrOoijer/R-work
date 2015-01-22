source("open_feed_api_CBS.R")
source("../library/pretty.plot.R")
cpalet=cpalet2
testVlag=T
perVlag="periodic"
tVlag=4

werkloosheid_tabel<-open_feed_api_CBS("80479ned", new=F)
maand_werkloosheid_tabel<-werkloosheid_tabel[substr(werkloosheid_tabel$Perioden,5,6) == "MM",]


# ----- werkloosheid jong vs oud relatief --------


werk_jeugd<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 25 jaar"  ,c(25,18) ]

p.lengte=dim(werk_jeugd)[1]-1

a<-ts(werk_jeugd[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_jeugd<-b[,2]

if(!testVlag) pretty.start(sprintf("./afbeelding/werkloos_%s.png", format(Sys.time(), "%Y_%b_%d")),
     650, 450, palet=2) else cpalet<-cpalet2


pretty.plot(a, kleur=4, ylab="%"
            , main="Werkloosheid jong vs oud\nNederlandse definitie - trend", ylim=c(0,18)
            , source="Bron: Cbs Open Data Portal 2014")
pretty.plot(trend_jeugd, add=TRUE, kleur=1, lwd=3)

werk_oud<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
    maand_werkloosheid_tabel$Leeftijd_C == "45 tot 65 jaar"  ,c(25,18) ]

a<-ts(werk_oud[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]

pretty.plot(a, add=TRUE, kleur=4)
pretty.plot(trend_tot, add=TRUE, kleur=2, lwd=3)
pretty.legend(kleur=c(1, 2), lwd=3, 
              c("15-25 jaar", "45-65 jaar")
)

if(!testVlag) pretty.end()

# ----- werkloosheid allen vs beroepsbevolking ---------------


if(!testVlag) pretty.start(sprintf("./afbeelding/werkloos_abs_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=2) else cpalet<-cpalet2


werk_oud<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 65 jaar"  ,c(25,15) ]

a<-ts(werk_oud[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]

pretty.plot(a, kleur=4, ylab="aantal * 1000"
            , main="Werkloosheid absoluut van allen \nAfgezet tegen beroepsbevolking - 7000"
            , ylim=c(200,1000)
            , source="Bron: Cbs Open Data Portal 2014")
pretty.plot(trend_tot, add=TRUE, kleur=2, lwd=3)

#beroepsbevolking
werk_zaam<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 65 jaar"  ,c(25,5) ]
a<-ts(werk_zaam[,2], start=2003, freq=12)-7000
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_zaam<-b[,2]

pretty.plot(a, add=TRUE, kleur=4)
pretty.plot(trend_zaam, add=TRUE, kleur=4, lwd=3)

pretty.legend(kleur=c(4, 2), lwd=3, 
              c( "beroepsbevolking", "totaal werkloos"))
              
if(!testVlag) pretty.end()
# he, is dat nou werkzaam of totaal beropesbevolking?

# ----- werkloosheid jeugd vs beroepsbevolking ---------------

werk_jeugd<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 25 jaar"  ,c(25,15) ]

a<-ts(werk_jeugd[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_jeugd<-b[,2]

if(!testVlag) pretty.start(sprintf("./afbeelding/werkloos_abs_jeugd_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=2) else cpalet<-cpalet2


pretty.plot(a, kleur=4, ylab="aantal * 1000"
            , main="Werkloosheid absoluut van jeugd\nAfgezet tegen beroepsbevolking - 600"
            , ylim=c(0,400)
            , source="Bron: Cbs Open Data Portal 2014")
pretty.plot(trend_jeugd, add=TRUE, kleur=2, lwd=3)

#beroepsbevolking
werk_zaam<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 25 jaar"  ,c(25,5) ]
a<-ts(werk_zaam[,2], start=2003, freq=12)-600
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_zaam<-b[,2]

pretty.plot(a, add=TRUE, kleur=4)
pretty.plot(trend_zaam, add=TRUE, kleur=4, lwd=3)

pretty.legend(kleur=c(4, 2), lwd=3, 
              c( "beroepsbevolking", "totaal werkloos"))

if(!testVlag) pretty.end()

# -----------------------------------------

werk_vrouw<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Vrouwen" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 25 jaar"  ,c(25,18) ]
werk_man<-maand_werkloosheid_tabel[
  maand_werkloosheid_tabel$Geslacht_C == "Mannen" &
    maand_werkloosheid_tabel$Leeftijd_C == "15 tot 25 jaar"  ,c(25,18) ]
a<-ts(werk_vrouw[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]

if(!testVlag) pretty.start(sprintf("./afbeelding/werkloos_sexe_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=2) else cpalet<-cpalet2


pretty.plot(a, kleur=4, ylab="%"
            , main="Werkloosheid jongeren \n sexe"
            , ylim=c(0,20)
            , source="Bron: Cbs Open Data Portal 2014")
pretty.plot(trend_tot, add=TRUE, kleur=1, lwd=3)

a<-ts(werk_man[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]

pretty.plot(a, kleur=4, add=TRUE)
pretty.plot(trend_tot, add=TRUE, kleur=2, lwd=3)

pretty.legend(kleur=c(1, 2), lwd=3, 
              c( "Vrouw", "Man"))

if(!testVlag) pretty.end()

# ................... beroepsbevolking opbouw ...........

temp<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal",]
temp2<-data.frame(Periode=unique(temp$Perioden_C) 
                  , Jong=temp$TotaalBeroepsbevolking_1[temp$Leeftijd_C == "15 tot 25 jaar"]
                  , Midden= temp$TotaalBeroepsbevolking_1[temp$Leeftijd_C == "25 tot 45 jaar"]
                  , Oud = temp$TotaalBeroepsbevolking_1[temp$Leeftijd_C == "45 tot 65 jaar"]) 
temp2$Periode<-2003+(0:p.lengte/12)
temp2[,3]<-temp2[,2]+temp2[,3]
temp2[,4]<-temp2[,4]+temp2[,3]
a<-ts(temp2[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
temp2[,2]<-b[,2]
a<-ts(temp2[,3], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
temp2[,3]<-b[,2]
a<-ts(temp2[,4], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
temp2[,4]<-b[,2]


if(!testVlag) pretty.start(sprintf("./afbeelding/beroep_leeftijd_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=3) else cpalet<-cpalet2


pretty.plot(temp2[,c(1,2)], kleur=2, lwd=3, ylab=""
            , main="Beroepsbevolking \nAantal * 1000 per leeftijdsgroep"
            , ylim=c(0,8500)
            , source="Bron: Cbs Open Data Portal 2014")
pretty.plot(temp2[,c(1,3)], add=TRUE, kleur=3, lwd=3)
pretty.plot(temp2[,c(1,4)], add=TRUE, kleur=4, lwd=3)
pretty.plot(temp2[,c(1,2)], add=TRUE, kleur=2, lwd=3, type="v")
pretty.plot(temp2[,c(1,3)], add=TRUE, kleur=3, lwd=3, type="v")
pretty.plot(temp2[,c(1,4)], add=TRUE, kleur=4, lwd=3, type="v")
pretty.legend(kleur=c(2, 3, 4), lwd=3, 
              c( "15-25", "15-45", "15-65"))

if(!testVlag) pretty.end()

# ................... beroepsbevolking opbouw 2...........

temp<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal",]
temp2<-data.frame(Periode=unique(temp$Perioden_C) 
                  , Jong=temp$TotaalBeroepsbevolking_1[temp$Leeftijd_C == "15 tot 25 jaar"]
                  , Midden= temp$TotaalBeroepsbevolking_1[temp$Leeftijd_C == "25 tot 45 jaar"]
                  , Oud = temp$TotaalBeroepsbevolking_1[temp$Leeftijd_C == "45 tot 65 jaar"]) 
temp2$Periode<-2003+(0:p.lengte/12)
a<-ts(temp2[,2], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
temp2[,2]<-b[,2]*100/b[1,2]
a<-ts(temp2[,3], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
temp2[,3]<-b[,2]*100/b[1,2]
a<-ts(temp2[,4], start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
temp2[,4]<-b[,2]*100/b[1,2]


if(!testVlag) pretty.start(sprintf("./afbeelding/beroep_leeftijd2_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=3) else cpalet<-cpalet2


pretty.plot(temp2[,c(1,2)], kleur=2, lwd=3, ylab="%"
            , main="Groei Beroepsbevolking \nPer leeftijdsgroep (2003 = 100%)"
            , ylim=c(80,170)
            , source="Bron: Cbs Open Data Portal 2014")
pretty.plot(temp2[,c(1,3)], add=TRUE, kleur=3, lwd=3)
pretty.plot(temp2[,c(1,4)], add=TRUE, kleur=4, lwd=3)
pretty.legend(kleur=c(2, 3, 4), lwd=3, 
              c( "15-25", "25-45", "45-65"))

if(!testVlag) pretty.end()

# ---------------------------
if(!testVlag) pretty.start(sprintf("./afbeelding/extra_1_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=3) else cpalet<-cpalet2

tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "15 tot 25 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(type="l", data.frame(tmp1$ym, 100*tmp1$beroeps/tmp1$totaal), kleur=5
            , main="Arbeidsparticipatie allen"
            , xlab="jaar", ylab="procent", ylim=c(25, 90))
pretty.plot(add=T, 100*trend_tot, kleur=1, lwd=2)
#-------------------
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "15 tot 65 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(add=T, 100*trend_tot, kleur=2, lwd=4)
# -----------
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "25 tot 45 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(add=T, 100*trend_tot, kleur=3, lwd=2)
# -----------
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "45 tot 65 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(add=T, 100*trend_tot, kleur=4, lwd=2)
# -----------
pretty.legend(kleur=c(1,2,3,4), lwd=c(2,4,2,2)
              , legend=c("15 tot 25 jaar", "15 tot 65 jaar", "25 tot 45 jaar","45 tot 65 jaar"))
if(!testVlag) pretty.end()

# -----------------------------------
if(!testVlag) pretty.start(sprintf("./afbeelding/extra_2_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=3) else cpalet<-cpalet2
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "15 tot 25 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(type="l", data.frame(tmp1$ym, 100*tmp1$beroeps/tmp1$totaal), kleur=5
            , main="Arbeidsparticipatie ten opzichte van 2003 (100%)"
            , xlab="jaar", ylab="procent", ylim=c(50,150))
pretty.plot(add=T, 100*trend_tot/trend_tot[1], kleur=1, lwd=2)
#-------------------
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "15 tot 65 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(add=T, 100*trend_tot/trend_tot[1], kleur=2, lwd=4)
# -----------
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "25 tot 45 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(add=T, 100*trend_tot/trend_tot[1], kleur=3, lwd=2)
# -----------
tmp0<-maand_werkloosheid_tabel[maand_werkloosheid_tabel$Geslacht_C == "Totaal"
                               & maand_werkloosheid_tabel$Leeftijd_C== "45 tot 65 jaar"
                               ,]
tmp0$Periode<-2003+(0:p.lengte/12)
tmp1<-data.frame(ym=tmp0$Periode, totaal=tmp0$TotaalNietBeroepsbevolking_16 +
                   tmp0$TotaalBeroepsbevolking_1, beroeps=tmp0$TotaalBeroepsbevolking_1)


a<-ts(tmp1$beroeps/tmp1$totaal, start=2003, freq=12)
b<-stl(a, s.window=perVlag, t.window=tVlag)
b<-b$time.series
trend_tot<-b[,2]
pretty.plot(add=T, 100*trend_tot/trend_tot[1], kleur=4, lwd=2)
# -----------
pretty.legend(kleur=c(1,2,3,4), lwd=c(2,4,2,2)
              , legend=c("15 tot 25 jaar", "15 tot 65 jaar", "25 tot 45 jaar","45 tot 65 jaar"))

if(!testVlag) pretty.end()
# -----------
if(!testVlag) pretty.start(sprintf("./afbeelding/extra_3_%s.png", format(Sys.time(), "%Y_%b_%d")),
                           650, 450, palet=2) else cpalet<-cpalet2

werk<-maand_werkloosheid_tabel[
 maand_werkloosheid_tabel$Geslacht_C == "Totaal" &
 maand_werkloosheid_tabel$Leeftijd_C == "15 tot 65 jaar"  , ]
beroeps<-ts(werk[,5], start=2003, freq=12)
niet_beroeps<-ts(werk[,20], start=2003, freq=12)
werkloos<-ts(werk[,15], start=2003, freq=12)

pretty.plot(beroeps-beroeps[1], kleur=5, ylim=c(-100,800)# yat=c(7000,9000)
            ,main="Beroepsbevolking: actief vs. werkloos\n in absolute aantallen  * 1000"
            , xlab="jaar", ylab=" "
            )
a<-stl(beroeps-beroeps[1], s.window=perVlag, t.window=tVlag)$time.series[,2]
pretty.plot(a-a[1]
            , lwd=3, kleur=1, add=TRUE)
pretty.plot(werkloos-werkloos[1], lwd=1, kleur=4, add=T)
a<-stl(werkloos-werkloos[1], s.window=perVlag, t.window=tVlag)$time.series[,2]
w.rem=a[1]+werkloos[1]
pretty.plot(a-a[1], lwd=3, kleur=2, add=T)
a<-beroeps+niet_beroeps-(beroeps[1]+niet_beroeps[1])
pretty.plot(a+600, lwd=3, kleur=4, add=T)
pretty.abline(kleur=4, v=2013.33)
pretty.legend(kleur=c(1,2,4), lwd=3, legend=c("beroepsbevolking", "werkloze bevolking", "totaal 15-65 jaar")
)
pretty.text(kleur=4, x=2003.1, y=580, font=4, adj=0, labels="totaal 15-65 in 2003: 10.891")
pretty.text(kleur=1, x=2003.8, y=40, font=4, adj=0, labels="beroepsbevolking in 2003: 7.252")
pretty.text(kleur=2, x=2003.1, y=-20, font=4, adj=0, labels="werkloos in 2003: 326(.000)")
if(!testVlag) pretty.end()