# R script for the Paper by Braun et al. 2024 published in Progress in Botany


# R script developed by Sabine Braun and Simon Tresch
# R version 4.1.2 (2021-11-01)
# Used packages
library(tidyverse) # ggplot and dplyr for data manipulation: Wickham et al., (2019). JOSS 43
library(readxl) # import xlsx data
library(foreign) # Load SYSTAT files
library(latex2exp) # LaTex expressions for plots
library(lme4) # mixed effect models Bates et al. 2018 JSS 67
library(jtools) # mixed models effect plots, tables for model outputs
library(ggeffects) # predicted values for all possible levels or values from a model: Luedecke D. (2018). JOSS 26
library(splines) # modelling natural cubic splines
library(ggpubr)  #  save plots
library(mdthemes)  ## italic labels


# used functions
get_legend<-function(myggplot){ #function to extract the legend of a ggplot
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

se <- function(x) sd(x)/sqrt(length(x)) #standard error

stfakalle <- read.systat("data/stfakalle.SYD")
stfakalle <- stfakalle %>% 
  dplyr::filter(GUELTIG>=0) %>% #-14 old sites
  droplevels()  %>%  #drop old level names
  dplyr::select(KANTON=KANTON.,STNRNEU=STNRNEU, site_name=STNAME.,FICHTE=FICHTE,BUCHE=BUCHE,EICHE=EICHEN) %>% 
  mutate(site_name=as.factor(dplyr::recode(as.character(site_name), 
                                           "Aarwangen   " = "Aarwangen", "Aeschau     "="Aeschau", "Aeschi      "="Aeschi", "Albis       "="Albis", "Allschwil   "="Allschwil", "Alvaneu     "="Alvaneu","Appenzell   "="Appenzell","Axalp       "="Axalp", "Bachtel O   "="Bachtel","Bachtel-O   "="Bachtel", "Bachs       "="Bachs", "Bennwil     "="Bennwil","Beromünster "="Beromünster","Bevaix oben "="Bevaix oben","Bichelsee   "="Bichelsee", "Biel-Benken "="Biel-Benken","Biel Mitte  "="Biel mitte", "Biel unten  "="Biel unten", "Birmenstorf "="Birmenstorf","Birsfelden  "="Birsfelden","Boncourt    "="Boncourt","Bonfol      "="Bonfol","Bonvillars  "="Bonvillars","Braunau     "="Braunau", "Breitenbach "="Breitenbach","Bremgartenwa"="Bremgartenwald", "Brigels     "="Brigels", "Brislach    "="Brislach", "Brusio      "="Brusio","Brütten     "="Brütten",  "Bubend neu  "="Bubendorf neu", "Bubendorf   "="Bubendorf", "Bürglen     "="Bürglen", "Büsserach   "="Büsserach","Busswil     "="Busswil","Cademario   "="Cademario", "Carona      "="Carona","Caslano     "="Caslano","Cavergno    "="Cavergno","Champagne   "="Champagne","Chrischona  "="Chrischona","Coeve       "="Coeve","Courtelary  "="Courtelary","Davos       "="Davos","Diessbach   "="Diessbach","Disentis    "="Disentis","Erlach      "="Erlach","Erlenbach N "="Erlenbach Nordhang", "Erlenbach S "="Erlenbach Südhang", "Evolène     "="Evolène", "Faido       "="Faido", "Fraubrunnen "="Fraubrunnen", "Frienisberg "="Frienisberg","Galm        "="Galm", "Gelfingen   "="Gelfingen", "Gempen      "="Gempen",  "Gempen neu  "="Gempen neu", "Giswil      "="Giswil", "Goumois     "="Goumois","Grosswangen "="Grosswangen", "Gubrist neu "="Gubrist neu", "Habsburg    "="Habsburg", "Habsburg K  "="Habsburg Kontrolle", "HabsburgK   "="Habsburg Kontrolle", "Hard        "="Hard Provenienz","Härkingen   "="Härkingen","Hauenstein  "="Hauenstein", "Herznach    "="Herznach", "Himmelr neu "="Himmelried neu","Himmelried  "="Himmelried","Hinwil      "="Hinwil", "Hochwald    "="Hochwald", "Hölstein    "="Hölstein", "Hölstein Exp"="Hölstein Exp", "Höri        "="Höri", "Hünenb Laubh"="Hünenberg Laubholz Provenienz","Hünenb Ndh  "="Hünenberg Nadelholz Provenienz","Innertkirche"="Innertkirchen","Ittenthal   "="Ittenthal","Jaunpass    "="Jaunpass",
                                           "Kleinandelfi"="Kleinandelfingen", "Klosters    "="Klosters", "Krattigen   "="Krattigen", "Kriechenwil "="Kriechenwil", "L.Erlen alt "="Lange Erlen alt", "L.Erlen jung"="Lange Erlen jung","La Sarraz   "="La Sarraz", "LaBrévine   "="La Brévine", "Lauwil      "="Lauwil", "LeChâtelard "="Le Châtelard","Leissigen   "="Leissigen", "LesVerrières"="Les Verrières", "Liesberg    "="Liesberg",  "Liestal     "="Liestal", "Lugnez      "="Lugnez", "Lurengo     "="Lurengo", "Magglingen  "="Magglingen", "Männedorf   "="Männedorf", "Mellikon    "="Mellikon", "Menzingen   "="Menzingen", "Merishausen "="Merishausen", "Mesocco     "="Mesocco", "Mettendorf  "="Mettendorf", "Metzerlen   "="Metzerlen", "Metzerlen ne"="Metzerlen neu", "Möhlin      "="Möhlin","Möhlin Sunne"="Möhlin Sunnenberg", "Möhl Sunn 5."="Möhl Sunnenberg 5.Str.", "Möhl Unt neu"="Möhlin Unterforst neu", "Möhlin Unt  "="Möhlin Unterforst", "Möhlin Unter"="Möhlin Unterforst", "Morgartenber"="Morgartenberg", "Mühledorf   "="Mühledorf","Muri        "="Muri", "Muttenz oben"="Muttenz oben", "Muttenz unte"="Muttenz unten", "Novaggio    "="Novaggio", "Oberbölchen "="Oberbölchen", "Oberschrot  "="Oberschrot", "Olsberg     "="Olsberg", "Oltingen    "="Oltingen", "Pampigny    "="Pampigny", "Plan-les-Oua"="Plan-les-Ouates","Pratteln    "="Pratteln",  "Rafz        "="Rafz", "Ramosch     "="Ramosch", "Rheinau     "="Rheinau", "Rickenbach  "="Rickenbach","Riehen Ausse"="Riehen Ausserberg", "Riehen Maien"="Riehen Maienbühl",  "Riehen Mitte"="Riehen Mittelberg", "Riggstäfeli "="Riggstäfeli", "Rivera      "="Rivera", "Rodersdorf  "="Rodersdorf", "Romanshorn  "="Romanshorn", "Rothenfluh  "="Rothenfluh", "Rothenfluh G"="Rothenfluh Manganmangelfläche", "Rötiboden   "="Rötiboden", "Rünenberg   "="Rünenberg", "Sagno       "="Sagno", "Satigny     "="Satigny", "Scheidw. obe"="Scheidwald oben","Scheidw.unte"="Scheidwald unten", "Schneisingen"="Schneisingen", "Schwende    "="Schwende","Sciss       "="Sciss","Seewald     "="Seewald", "Seewald Ern."="Seewald Ernährungsfläche", "Selzach Brue"="Selzach Brüel", "Selzach Süls"="Selzach Sülsrain","Sempach     "="Sempach",  "Silenen     "="Silenen",  "Sion        "="Sion", "Sissach     "="Sissach","Souboz      "="Souboz", "Splügen     "="Splügen", "StaMaria    "="Santa Maria", "Stammheim   "="Stammheim", "Stampa      "="Stampa","Stans       "="Stans", "Steckborn   "="Steckborn", "Steinhausen "="Steinhausen", "Sulzchopf   "="Sulzchopf","Tamins      "="Tamins","Teufen      "="Teufen", "Therwil     "="Therwil", "Tomils      "="Tomils", "Tschlin     "="Tschlin", "Twann       "="Twann", "Uhwiesen    "="Uhwiesen", "Unterägeri  "="Unterägeri", "Waldiberg   "="Waldiberg", "Wallisellen "="Wallisellen", "Wangen      "="Wangen", "WangenSZ    "="Wangen SZ", "Wengernalp  "="Wengernalp", "Wilchingen  "="Wilchingen", "Winterthur  "="Winterthur","Zofingen    "="Zofingen",  "Zugerb_VG   "="Zugerb Vordergeissboden", "Zugerberg   "="Zugerberg Hintergeissboden","Zunzgen     "="Zunzgen","Zürichberg  "="Zürichberg" )))  %>% 
  mutate(sites=as.factor(STNRNEU))




# Tree nutrition####

## Modelling ####

### Beech data ####
bbb <- read.systat("data/bun23sq.SYD")
table(bbb$JAHR)

ccc <- bbb %>% 
  left_join(stfakalle, by="STNRNEU")
ddd <- read.systat("data/blattflaechenbuchen23sq.SYD")
eee <- left_join(ccc,ddd,by=c("STNRNEU","BEZ.","JAHR"))

eee <- eee %>%
  mutate(NFL=STICKST*TGFL/1000) %>%  ## Nährstoffe pro cm2; TGFL ist mg/cm2
  mutate(PFL=PHOSPHOR*TGFL/1000) %>%
  mutate(KFL=KALIUM*TGFL/1000) %>%
  mutate(CAFL=CALCIUM*TGFL/1000) %>%
  mutate(MGFL=MAGNES*TGFL/1000) %>%
  mutate(MNFL=MANGAN*TGFL/1000) %>%
  mutate(NBLATT=STICKST*TGBLATT/1000) %>%  ## Nährstoffe pro Blatt
  mutate(PBLATT=PHOSPHOR*TGBLATT/1000) %>%
  mutate(KBLATT=KALIUM*TGBLATT/1000) %>%
  mutate(CABLATT=CALCIUM*TGBLATT/1000) %>%
  mutate(MGBLATT=MAGNES*TGBLATT/1000) %>%
  mutate(MNBLATT=MANGAN*TGBLATT/1000) 

bunaehrsq <- subset(eee, !is.na(PHOSPHOR))
bunaehrsqca <- subset(bunaehrsq,!is.na(CALCIUM))
bunaehrsqmn <- subset(bunaehrsq,!is.na(MANGAN)& MANGAN>=0.1)
ndep1 <- read.systat("data/meteotestndephist23.SYD")
ndep <- ndep1[,c("STNRNEU","JAHR","NDEPHIST")]
aaa <- merge(bunaehrsq,ndep,by=c("STNRNEU","JAHR"))
bunaehrndep <- subset(aaa,!is.na(PHOSPHOR))
bunaehrndepca <- subset(bunaehrndep,!is.na(CALCIUM))
bunaehrfl <- subset(bunaehrsq,!is.na(PFL))
bunaehrsqmn$LMN <- log(bunaehrsqmn$MANGAN)

nbu6 <- lmer(STICKST~as.factor(JAHR)+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summary(nbu6)
nbucont <- lmer(STICKST~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(nbucont)
pbu6 <- lmer(PHOSPHOR~as.factor(JAHR)+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summary(pbu6)
pbucont <- lmer(PHOSPHOR~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(pbucont)
kbu6 <- lmer(KALIUM~as.factor(JAHR)+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summary(kbu6)
kbucont <- lmer(KALIUM~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(kbucont)
mgbu6 <- lmer(MAGNES~as.factor(JAHR)+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summary(mgbu6)
mgbucont <- lmer(MAGNES~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(mgbucont)
cabu6 <- lmer(CALCIUM~as.factor(JAHR)+(1|STNRNEU), bunaehrsqca ,na.action=na.exclude)
summary(cabu6)
cabucont <- lmer(CALCIUM~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(cabucont)
mnbu6 <- lmer(LMN~as.factor(JAHR)+(1|STNRNEU), bunaehrsqmn ,na.action=na.exclude)
summary(mnbu6)
mnbucont <- lmer(LMN~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(mnbucont)
npbu6 <- lmer(NP~as.factor(JAHR)+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summary(npbu6)
npbucont <- lmer(NP~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(npbucont)
nkbu6 <- lmer(NK~as.factor(JAHR)+(1|STNRNEU),bunaehrsq ,na.action=na.exclude)
summary(nkbu6)
nkbucont <- lmer(NK~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(nkbucont)
nmgbu6 <- lmer(NMG~as.factor(JAHR)+(1|STNRNEU),bunaehrsq ,na.action=na.exclude)
summary(nmgbu6)
nmgbucont <- lmer(NMG~JAHR+(1|STNRNEU), bunaehrsq ,na.action=na.exclude)
summ(nmgbucont)

## ggpredict liefert mit logtransformierten abhängigen Variablen ungültige Konfidenzintervalle
## Deshalb Umstellung auf untransformierte ausser Mangan

prednbu <- ggpredict(nbu6,terms="JAHR",as.factor="FALSE")
predpbu <- ggpredict(pbu6,terms="JAHR",as.factor="FALSE")
predkbu <- ggpredict(kbu6,terms="JAHR",as.factor="FALSE")
predmgbu <- ggpredict(mgbu6,terms="JAHR",as.factor="FALSE")
predcabu <- ggpredict(cabu6,terms="JAHR",as.factor="FALSE")
predmnbu1 <- ggpredict(mnbu6,terms="JAHR",as.factor="FALSE")
prednpbu <- ggpredict(npbu6,terms="JAHR",as.factor="FALSE")
prednkbu <- ggpredict(nkbu6,terms="JAHR",as.factor="FALSE")
prednmgbu <- ggpredict(nmgbu6,terms="JAHR",as.factor="FALSE")

predpbu <- ggpredict(pbu6,terms="JAHR [all]")
plot(predpbu)

predmnbu <- predmnbu1
predmnbu$predicted <- exp(predmnbu1$predicted)/1000
predmnbu$conf.low <- exp(predmnbu1$conf.low)/1000
predmnbu$conf.high <- exp(predmnbu1$conf.high)/1000
plot(predmnbu)


prednbu <- data.frame(prednbu)
prednbu <- prednbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Stickstoff") %>%
  mutate(Baumart="Buchen")
predpbu <- data.frame(predpbu)
predpbu <- predpbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Phosphor")%>%
  mutate(Baumart="Buchen")
predkbu <- data.frame(predkbu)
predkbu <- predkbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Kalium")%>%
  mutate(Baumart="Buchen")
predmgbu <- data.frame(predmgbu)
predmgbu <- predmgbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Magnesium")%>%
  mutate(Baumart="Buchen")
predcabu <- data.frame(predcabu)
predcabu <- predcabu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Calcium")%>%
  mutate(Baumart="Buchen")
predmnbu1 <- data.frame(predmnbu1)
predmnbu1 <- predmnbu1 %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Mangan")%>%
  mutate(Baumart="Buchen")
prednpbu <- data.frame(prednpbu)
prednpbu <- prednpbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/P") %>%
  mutate(Baumart="Buchen")
prednkbu <- data.frame(prednkbu)
prednkbu <- prednkbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/K") %>%
  mutate(Baumart="Buchen")
prednmgbu <- data.frame(prednmgbu)
prednmgbu <- prednmgbu %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/Mg") %>%
  mutate(Baumart="Buchen")

predbunaehr <- rbind(prednbu,predpbu,predkbu,predmgbu,predcabu,predmnbu1,prednpbu,prednkbu,prednmgbu)

### Spruce data ####

bbb <- read.systat("data/fin19sq.SYD")
ccc <- bbb %>% 
  left_join(stfakalle, by="STNRNEU")

ddd <- read.systat("data/blattflfichten1123sq.SYD")
eee <- left_join(ccc,ddd,by=c("STNRNEU","BEZ.","JAHR"))

eee <- eee %>%
  mutate(NFL=STICKST*TGFL/1000) %>%  ## Nährstoffe pro cm2; TGFL ist mg/cm2
  mutate(PFL=PHOSPHOR*TGFL/1000) %>%
  mutate(KFL=KALIUM*TGFL/1000) %>%
  mutate(CAFL=CALCIUM*TGFL/1000) %>%
  mutate(MGFL=MAGNES*TGFL/1000) %>%
  mutate(MNFL=MANGAN*TGFL/1000) %>%
  mutate(NNAD=STICKST*NADELGEW/1000) %>%  ## Nährstoffe pro Nadel
  mutate(PNAD=PHOSPHOR*NADELGEW/1000) %>%
  mutate(KNAD=KALIUM*NADELGEW/1000) %>%
  mutate(CANAD=CALCIUM*NADELGEW/1000) %>%
  mutate(MGNAD=MAGNES*NADELGEW/1000) %>%
  mutate(MNNAD=MANGAN*NADELGEW/1000) 


finaehrsq <- subset(eee,!is.na(PHOSPHOR))
finaehrsqca <- subset(finaehrsq,!is.na(CALCIUM))
finaehrsqmn <- subset(finaehrsq,!is.na(MANGAN)& MANGAN>=0.1 & MANGAN <=10000)
finaehrfl <- subset(eee,!is.na(PNAD))


ndep1 <- read.systat("data/meteotestndephist23.SYD")
ndep <- ndep1[,c("STNRNEU","JAHR","NDEPHIST")]
aaa <- merge(finaehrsq,ndep,by=c("STNRNEU","JAHR"))
finaehrndep <- subset(aaa,!is.na(PHOSPHOR))
finaehrndepca <- subset(finaehrndep,!is.na(CALCIUM))

finaehrsqmn$LMN <- log(finaehrsqmn$MANGAN)


nfi6 <- lmer(STICKST~as.factor(JAHR)+(1|STNRNEU), finaehrsq ,na.action=na.exclude)
summary(nfi6)
pfi6 <- lmer(PHOSPHOR~as.factor(JAHR)+(1|STNRNEU), finaehrsq ,na.action=na.exclude)
summary(pfi6)
kfi6 <- lmer(KALIUM~as.factor(JAHR)+(1|STNRNEU), finaehrsq ,na.action=na.exclude)
summary(kfi6)
mgfi6 <- lmer(MAGNES~as.factor(JAHR)+(1|STNRNEU), finaehrsq ,na.action=na.exclude)
summary(mgfi6)
cafi6 <- lmer(CALCIUM~as.factor(JAHR)+(1|STNRNEU), finaehrsqca ,na.action=na.exclude)
summary(cafi6)
mnfi6 <- lmer(LMN~as.factor(JAHR)+(1|STNRNEU), finaehrsqmn ,na.action=na.exclude)
summary(mnfi6)
npfi6 <- lmer(NP~as.factor(JAHR)+(1|STNRNEU), finaehrsq ,na.action=na.exclude)
summary(npfi6)
nkfi6 <- lmer(NK~as.factor(JAHR)+(1|STNRNEU),finaehrsq ,na.action=na.exclude)
summary(nkfi6)
nmgfi6 <- lmer(NMG~as.factor(JAHR)+(1|STNRNEU),finaehrsq ,na.action=na.exclude)
summary(nmgfi6)


jahr <- as.factor(finaehrsq$JAHR)
jahrca <- as.factor(finaehrsqca$JAHR)
jahrmn <- as.factor(finaehrsqmn$JAHR)


prednfi <- ggpredict(nfi6,terms="JAHR",as.factor="FALSE")
predpfi <- ggpredict(pfi6,terms="JAHR",as.factor="FALSE")
predkfi <- ggpredict(kfi6,terms="JAHR",as.factor="FALSE")
predmgfi <- ggpredict(mgfi6,terms="JAHR",as.factor="FALSE")
predcafi <- ggpredict(cafi6,terms="JAHR",as.factor="FALSE")
predmnfi1 <- ggpredict(mnfi6,terms="JAHR",as.factor="FALSE")
prednpfi <- ggpredict(npfi6,terms="JAHR",as.factor="FALSE")
prednkfi <- ggpredict(nkfi6,terms="JAHR",as.factor="FALSE")
prednmgfi <- ggpredict(nmgfi6,terms="JAHR",as.factor="FALSE")


predmnfi <- predmnfi1
predmnfi$predicted <- exp(predmnfi1$predicted)/1000
predmnfi$conf.low <- exp(predmnfi1$conf.low)/1000
predmnfi$conf.high <- exp(predmnfi1$conf.high)/1000
plot(predmnfi)



prednfi <- data.frame(prednfi)
prednfi <- prednfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Stickstoff") %>%
  mutate(Baumart="Fichten")
predpfi <- data.frame(predpfi)
predpfi <- predpfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Phosphor")%>%
  mutate(Baumart="Fichten")
predkfi <- data.frame(predkfi)
predkfi <- predkfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Kalium")%>%
  mutate(Baumart="Fichten")
predmgfi <- data.frame(predmgfi)
predmgfi <- predmgfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Magnesium")%>%
  mutate(Baumart="Fichten")
predcafi <- data.frame(predcafi)
predcafi <- predcafi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Calcium")%>%
  mutate(Baumart="Fichten")
predmnfi1 <- data.frame(predmnfi1)
predmnfi1 <- predmnfi1 %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Mangan")%>%
  mutate(Baumart="Fichten")
prednpfi <- data.frame(prednpfi)
prednpfi <- prednpfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/P") %>%
  mutate(Baumart="Fichten")
prednkfi <- data.frame(prednkfi)
prednkfi <- prednkfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/K") %>%
  mutate(Baumart="Fichten")
prednmgfi <- data.frame(prednmgfi)
prednmgfi <- prednmgfi %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/Mg") %>%
  mutate(Baumart="Fichten")



predfinaehr <- rbind(prednfi,predpfi,predkfi,predmgfi,predcafi,predmnfi1,prednpfi,prednkfi,prednmgfi)


### Oak data ####
bbb <- read.systat("data/ein23sq.SYD")
ccc <- bbb %>% 
  left_join(stfakalle, by="STNRNEU")
ddd <- read.systat("data/blattflaecheneichen23sq.SYD")
eee <- left_join(ccc,ddd,by=c("STNRNEU","BEZ.","JAHR"))

eee <- eee %>%
  mutate(NFL=STICKST*TGFL/1000) %>%  ## Nährstoffe pro cm2; TGFL ist mg/cm2
  mutate(PFL=PHOSPHOR*TGFL/1000) %>%
  mutate(KFL=KALIUM*TGFL/1000) %>%
  mutate(CAFL=CALCIUM*TGFL/1000) %>%
  mutate(MGFL=MAGNES*TGFL/1000) %>%
  mutate(MNFL=MANGAN*TGFL/1000) %>%
  mutate(NBLATT=STICKST*TGBLATT/1000) %>%  ## Nährstoffe pro Blatt
  mutate(PBLATT=PHOSPHOR*TGBLATT/1000) %>%
  mutate(KBLATT=KALIUM*TGBLATT/1000) %>%
  mutate(CABLATT=CALCIUM*TGBLATT/1000) %>%
  mutate(MGBLATT=MAGNES*TGBLATT/1000) %>%
  mutate(MNBLATT=MANGAN*TGBLATT/1000) 


einaehrsq1 <- subset(eee,!is.na(PHOSPHOR))
einaehrsq <- einaehrsq1
ndep1 <- read.systat("data/meteotestndephist23.SYD")
ndep <- ndep1[,c("STNRNEU","JAHR","NDEPHIST")]
aaa <- merge(einaehrsq,ndep,by=c("STNRNEU","JAHR"))
einaehrndep <- subset(aaa,!is.na(PHOSPHOR))
einaehrfl <- subset(eee,!is.na(PFL))

einaehrsq$LMN <- log(einaehrsq$MANGAN+0.001)


nei6 <- lmer(STICKST~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(nei6)
pei6 <- lmer(PHOSPHOR~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(pei6)
kei6 <- lmer(KALIUM~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(kei6)
mgei6 <- lmer(MAGNES~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(mgei6)
caei6 <- lmer(CALCIUM~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(caei6)
mnei6 <- lmer(LMN~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(mnei6)
npei6 <- lmer(NP~as.factor(JAHR)+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(npei6)
nkei6 <- lmer(NK~as.factor(JAHR)+(1|STNRNEU),einaehrsq ,na.action=na.exclude)
summary(nkei6)
nmgei6 <- lmer(NMG~as.factor(JAHR)+(1|STNRNEU),einaehrsq ,na.action=na.exclude)
summary(nmgei6)


mnei6a <- lm(LMN~JAHR+(1|STNRNEU), einaehrsq ,na.action=na.exclude)
summary(mnei6a)


jahrei <- as.factor(einaehrsq$JAHR)


prednei <- ggpredict(nei6,terms="JAHR",as.factor="FALSE")
predpei <- ggpredict(pei6,terms="JAHR",as.factor="FALSE")
predkei <- ggpredict(kei6,terms="JAHR",as.factor="FALSE")
predmgei <- ggpredict(mgei6,terms="JAHR",as.factor="FALSE")
predcaei <- ggpredict(caei6,terms="JAHR",as.factor="FALSE")
predmnei1 <- ggpredict(mnei6,terms="JAHR",as.factor="FALSE")
prednpei <- ggpredict(npei6,terms="JAHR",as.factor="FALSE")
prednkei <- ggpredict(nkei6,terms="JAHR",as.factor="FALSE")
prednmgei <- ggpredict(nmgei6,terms="JAHR",as.factor="FALSE")

predmnei <- predmnei1
predmnei$predicted <- exp(predmnei1$predicted)/1000
predmnei$conf.low <- exp(predmnei1$conf.low)/1000
predmnei$conf.high <- exp(predmnei1$conf.high)/1000
plot(predmnei)
manganei <- einaehrsq$MANGAN/1000



prednei <- data.frame(prednei)
prednei <- prednei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Stickstoff") %>%
  mutate(Baumart="Eichen")
predpei <- data.frame(predpei)
predpei <- predpei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Phosphor")%>%
  mutate(Baumart="Eichen")
predkei <- data.frame(predkei)
predkei <- predkei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Kalium")%>%
  mutate(Baumart="Eichen")
predmgei <- data.frame(predmgei)
predmgei <- predmgei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Magnesium")%>%
  mutate(Baumart="Eichen")
predcaei <- data.frame(predcaei)
predcaei <- predcaei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Calcium")%>%
  mutate(Baumart="Eichen")
predmnei1 <- data.frame(predmnei1)
predmnei1 <- predmnei1 %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="Mangan")%>%
  mutate(Baumart="Eichen")
prednpei <- data.frame(prednpei)
prednpei <- prednpei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/P") %>%
  mutate(Baumart="Eichen")
prednkei <- data.frame(prednkei)
prednkei <- prednkei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/K") %>%
  mutate(Baumart="Eichen")
prednmgei <- data.frame(prednmgei)
prednmgei <- prednmgei %>%
  mutate(Jahr = as.numeric(as.character(x))) %>%
  mutate(Element="N/Mg") %>%
  mutate(Baumart="Eichen")




predeinaehr <- rbind(prednei,predpei,predkei,predmgei,predcaei,predmnei1,prednpei,prednkei,prednmgei)



rm(list=setdiff(ls(), c("predfinaehr","predbunaehr","predeinaehr")))
##  N ####

databu <- subset(predbunaehr,Element=="Stickstoff")
datafi <- subset(predfinaehr,Element=="Stickstoff")
dataei <- subset(predeinaehr,Element=="Stickstoff")

nentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
    ylab(expression(N~(mg~g^-1~d.m.)))+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=13.1,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=19,linetype=2,color="firebrick3")+
  geom_hline(yintercept=20,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
   theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
nentweng


## P ####
databu <- subset(predbunaehr,Element=="Phosphor")
datafi <- subset(predfinaehr,Element=="Phosphor")
dataei <- subset(predeinaehr,Element=="Phosphor")

pentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  ylab(expression(P~(mg~g^-1~d.m.)))+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"), name="Species", labels= c("Fagus sylvatica", "Quercus spp.", "Picea abies"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"), name="Species", labels= c("Fagus sylvatica", "Quercus spp.", "Picea abies"))+
  theme(legend.position="bottom")+
  geom_hline(yintercept=1.3,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=1.2,linetype=2,color="firebrick3")+
  geom_hline(yintercept=1.35,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
   theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),plot.title = element_text(hjust = 0.5))
pentweng


## K ####
databu <- subset(predbunaehr,Element=="Kalium")
datafi <- subset(predfinaehr,Element=="Kalium")
dataei <- subset(predeinaehr,Element=="Kalium")

kentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  ylab(expression(K~(mg~g^-1~d.m.)))+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=4.5,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=6,linetype=2,color="firebrick3")+
  geom_hline(yintercept=6.1,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
kentweng

## Mg ####
databu <- subset(predbunaehr,Element=="Magnesium")
datafi <- subset(predfinaehr,Element=="Magnesium")
dataei <- subset(predeinaehr,Element=="Magnesium")

mgentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  ylab(expression(Mg~(mg~g^-1~d.m.)))+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=0.8,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=1,linetype=2,color="firebrick3")+
  geom_hline(yintercept=1.2,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
mgentweng


## Ca ####
databu <- subset(predbunaehr,Element=="Calcium")
datafi <- subset(predfinaehr,Element=="Calcium")
dataei <- subset(predeinaehr,Element=="Calcium")

caentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  ylab(expression(Ca~(mg~g^-1~d.m.)))+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=2,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=5,linetype=2,color="firebrick3")+
  geom_hline(yintercept=5.05,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
caentweng


## Mn ####
databu <- subset(predbunaehr,Element=="Mangan")
datafi <- subset(predfinaehr,Element=="Mangan")
dataei <- subset(predeinaehr,Element=="Mangan")

mnentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
 geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  ylab(expression(Ca~(mg~g^-1~d.m.)))+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  # geom_hline(yintercept=0.050,linetype=2,color="dodgerblue3")+
  # geom_hline(yintercept=0.060,linetype=2,color="firebrick3")+
  # geom_hline(yintercept=0.066,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
mnentweng


##  N:P ####
databu <- subset(predbunaehr,Element=="N/P")
datafi <- subset(predfinaehr,Element=="N/P")
dataei <- subset(predeinaehr,Element=="N/P")

npentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  scale_y_continuous (name="N:P ratio (w/w)")+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=10.44,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=15.7,linetype=2,color="firebrick3")+
  geom_hline(yintercept=14.31,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
npentweng


##  N:K ####
databu <- subset(predbunaehr,Element=="N/K")
datafi <- subset(predfinaehr,Element=="N/K")
dataei <- subset(predeinaehr,Element=="N/K")

nkentweng <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.9))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.9))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  scale_y_continuous (name="N:K ratio (w/w)")+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=2.89,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=2.73,linetype=2,color="firebrick3")+
  geom_hline(yintercept=2.7,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
nkentweng

##  N:Mg ####
databu <- subset(predbunaehr,Element=="N/Mg")
datafi <- subset(predfinaehr,Element=="N/Mg")
dataei <- subset(predeinaehr,Element=="N/Mg")


nmgentw <- ggplot(bind_rows(databu,datafi,dataei),aes(Jahr,predicted,colour=Baumart,fill=Baumart))+
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high,colour=Baumart, width=.001),position = position_dodge(width = 0.4))+
  geom_point(aes(colour=Baumart),size=3,position = position_dodge(width = 0.4))+
  geom_smooth(method="loess",span=2, alpha=0.2, size=1,se=T)+
  scale_y_continuous (name="N:Mg ratio (w/w)")+
  scale_x_continuous (name="", limits=c(1983, 2024))+
  scale_fill_manual(values=c("firebrick3", "forestgreen","dodgerblue3"))+
  scale_colour_manual(values=c("firebrick3","forestgreen", "dodgerblue3"))+
  theme(legend.position="none")+

  
  geom_hline(yintercept=17.10,linetype=2,color="dodgerblue3")+
  geom_hline(yintercept=26.93,linetype=2,color="firebrick3")+
  geom_hline(yintercept=16.10,linetype=2,color="forestgreen")+
  theme(axis.text=element_text(size=18))+
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_text(size=16))+
  theme(legend.text=element_text(size=14))+
  theme(axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),axis.text.x = element_text(size =16, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))
nmgentw


### Save plots #####
get_legend_style <- get_legend(pentweng)

plots <-ggarrange(nentweng,pentweng,kentweng,mgentweng,npentweng,nkentweng,
                           nrow=2,ncol=3,common.legend = T, legend="bottom",legend.grob=get_legend_style, align="v")


ggsave(plots,file="plots/naehrentweng.pdf",width=40,height=30, units = "cm", dpi = 1000)





# Crown transperancy####

# Beech data #

# crown defoliation in percentage are recorded annually described in Braun et al. 2021 (https://www.frontiersin.org/articles/10.3389/ffgc.2021.765782/full) according to ICP Forests (2016). Manual on Methods and Criteria for Harmonized Sampling, Assessment, Monitoring and Analysis of the Effects of Air Pollution on Forests.
# crown_def_0 = weighted mean per year and site > 15%,  crown_def_1 = weighted mean per year and site > 15% < 25%,  crown_def_2 = weighted mean per year and site > 25% < 60% crown defoliation, crown_def_3 = weighted mean per year and site > 60% crown defoliation, crown_def_2_3 = weighted mean per year and site > 25% & >60% crown defoliation

crown_beech<-openxlsx::read.xlsx("data/WDB_beech_crown_2023.xlsx")

crown_beech_year <-crown_beech %>%
  group_by(year) %>%
  mutate(mean_PROZ2=( ((sum((crown_def_2*no_trees)/100))/sum(no_trees))*100),
         se_PROZ2=(se(crown_def_2)),
         mean_PROZ3=( ((sum((crown_def_3*no_trees)/100))/sum(no_trees))*100),
         se_PROZ3=(se(crown_def_3)),
         mean_PROZ23=( ((sum((crown_def_2_3*no_trees)/100))/sum(no_trees))*100),
         se_PROZ23=(se(crown_def_2_3)),
         ANZB=sum(no_trees)) %>%
  ungroup()

crown_beech_year$tree<- "beech"


# Spruce data #

# crown defoliation in percentage are recorded annually described in Tresch et al. 2023 (https://linkinghub.elsevier.com/retrieve/pii/S0048969723028449) according to ICP Forests (2016). Manual on Methods and Criteria for Harmonized Sampling, Assessment, Monitoring and Analysis of the Effects of Air Pollution on Forests.
# crown_def_0 = weighted mean per year and site > 15%,  crown_def_1 = weighted mean per year and site > 15% < 25%,  crown_def_2 = weighted mean per year and site > 25% < 60% crown defoliation, crown_def_3 = weighted mean per year and site > 60% crown defoliation, crown_def_2_3 = weighted mean per year and site > 25% & >60% crown defoliation

crown_spruce <- openxlsx::read.xlsx("data/WDB_spruce_crown_2023.xlsx")
crown_spruce

crown_spruce_year <- crown_spruce %>%
  group_by(year) %>%
  mutate(
    mean_PROZ2= mean(crown_def_2,na.rm=T),
    se_PROZ2=(se(crown_def_2)),
    mean_PROZ3=mean(crown_def_3,na.rm=T),
    se_PROZ3=(se(crown_def_3)),
    mean_PROZ23=mean(crown_def_2_3,na.rm=T),
    se_PROZ23=(se(crown_def_2_3)),
    ANZB=sum(no_trees)) %>%
  ungroup()

crown_spruce_year$tree<- "spruce"

crown_beech_spruce_year <- crown_beech_year %>% bind_rows(crown_spruce_year)


# Oak data #


crown_oak <- openxlsx::read.xlsx("data/WDB_oak_crown_BART_2023.xlsx")
crown_oak





load("data/bufieiverl.RData")

verlallebufi <- subset(verlalle,Baumart=="Buchen" | Baumart=="Fichten")
verlallebuei <- subset(verlalle,Baumart=="Buchen" | Baumart=="Eichen")



## Plots ####
bufrb <- "firebrick3"
fifrb <- "#225ea8"
eifrb <- "forestgreen"
bufifrb <- "#fe9929"
bueifrb <- "#8c2d04"


### Trees with crown transparency >60% #####

bufiverlproz3eng <-ggplot(crown_beech_spruce_year %>% mutate (year = as.numeric(year)), aes(x=year, y=mean_PROZ3, color=tree,fill=tree)) +
  geom_point(size=3,position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin=mean_PROZ3-se_PROZ3, ymax=mean_PROZ3+se_PROZ3), width=.0001,position = position_dodge(width = 0.9))+
  geom_line(position = position_dodge(width = 0.9))+
  labs(title="",y= ("Trees with crown transparency >60%"), x = "")+
  scale_x_continuous (name="", limits=c(1984, 2023), breaks = c(1984, 1990, 2000, 2010, 2020))+
  mdthemes::md_theme_classic() +
  scale_color_manual(name = "Species",  values = c(bufrb,fifrb),label=c("*Fagus sylvatica*","*Picea abies*")) +
  scale_fill_manual(name = "Species",  values = c(bufrb,fifrb),label=c("*Fagus sylvatica*","*Picea abies*")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),limits = c(-0.1, 5.2),expand = c(0, 0))+
  theme(axis.ticks = element_line(size=0.2), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.justification = c(0, 1), legend.position = c(0,1),legend.direction="vertical",legend.key.height=unit(1,"line"))
bufiverlproz3eng



eiartverlproz3eng <-ggplot(crown_beech_spruce_year %>% mutate (year = as.numeric(year)), aes(x=year, y=mean_PROZ3, color=tree,fill=tree)) +
  geom_point(size=3,position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin=mean_PROZ3-se_PROZ3, ymax=mean_PROZ3+se_PROZ3), width=.0001,position = position_dodge(width = 0.9))+
  geom_line(position = position_dodge(width = 0.9))+
  labs(title="",y= ("Trees with crown transparency >60%"), x = "")+
  scale_x_continuous (name="", limits=c(1984, 2023), breaks = c(1984, 1990, 2000, 2010, 2020))+
  mdthemes::md_theme_classic() +
  scale_colour_brewer(palette ="Dark2", name="Species",label=c("*Quercus pubescens*","*Quercus robur*","*Quercus petraea*"))+
  scale_fill_brewer(palette ="Dark2", name="Species",label=c("*Quercus pubescens*","*Quercus robur*","*Quercus petraea*"))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),limits = c(-0.1, 5.2),expand = c(0, 0))+
  theme(axis.ticks = element_line(size=0.2), axis.line=element_line(colour="black"),text = element_text(size=16, colour = "black"),
        axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.justification = c(0, 1), legend.position = c(0,1),legend.direction="vertical",legend.key.height=unit(1,"line"))
eiartverlproz3eng


### Save plots #####

plots <-ggarrange(bufiverlproz3eng,eiartverlproz3eng,
                  nrow=1,ncol=2,align="v")


ggsave(plots,file="plots/bueifiverlproz3eng.pdf",width=40,height=30, units = "cm", dpi = 1000)


