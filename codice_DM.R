setwd("C:/Users/finzi/OneDrive/Desktop/Università/Terzo anno/Data meaning e Machine learning/ELABORATO_DM")

options(scipen=999)

r <- read.csv("car details v4.csv" , sep="," , dec = ".",  
              stringsAsFactors=TRUE, na.strings=c("NA","NaN", ""))

r$Prezzo_eu <- r$Price * 0.011 ; head(r$Price_eu)
r$Model <- as.character(r$Model)

variabili <- paste(colnames(r), collapse="','")
variabili

features <- c('Make','Prezzo_eu','Model','Year','Kilometer','Fuel.Type','Transmission','Location','Color','Owner','Seller.Type','Engine','Drivetrain','Length','Width','Height','Seating.Capacity','Fuel.Tank.Capacity')
r=r[,features]

library("tidyr")

r <- r %>%
  separate(Engine, c("col1", "col2"), " ")

library(dplyr)
r <- r[, -13]

colnames(r)[12] <- "Engine"
r$Engine <- as.numeric(r$Engine)

library(Hmisc)
describe(r)

####MISSING

sapply(r, function(x)(sum(is.na(x)))) 

library(VIM)
missingness_r <- aggr(r, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(r), cex.axis=.7,gap=3)

#MODELLO

fit_0 <- lm(Prezzo_eu ~ ., data = r)
summary(fit_0)
nrow(r)
length(fit_0$residuals) #il modello usa 1874 obs -> ci sono 1874 righe complete

drop1(fit_0, test="F")

par(mfrow=c(2,2)) 
plot(fit_0)
par(mfrow=c(1,1)) 

plot(r$Color) #controllino su color per concludere che in realtà ci sta tenerlo così #LO TOGLIEREMO IN QUANTO NON SIGNIFICATIVO]
plot(r$Seller.Type) #controllino su sellertype: stepwise lo toglie, infatti sono tutti "individual", quindi lo togliamo (tra l'altro è quasi zero variance) #[LO TOGLIEREMO  IN QUANTO NON SIGNIFICATIVO]
plot(r$Height) #togliamo anche height poiché nonostante la model lo salvi, non è significativo #[LO TOGLIEREMO IN QUANTO NON SIGNIFICATIVO]
plot(r$Owner) #pure questo lo teniamo  #[LO TOGLIEREMO IN QUANTO NON SIGNIFICATIVO]

#make è collineare con model per questo non viene fuori 


#variili qualitative:
covariate0 = attr(terms(fit_0), "term.labels") ; covariate0 #estrazione delle covariate usate dal modello in fit0
library(plyr) 
library(dplyr)
b2_factors <- r[,covariate0] %>% 
  dplyr::select_if(is.factor) ; b2_factors 
colnames(b2_factors)
str(b2_factors)

combos <- combn(ncol(b2_factors),2)
adply(combos, 2, function(x) {
  test <- chisq.test(b2_factors[, x[1]], b2_factors[, x[2]])
  tab  <- table(b2_factors[, x[1]], b2_factors[, x[2]])
  out <- data.frame("Row" = colnames(b2_factors)[x[1]]
                    , "Column" = colnames(b2_factors[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))
                    , "u1" =length(unique(b2_factors[,x[1]]))-1
                    , "u2" =length(unique(b2_factors[,x[2]]))-1
                    , "nMinu1u2" =sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))* min(length(unique(b2_factors[,x[1]]))-1 , length(unique(b2_factors[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))* min(length(unique(b2_factors[,x[1]]))-1 , length(unique(b2_factors[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) #se chi quadrato normalizzato maggiore di 0.8 la butti

#varibili numeriche:
isnumeric <- sapply(r, function(x) is.numeric(x))
numericdata <- r[, isnumeric]

#ATTENZIONE: il prossimo comando fa sbarellare R a volte, usarlo alla fine o prima di riavviare (grafico covariate numeriche)
library(PerformanceAnalytics)
chart.Correlation(numericdata , histogram=TRUE, pch=19)

##numeriche VIF
y <- as.numeric(r$Prezzo_eu) 
X <- numericdata
X_matrix = as.matrix(X)
mod <- lm(y ~ X_matrix) #in notazione matriciale
library(mctest)
imcdiag(mod) #qui produciamo TOL e VIF 

#
#OPTIMAL GROUPING di LOCATION
library(dplyr)
library(factorMerger)

reduce_levels <- mergeFactors(response = r$Prezzo_eu, factor = r$Location)
mergingHistory(reduce_levels, showStats = TRUE ) %>%head(5)
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE )
plot(reduce_levels, palette = "Reds")
plot(reduce_levels, responsePanel = "boxplot", colorCluster = TRUE)
?cutree
og = cutTree(reduce_levels)
r$optimal_grouping = og
head(r)

r$optimal_grouping <- as.factor(as.numeric(r$optimal_grouping))

colnames(r)[19] <- "Location_G"
r <- r[,-8] #togliamo location originale
r <- r[,-3] #togliamo model [DA FARE PRIMA]
fit_1 <- lm(Prezzo_eu ~ ., data = r)
summary(fit_1)
drop1(fit_1, test ="F")

plot(r$Location_G, r$Prezzo_eu)

#COLLIN

covariate1 = attr(terms(fit_1), "term.labels") ; covariate1 #estrazione delle covariate usate dal modello in fit2
library(plyr) 
library(dplyr)
b2_factors <- r[,covariate1] %>% 
  dplyr::select_if(is.factor) ; b2_factors 
colnames(b2_factors)
str(b2_factors)

combos <- combn(ncol(b2_factors),2)
adply(combos, 2, function(x) {
  test <- chisq.test(b2_factors[, x[1]], b2_factors[, x[2]])
  tab  <- table(b2_factors[, x[1]], b2_factors[, x[2]])
  out <- data.frame("Row" = colnames(b2_factors)[x[1]]
                    , "Column" = colnames(b2_factors[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))
                    , "u1" =length(unique(b2_factors[,x[1]]))-1
                    , "u2" =length(unique(b2_factors[,x[2]]))-1
                    , "nMinu1u2" =sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))* min(length(unique(b2_factors[,x[1]]))-1 , length(unique(b2_factors[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))* min(length(unique(b2_factors[,x[1]]))-1 , length(unique(b2_factors[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 

##numeriche VIF
y <- as.numeric(r$Prezzo_eu) 
X <- numericdata
X_matrix = as.matrix(X)
mod <- lm(y ~ X_matrix) #in notazione matriciale
library(mctest)
imcdiag(mod) #qui produciamo TOL e VIF


cov=attr(terms(fit_1), "term.labels") 
cov

library(dplyr)
r_numeric <- r[,covariate1]%>% dplyr::select_if(is.numeric)
colnames(r_numeric)

require(corrgram)
corrgram(r_numeric)
corrgram(r_numeric, lower.panel = panel.cor, cex=1, cex.labels = 1)

##OPTIMAL GROUPING MAKE

library(dplyr)
library(factorMerger)

reduce_levels1 <- mergeFactors(response = r$Prezzo_eu, factor = r$Make)
mergingHistory(reduce_levels1, showStats = TRUE ) %>%head(50)
plot(reduce_levels1 , panel = "GIC",title = "", panelGrid = FALSE )
plot(reduce_levels1, palette = "Reds")
plot(reduce_levels1, responsePanel = "boxplot", colorCluster = TRUE)

og_2 <- cutTree(reduce_levels1, stat = "loglikelihood" , value = -23000); str(og_2) # taglio dendrogramma a 5 gruppi
#og1 = cutTree(reduce_levels1) vecchio sbagliato
r$optimal_grouping1 = og_2
head(r)

fit_g3 = lm(Prezzo_eu ~ Make  ,r)
fit_g4 = lm(Prezzo_eu ~ optimal_grouping1 ,r)
summary(fit_g3)
summary(fit_g4)

r$optimal_grouping1 <- as.factor(as.numeric(r$optimal_grouping1))

#r <- r[,-17] sbagliato
r <- r[,-1]
colnames(r)[17] <- "Make_G"
fit_1 <- lm(Prezzo_eu ~ ., data = r)
summary(fit_1)
drop1(fit_1, test ="F")

plot(r$Make_G, r$Prezzo_eu)

par(mfrow=c(2,2)) 
plot(fit_1)
par(mfrow=c(1,1)) 

#COLLIN 

variabili <- paste(colnames(r), collapse="+") ; variabili

fit_2 <- lm(Prezzo_eu ~ Year+Kilometer+Fuel.Type+Transmission+Color+Owner+Seller.Type+Engine+Drivetrain+Width+Height+Seating.Capacity+Fuel.Tank.Capacity+Location_G+Make_G, data =r)
summary(fit_2)
drop1(fit_2, test ="F")

isnumeric <- sapply(r1, function(x) is.numeric(x))
numericdata1 <- r1[, isnumeric]

#r1 <- r[,-11]   #per la regressione senza Lenght pwer verificare collin 
y1 <- as.numeric(r1$Prezzo_eu) 
X1 <- numericdata1
X_matrix1 = as.matrix(X1)
mod1 <- lm(y1 ~ X_matrix1) #in notazione matriciale
library(mctest)
imcdiag(mod1) #qui produciamo TOL e VIF

# PRINCIPAL COMPONENTS
#2 modi

library(dplyr)
r_noNA <- na.omit(r)  ; nrow(r_noNA)
r_pca <- r_noNA %>% select("Length","Width","Fuel.Tank.Capacity","Engine") ; head(r_pca)
nrow(r_pca)

#primo modo [non usare!!]
pca <- prcomp(r_pca, scale=TRUE, center= TRUE) # metodo per avere output più chiari [NON SERVE NEL COSO FINALE]
library(factoextra)
tt <- data.frame(get_eig(pca))
tt$pca <- 1:nrow(tt)
# % var extracted
head(tt)
# prima pc spiega 85% varianza

# secondo modo non usare!! meglio il prox
?princomp
fit_pca_1 <- princomp(r_pca,  cor=TRUE) #normalizzate poiché d
summary(fit_pca_1) 

#terzo modo che usiamo [specifico per info su prima pc che poi teniamo]
library(psych)
fit_pca_2 <- principal(r_pca, nfactors=1, scores = TRUE,  covar=FALSE) ; head(fit_pca_2$scores) ; fit_pca_2
summary(fit_pca_2)
??principal
pc1 <- data.frame(fit_pca_2$scores) ; head(pc1)

r_noNA_pc <- cbind(r_noNA, pc1) ; head(r_noNA_pc) #così hanno lo stesso numero di righe

#COLLINEARITà ancora

r2 <- r_noNA_pc[,!names(r_noNA_pc) %in% c("Length","Width","Fuel.Tank.Capacity","Engine")] #dataset senza variabili di pc

isnumeric <- sapply(r2, function(x) is.numeric(x))
numericdata2 <- r2[, isnumeric]

y <- as.numeric(r_noNA_pc$Prezzo_eu) 
X2 <- numericdata2
X_matrix2 = as.matrix(X2)
mod2 <- lm(y ~ X_matrix2) #in notazione matriciale
library(mctest)
imcdiag(mod2) #qui produciamo TOL e VIF (pc1 è una var quantitaliva!!!nb)

#collinearità risolta 

#[potremmo togliere qua color perchè non significativa]
#LINEARITà

fit_c <- lm(Prezzo_eu ~ . , data = r2)
summary(fit_c)
drop1(fit_c, test = "F")

par(mfrow=c(2,2)) 
plot(fit_c)
par(mfrow=c(1,1)) 

prova <- cbind(r2, r_noNA_pc$Prezzo_eu) ;  head(prova)
prova2 <- prova[,-14] ; head(prova2)

library(lmtest)
resettest(fit_c, power = 2, type = "fitted",  data = prova2) # modello non correttamente specificato

#boxcox

library(MASS)
boxcoxreg1 <- boxcox(fit_c) ; boxcoxreg1 ; summary(boxcoxreg1)
title("Lambda vs Log-Likelihood")
lambdamax <- boxcoxreg1$x[which.max(boxcoxreg1$y)] ; lambdamax 

ylog <- log(r2$Prezzo_eu)
r2 <- cbind(r2, ylog) 
r2 <- r2[,-1] 

fit_l0 <- lm(ylog ~ . , data= r2) 

library(lmtest) 
resettest(fit_l0, power = 2, type = "fitted",  data = r2) # non corettamente specificato (ma meglio del reset pre boxcox)

#gam 

variabili2 <- paste(colnames(r2), collapse="+") ; variabili2

library(gam)
gam_1 <- gam(ylog ~ lo(Year) + lo(Kilometer) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain+lo(Height)+lo(Seating.Capacity)+Location_G+Make_G+lo(PC1), data = r2)
summary(gam_1) 

par(mfrow=c(3,2)) 
plot(gam_1)
par(mfrow=c(1,1)) 

summary(r2$Year)
plot(r2$Year)
plot(r2$Kilometer)
plot(r2$Seating.Capacity)

gam_2 <- gam(ylog ~ s(Year) + s(Kilometer) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain+s(Height)+s(Seating.Capacity)+Location_G+Make_G+s(PC1), data = r2)
summary(gam_2) 

par(mfrow=c(3,3)) 
plot(gam_2)
par(mfrow=c(1,1)) 

#modello post gam, ma serve gestire gli influenti prima
fit_l1 <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain + Height+ I(Height^2)+I(Height^3) + Seating.Capacity + Location_G+Make_G+ PC1, data = r2)
drop1(fit_l1, test = "F")
library(lmtest) 
resettest(fit_l1, power = 2, type = "fitted",  data = r2) # non corettamente specificato (ma meglio del reset pre boxcox)


#(OUTLIER PER CAPIRE MEGLIO GAM)
#perchè outlier molto elevati potrebbero sballare le gam facendo vedere finte forme funzionali nelle distribuzioni
library(car)
influencePlot(fit_l0,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#Cook's D

cooksd <- cooks.distance(fit_l0)
cd <- data.frame(cooksd)
#Settiamo ora il valore di cut-off, pari a 4/n, dove n = numero di osservazioni usate dal modello
cutoff <- 4/(length(fit_l0$residuals)-length(fit_l0$coefficients)-2)
oss_influenti <- data.frame(r2[cooksd > cutoff, ])  # influential row numbers
nrow(oss_influenti) 
#Ora sappiamo quante osservazioni sono influenti secondo Cook's D:108

#Si tratta ora di considerare solo le righe non influenti e fittare il modello su questi dati

r2_noinflu <- data.frame(r2[cooksd < cutoff, ])  
fit_l0_noinflu <- lm(ylog ~ . , data= r2_noinflu) 
influencePlot(fit_l0_noinflu,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#rimangono osservazioni ad alto leverage (quindi influenti per le x ma non per la y)

#RIFACCIO UNA GAM POST INFLUENTI 
gam_3 <- gam(ylog ~ s(Year) + s(Kilometer) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain+s(Height)+s(Seating.Capacity)+Location_G+Make_G+s(PC1), data = r2_noinflu)

par(mfrow=c(3,3)) 
plot(gam_3)
par(mfrow=c(1,1)) 

fit_l2_noin <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + Color +
                    Owner+Seller.Type+Drivetrain + Height+ I(Height^2)+I(Height^3) + Seating.Capacity + Location_G+Make_G+ PC1, data = r2_noinflu)
drop1(fit_l2_noin, test = "F")
resettest(fit_l2_noin, power = 2, type = "fitted",  data = r2_noinflu) 
#comunque non specificato bene ma molto meglio di prima NB: non significative height e potenze e seller type
#quindi, togliamo potenze ma teniamo seller type e height^1

#parentesi su fit quasi definitivo STRANO SEATING CAPACITY [RISOLTO DOPO, NON METTERE NEL FINALE]
fit_z <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission +
                    Owner+Drivetrain + Seating.Capacity + I(Seating.Capacity^2) + Location_G+Make_G+ PC1, data = r2_noinflu)
drop1(fit_z, test="F")
resettest(fit_z, power = 2, type = "fitted",  data = r2_noinflu) 
summary(fit_z)


###decidiamo di togliere le variabili non significative height, color seller.type 

#DA QUI IN POI DECIDERE SE MANTENERE SEATING CAPACITY^2 POICHè NON SIGNIFICATIVO MA MIGLIRA UN BOTTO IL RESET (EVENTUALMENTE SCRIVERE A LOVAGLIO) [RISOLTO, NON LO TENIAMO]

fit_clo <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + 
                Owner+Drivetrain+ Seating.Capacity + Location_G+Make_G+ PC1, data = r2_noinflu)
drop1(fit_clo, test="F")
summary(fit_clo)
library(lmtest)
resettest(fit_clo, power = 2, type = "fitted",  data = r2_noinflu) 

#fit post gestione di coll, lin, outlier
library(car)
par(mforw=c(4,4))
avPlots(fit_clo) #PNG
par(mfrow=c(1,1)) 
#commento: in kilometer (e kilometer^2) l'oss 1516 sbarella e influenza fortemente la pendenza della retta: poiché evidentemente dopo un tot nr di km il prezzo non varia più. Quindi non ha senso tenerla perchè la retta così fatta non riflette la vera relazione. Proviamo a levarla per vedere la retta come cambia.  
#In Make si vede che (essendo contrasti con il reference di fascia bassa) l'evidenza di outlier (che non sono però influenti dalla prima toranta) è progressivaemnte più marcata più ci si allontata come fascia di prezzo di Make

fit_cloz <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + 
                Owner+Drivetrain+ Seating.Capacity + Location_G+Make_G+ PC1, data = r_z)
drop1(fit_cloz, test="F")
summary(fit_cloz)
library(lmtest)
resettest(fit_cloz, power = 2, type = "fitted",  data = r_z)
library(car)

par(mforw=c(4,4))
avPlots(fit_cloz) #PNG:fit_cloz_partialplots1/2/3.png
par(mfrow=c(1,1)) 

par(mfrow=c(2,2)) 
plot(fit_cloz) #PNG: fit_clo_diagnostic_plot
par(mfrow=c(1,1)) 
#diagnostici molto promettenti, non ci azzaridamo a togliere le obs con alto leverage perchè abbiamo pochi dati e non vogliamo inficiare l'analisi


#manca: model selection, etero, bootstrap, grafichini conclusivi comparativi vari, regressione logistica


#modelselection
library(MASS)
selectedMod <- step(fit_cloz, direction="both") 
#stepwise non toglie nulla quindi siamo a posto così

plot(r2_noinflu$Color) #controllino su color per concludere che in realtà ci sta tenerlo così #[LO ABBIAMO GIà TOLTO IN QUANTO NON SIGNIFICATIVO]
plot(r2_noinflu$Seller.Type) #controllino su sellertype: stepwise lo toglie, infatti sono tutti "individual", quindi lo togliamo (tra l'altro è quasi zero variance) #[LO ABBIAMO GIà TOLTO IN QUANTO NON SIGNIFICATIVO]
plot(r2_noinflu$Height) #togliamo anche height poiché nonostante la model lo salvi, non è significativo #[LO ABBIAMO GIà TOLTO IN QUANTO NON SIGNIFICATIVO]
plot(r2_noinflu$Owner) #pure questo lo teniamo  #[LO ABBIAMO GIà TOLTO IN QUANTO NON SIGNIFICATIVO]
#[GRAFICI SPOSTATI PIù SU]

fit_clozm <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + 
                 Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) #modello post model
summary(fit_clozm)
drop1(fit_clozm, test="F")
library(lmtest)
resettest(fit_clozm, power = 2, type = "fitted",  data = r_z) # non ancora specificato bene ma migliorato

#etero

plot(fit_clozm, which=1) 

library(car)
ncvTest(fit_clozm)
#ipotesi nulla: omoschedasticità quindi dal test risulta 
library(lmtest)
bptest(fit_clozm)
#da questo risulta palese l'etero
#risolviamo etero corrggendo gli se con white standard errors
library(lmtest)
library(sandwich)
coeftest(fit_clozm,vcov=vcovHC(fit_clozm)) #inferenza robusta (valida anche con etero)
summary(fit_clozm) #inferenza normale
#con coeftest le estimates sono indentiche ma standard error sono più grandi (quindi le 
#stime più variabili) infatti l'inferenza è differente e quella robusta è quella di coeftest
#Vediamo che ovviamente s.e robusto > s.e normale e che p-value robusto > (quindi peggio di) p-value normale; vediamo però che la situa della significatività è pressoché identica 

#plottiao il tutto
library(coefplot)
coefplot(fit_clozm, decreasing=T,sort="magnitude",intercept=F)
library(forestmodel)
forest_model(fit_clozm)
#dato che l'inferena robusta del forest boccia fuel type quasi in toto (l'unci lievemente significativo è LPG) proviamo a toglierla

fit_clozm3 <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Transmission + 
                  Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) #modello post model
resettest(fit_clozm3, power = 2, type = "fitted",  data = r_z) # miglioratissimo:
#  0.000000000000001238
#< 0.00000000000000022
#=> migliorato sia il numero puro (65 contro 76) ma anche un sacco il p-value!!!!

#proviamoa anchr senza km^2 che è solo lilevemente significativo
fit_clozm4 <- lm(ylog ~ Year + Kilometer + Transmission + 
                  Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) #modello post model
resettest(fit_clozm4, power = 2, type = "fitted",  data = r_z) 
#peggiora

#definitivo post etero
fit_clozme <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) #modello post model
summary(fit_clozme)
drop1(fit_clozme, test="F")
resettest(fit_clozme, power = 2, type = "fitted",  data = r_z) 
library(coefplot)
coefplot(fit_clozme, decreasing=T,sort="magnitude",intercept=F)
library(forestmodel)
forest_model(fit_clozme)

?gvlma

library(gvlma)
fit_clozme_xgvlma <- lm(ylog ~ Year + Kilometer + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z)
gvlma(fit_clozme_xgvlma)
library(Hmisc)
describe(r_z)


gvlma(fit_clozme_xgvlma) #decideremo se metterlo o no


#bootstrap

#bootstrap richiede righe complete, per qualche motivo ci sono 2 righe vuote interamente in r_z -> ne facciamo un altro r_z2
sapply(r_z, function(x)(sum(is.na(x)))) 
r_z2 <- na.omit(r_z)  ; nrow(r_z2); nrow(r_z)
sapply(r_z2, function(x)(sum(is.na(x)))) 

fit_clozme_2 <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z2) 

library(car)
BOOT.MOD=Boot(fit_clozme_2, R=1999)
summary(BOOT.MOD, high.moments=TRUE)

# confint boot
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")

#son tutte belle (= l'inferenza è robusta) tranne che un livello di owner (trascurabile) e km^2
#nonostante il reset migliori leggermente con dentro km^2, entrambe le verifiche robuste ce lo bocciao => lo togliamo

fit_finale <- lm(ylog ~ Year + Kilometer + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z2) 

summary(fit_finale)
drop1(fit_finale, test = "F")
resettest(fit_finale, power = 2, type = "fitted",  data = r_z2)

library(car)
BOOT.MOD=Boot(fit_finale, R=1999)
summary(BOOT.MOD, high.moments=TRUE)
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")

par(mfrow=c(2,2)) 
plot(fit_finale) #PNG: fit_clo_diagnostic_plot
par(mfrow=c(1,1)) 

gvlma(fit_finale) #decideremo se metterlo o no

plot(r_z2$ylog, fit_finale$fitted.values)


#REGRESSIONE LOGISTICA

#aggiustiamo il dataset (l'ultimo dal lineare)
r_rl90 <- r_z2 
r_rl90$Prezzo_eu_reverse <- exp(r_rl90$ylog) ; head(r_rl90)
r_rl0 <- r_rl90[,-14] ; head(r_rl0)

#decidiamo come binarizzare
library(Hmisc)
describe(r_rl0$Prezzo_eu_reverse)
plot(r_rl0$Prezzo_eu_reverse)

#Prendiamo come threshold per binarizzare Prezzo_eu la mediana, in quanto la media risente troppo degli outlier verso l'alto 

r_rl0$Prezzo_alto <- ifelse(r_rl0$Prezzo_eu_reverse > 9185, 1, 0) ; head(r_rl0) # 1 = sopra la mediana
#droppiamo Prezzo_eu non binario
r_rl1 <- r_rl0[,-14] ; head(r_rl1)

#usiamo il dataset con collin già risolta, e non abbiamo fattori o variabili continue con zero-varianca o near-zero-var, quindi l'unico problema può essere la separation o quasi-separation
#fit logistico con covariate dell'ultimo modello 
rl_0 <- glm(Prezzo_alto ~ Year + Kilometer + Transmission + Make_G + 
              Owner + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #warning che suggerisce separation (su Make_G, col senno di poi)
summary(rl_0)
drop1(rl_0, test = "LRT") 

table(r_rl1$Prezzo_alto) #freq assolute
prop.table(table(r_rl1$Prezzo_alto)) #freq relative

print(rl_0$coefficients)
exp(rl_0$coefficients) #odds ratio delle covariate : STRANO MAKE_G (e anche PC1 OR molto alto...)
exp(confint(rl_0))

#quasi separation/separation per Make_G

table(r_rl1$Prezzo_alto, r_rl1$Make_G)
#ecco il problema: Make_G soffre di spearation nei livelli 3,4,5: proprio quelli non significativi sul summary e con OR estremi
#soluzione: droppiamo Make_G in quanto praticamentenè una regola classificativi deterministica (in realtà si potrebbe fare grouping in 2 soli livelli...)

table(r_rl1$Prezzo_alto, r_rl1$PC1)
describe(r_rl1$PC1)

table(r_rl1$Prezzo_alto, r_rl1$Year) #vediamo che Year invece è a posto
table(r_rl1$Prezzo_alto, r_rl1$Location) #perfetta
table(r_rl1$Prezzo_alto, r_rl1$Owner) #siccome nel dataset pulito presumiamo che siano stati tolte alcune obs che avevano Owner =4 o 4+, per tenere owner modifichiamo la varaibile togliendo i livelli incriminati

# droppiamo i livelli non usati di Owner (con 0 osservazioni)
library(MASS)
r_rl1$Owner <- droplevels(r_rl1$Owner) # droppiamo i livelli non usati (con 0 osservazioni)

table(r_rl1$Prezzo_alto, r_rl1$Seating.Capacity) #perfetto
table(r_rl1$Prezzo_alto, r_rl1$Drivetrain) #perfetto
table(r_rl1$Prezzo_alto, r_rl1$Transmission) #perfetto

rl_1 <- glm(Prezzo_alto ~ Year + Kilometer + Transmission + 
              Owner + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #gira
summary(rl_1) #bene

library(forestmodel)
print(forest_model(rl_1),text_size = 5) 

#togliamo Owner in quanto significativo solo un livello e al 10% e al limite proprio, quindi si leva

rl_2 <- glm(Prezzo_alto ~ Year + Kilometer + Transmission 
               + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #gira
summary(rl_2)
# vediamo che kilometer controintuitivamente non è molto significativo: spiegazione: presupponiamoc he essendo macchine usate, sia sopra che sotto la mediana del prezzo la variabilità di km sia più o meno la stessa (di fatto è omoschedastico rispetto al prezzo)

exp(rl_2$coefficients) #notiamo che l'odds ratio di kilometer (dato da exp(beta_km)) è =ca. 1 => togliamo km 
exp(confint(rl_2))

rl_3 <- glm(Prezzo_alto ~ Year  + Transmission 
            + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #gira
summary(rl_3)
drop1(rl_3, test = "LRT")

library(forestmodel)
print(forest_model(rl_3),text_size = 5) 

#rimane il dubbio su PC1: è molto significativa e ha senso, ma non si capisce la sua situa di separation

table(r_rl1$Prezzo_alto, r_rl1$PC1) #dice poco, troppo disaggregato
plot(r_rl1$PC1, r_rl1$Prezzo_alto) #dal plot non sembra esserci separation => proviamo a tenerla nel modello

#OR e confint del mod finale

a <- exp(rl_3$coefficients) 
b <- exp(confint(rl_3))
tab_OR <- cbind("OR"=a,b); tab_OR
round(tab_OR, digits=2)

#plots
par(mfrow=c(2,2)) 
plot(rl_3)
par(mfrow=c(1,1)) 

#ricaviamo l'R^2 

null = glm(Prezzo_alto ~ 1, data =r_rl1  ,family = binomial) #modello costante
summary(null) #la devianza residua COINCIDE CON LA DEVIANZA TOTALE DA SPIEGARE 
#Residual deviance: 2446.8  on 1764  degrees of freedom = deivanza totale da spiegare

ls(null)
dev_tot <- null$deviance # devianza totale (da spiegare)

ls(rl_3)
dev_residua <- rl_3$deviance #devianza residua del modello in questione

R2 <- 1 - (dev_residua/dev_tot) ; R2
#0.8036409

#faccamo un'altra logistica per curiosità per poi fare un confronto

rl_4 <- glm(Prezzo_alto ~ Year  + Transmission 
            + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) 
anova(rl_4, rl_3, test="LRT") #meglio il 3 con drivetrain

#proviamo a decidere usando aic e bic

npar.all=c(length(rl_3$coefficients),length(rl_4$coefficients))
aic.all=c(rl_3$aic, rl_4$aic) #aic del modello completo rl_3 vs aic del modello ridotto rl_4 (senza drivetrain)
bic.all = aic.all-2*npar.all+npar.all*log(nrow(r_rl1)) #bic del modello completo rl_3 vs aic del modello ridotto rl_4 (senza drivetrain)
aic.all #rl_3 minimizza il aic
bic.all #rl_4 minimizza il bic (che infatti penalizza di più per il numero di covariate)

#il test anova concorda con l'aic => teniamo drivetrain

#proviamo a togliere PC1 in quanto possibilmente problematica per separation
rl_5 <- glm(Prezzo_alto ~ Year  + Transmission + Drivetrain 
            + Seating.Capacity + Location_G, data = r_rl1, family = binomial) 
anova(rl_5, rl_3, test="LRT") #meglio tenerla

npar.all2=c(length(rl_3$coefficients),length(rl_5$coefficients))
aic.all2=c(rl_3$aic, rl_5$aic) #aic del modello completo rl_3 vs aic del modello ridotto rl_4 (senza drivetrain)
bic.all2 = aic.all2-2*npar.all2+npar.all2*log(nrow(r_rl1)) #bic del modello completo rl_3 vs aic del modello ridotto rl_4 (senza drivetrain)
aic.all2 #rl_3 minimizza il aic
bic.all2 #rl_4 minimizza il bic (che infatti penalizza di più per il numero di covariate)
#confermato, prevedibilmente: meglio tenerlo

rl_finale <- glm(Prezzo_alto ~ Year  + Transmission + Drivetrain 
                 + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial)

