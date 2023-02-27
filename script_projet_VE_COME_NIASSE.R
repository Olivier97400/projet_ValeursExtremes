library(ismev)
library(fExtremes)
library(evd)

load("C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\projet_ValeursExtremes\\donneesVagues.RData")
load("C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\projet_ValeursExtremes\\donneesStations.RData")
data1 <- donneesVague
data2 <- buoysInfos
attach(data1)

str(donneesVague)

summary(donneesVague)

#Approche GEV
# SA est de taille 464280 on va utiliser une taille de bloc n = 159
# dans chaque bloc on aura 2920 données
SA = station2
li_divisor = c()
s = length(SA)
for(d in 1:s){
  if(s%%d == 0){
    li_divisor = c(li_divisor, d)
  }
}
print(li_divisor)
#SA = data.frame(SA)

n=2920

SA<-station2


max_SA<-rep(0,length(SA)/n)
cat("le nombre de bloc est ",length(SA)/n,"\n")

# methode par blocs
for(i in 1:(length(SA)/n)){ max_SA[i]<-max(SA[((i-1)*n+1):(i*n)])}
maxfit<-gev.fit(max_SA)
plot(max_SA)

plot(SA)

# Pour affciher le quantile plot
gev.diag(maxfit)


# niveau de retour associé à la période de retour 100
T=100
q=1/T
dep100=fgev(max_SA,prob=q)
cat(" le montant qui sera depasse tous les 100 ans est ", dep100$estimate[1])

#IC 95%
cat(
  " l'intervalle de confiance de l'estimateur du depassement de 100 ans est:"
  ,"\n","[" ,dep100$estimate[1]- 1.96*dep100$std.err[1],"," 
  ,dep100$estimate[1]+ 1.96*dep100$std.err[1],"]","\n"
  )

# niveau de retour associé à la période de retour 500
T=500
q=1/T
dep500=fgev(max_SA,prob=q)
cat(" le montant qui sera depasse tous les 500 ans est ", dep500$estimate[1])

#IC 95%
cat(
  " l'intervalle de confiance de l'estimateur du depassement de 500 ans est:",
  "\n","[" ,dep500$estimate[1]- 1.96*dep500$std.err[1],"," ,
  dep500$estimate[1]+ 1.96*dep500$std.err[1],"]","\n"
    )

# niveau de retour associé à la période de retour 1000
T=1000
q=1/T
dep1000=fgev(max_SA,prob=q)
cat(" le montant qui sera depasse tous les 1000 ans est ", dep1000$estimate[1])

#IC 95%
cat(
  " l'intervalle de confiance de l'estimateur du depassement de 100 ans est:",
  "\n","[" ,dep1000$estimate[1]- 1.96*dep1000$std.err[1],"," ,
  dep1000$estimate[1]+ 1.96*dep1000$std.err[1],"]","\n"
    )

# Approche GPD
mrlplot(SA)

th1=2.7
sagpd1=fpot(SA,threshold=th1)
sagpd1

par(mfrow=c(2,2))
plot(sagpd1)

th=3.6
sagpd=fpot(SA,threshold=th)
sagpd

par(mfrow=c(2,2))
plot(sagpd)

# niveau de retour associé à la période de retour 100
T=100
q=1/T
depas100=fpot(SA,threshold=th,npp=1,mper = 100*n, std.err = FALSE)
cat(
  " le montant qui sera depasse tous les 100 ans est ", depas100$estimate[1],
  "par l'autre methode on avait ",dep100$estimate[1]
  )

# niveau de retour associé à la période de retour 500
depas500=fpot(SA,threshold=th,npp=1,mper = 500*n,std.err = FALSE)
cat(
  " le montant qui sera depasse tous les 500 ans est ", depas500$estimate[1],
  "par l'autre methode on avait",dep500$estimate[1]
  )
# niveau de retour associé à la période de retour 1000
depas1000=fpot(SA,threshold=th,npp=1,mper = 1000*n,std.err = F)
cat(
  " le montant qui sera depasse tous les 1000 ans est  ", depas1000$estimate[1],
  "par l'autre methode on avait",dep1000$estimate[1]
  )

# Partie II bivariée
SB = station9
li_divisor = c()
s = length(SA)
for(d in 1:s){
  if(s%%d == 0){
    li_divisor = c(li_divisor, d)
  }
}
print(li_divisor)

n=2920
SA<-station2
SB=station9
SASB=cbind(SA,SB)

max_SA<-rep(0,length(SA)/n)
max_SB<-rep(0,length(SB)/n)
cat("le nombre de bloc est ",length(SA)/n,"\n")
cat("le nombre de bloc est ",length(SB)/n,"\n")

for(i in 1:(length(SA)/n)){ max_SA[i]<-max(SA[((i-1)*n+1):(i*n)])
max_SB[i]<-max(SB[((i-1)*n+1):(i*n)])}

maxSASB=data.frame(max_SA,max_SB)
par(mfrow=c(1,2))
plot(max_SB,main= "maxima de SB", col='red' )
plot(max_SA,main= "maxima de SA" , col='blue')

#Question 2. Effectuons un ajustement par une loi GEV sur chacune des 2 marginales.
# On récupéré les paramétres pour chaque ajustement des marginales.
gev_SA=gev.fit(max_SA)
gev_SB=gev.fit(max_SB)
summary(gev_SA)


parametre_SA=c(gev_SA$mle[1],gev_SA$mle[2],gev_SA$mle[3])
parametre_SB=c(gev_SB$mle[1],gev_SB$mle[2],gev_SB$mle[3])

parametres<-data.frame(parametre_SA,parametre_SB)
rownames(parametres)=c("mu","sigma","gamma")
colnames(parametres)=c("parametre SA","parametre SB")
parametres

# On calcul maintenant les quantiles extreme zp
# on calcule le denominateur
# correspond à p(Y > y)
model_Y=pgev(
  4,loc=gev_SB$mle[1], scale = gev_SB$mle[2], shape =gev_SB$mle[3], 
  lower.tail = FALSE
  )

# on recupere les parametres utile pour calculer le numerateur 
gevfb_SAB= fbvevd(maxSASB)

summary(gevfb_SAB)
#gevfb_SB = fbvevd(max_SB)

# Correspond P(X > zp inter Y > y)
model_X_Y= pbvevd(c(1,4), dep = 0.70145, 
                  mar1 = c(loc1=1.93762, scale1=0.62531, shape1=-0.11469),
                  mar2=c(loc2=3.39860, scale2=0.94585, shape2=-0.08401)
                  )


# nuage points bivarié
plot(maxSASB)

# Selection du meilleur modèle avec critère Aic
a=c("log", "alog")
aic= rep(0,length(a))
for(i in  a){
  print(i)
  print( fbvevd(maxSASB, model = i)$estimate)
  aic[i]=AIC(fbvevd(maxSASB, model = i))
}
aic


# le model logistique qui a le plus faible AIC donc c'est le meilleur model 
#ajustons le

mod_log <- fbvevd(maxSASB, model = "log")
mod_log
par(mfrow=c(3,2))
plot(mod_log)


# Comparer les 2 modèles et discuter de la dépendance présente dans 
#les extrémes


nllh_model=data.frame("log"=(-AIC(mod_log)+2*3)/2,"alog"=(-AIC(mod_alog)+2*3)/2)
row.names(nllh_model)=c("a")
nllh_model

# Partie Analyse des stations SA et SC
# distance euclidienne
# plus proche de SA que de S9
sqrt((data2$lon[2] - data2$lon[7])^2 + (data2$lat[2] - data2$lat[7])^2)
sqrt((data2$lon[7] - data2$lon[9])^2 + (data2$lat[7] - data2$lat[9])^2)


# nuage de points de SA et SC
n=2920
SA<-station2
SC=station7 # SC est la station 7
SASC=cbind(SA,SC)

max_SA<-rep(0,length(SA)/n)
max_SC<-rep(0,length(SC)/n)
cat("le nombre de bloc est ",length(SA)/n,"\n")

for(i in 1:(length(SA)/n)){ max_SA[i]<-max(SA[((i-1)*n+1):(i*n)])
max_SC[i]<-max(SC[((i-1)*n+1):(i*n)])}

maxSASC=data.frame(max_SA,max_SC)
par(mfrow=c(1,2))
plot(max_SC,main= "maxima de SC" , col='magenta')
plot(max_SA,main= "maxima de SA", col = 'deepskyblue')

# nuage de points bivarié
plot(maxSASC)

# Selection du meilleur modèle avec critère Aic
a=c("log", "alog")
aic= rep(0,length(a))
for(i in  a){
  print(i)
  print( fbvevd(maxSASC, model = i)$estimate)
  aic[i]=AIC(fbvevd(maxSASC, model = i))
}
aic


#c le model logistique qui a la plus faible AIC donc c est le meilleur model 
#ajustons le

mod_log <- fbvevd(maxSASC, model = "log")
mod_log
par(mfrow=c(3,2))
plot(mod_log)

# Partie Analyse des stations SB et SD
# distance euclidienne
# plus proche de SB que de SA
sqrt((data2$lon[4] - data2$lon[9])^2 + (data2$lat[4] - data2$lat[9])^2)
sqrt((data2$lon[2] - data2$lon[4])^2 + (data2$lat[2] - data2$lat[4])^2)

# nuage de points de SB et SD
n=2920
SB<-station9
SD=station4 # SD est la station 4
SBSD=cbind(SB,SD)

max_SB<-rep(0,length(SB)/n)
max_SD<-rep(0,length(SD)/n)
cat("le nombre de bloc est ",length(SB)/n,"\n")

for(i in 1:(length(SB)/n)){ max_SB[i]<-max(SB[((i-1)*n+1):(i*n)])
max_SD[i]<-max(SD[((i-1)*n+1):(i*n)])}

maxSBSD=data.frame(max_SB,max_SD)
par(mfrow=c(1,2))
plot(max_SD,main= "maxima de SD" , col='orange')
plot(max_SB,main= "maxima de SB", col = 'chartreuse3')

# nuage de points bivarié
plot(maxSBSD)

# Selection du meilleur modèle avec critère Aic
a=c("log", "alog")
aic= rep(0,length(a))
for(i in  a){
  print(i)
  print( fbvevd(maxSBSD, model = i)$estimate)
  aic[i]=AIC(fbvevd(maxSBSD, model = i))
}
aic


#c le model logistique qui a la plus faible AIC donc c est le meilleur model 
#ajustons le

mod_log <- fbvevd(maxSBSD, model = "log")
mod_log
par(mfrow=c(3,2))
plot(mod_log)
