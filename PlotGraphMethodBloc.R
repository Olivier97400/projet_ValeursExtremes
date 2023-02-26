library(ismev)
library(fExtremes)
library(evd)

load("C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\projet_ValeursExtremes\\donneesVagues.RData")
load("C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\projet_ValeursExtremes\\donneesStations.RData")
data1 <- donneesVague
data2 <- buoysInfos
attach(data1)
#str(buoysInfos)
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
#SB<-station2

max_SA<-rep(0,length(SA)/n)
cat("le nombre de bloc est ",length(SA)/n,"\n")

for(i in 1:(length(SA)/n)){ max_SA[i]<-max(SA[((i-1)*n+1):(i*n)])}
maxfit<-gev.fit(max_SA)
#
plot(max_SA)

plot(SA)


gev.diag(maxfit)


# niveau de retour associé à la période de retour 100
T=100
q=1/T
dep100=fgev(max_SA,prob=q)
cat(" le montant qui sera depasser tous les 100 ans est ", dep100$estimate[1])

#IC à 95%
cat(
  " l'intervalle de confiance à 95%  de l'estimateur  du depassement de 100 ans  est:"
  ,"\n","[" ,dep100$estimate[1]- 1.96*dep100$std.err[1],"," 
  ,dep100$estimate[1]+ 1.96*dep100$std.err[1],"]","\n"
  )

# niveau de retour associé à la période de retour 500
T=500
q=1/T
dep500=fgev(max_SA,prob=q)
cat(" le montant qui sera depasser tous les 500 ans est ", dep500$estimate[1])

#IC à 95%
cat(
  " l'intervalle de confiance à 95%  de l'estimateur  du depassement de 500 ans  est:",
  "\n","[" ,dep500$estimate[1]- 1.96*dep500$std.err[1],"," ,
  dep500$estimate[1]+ 1.96*dep500$std.err[1],"]","\n"
    )

# niveau de retour associé à la période de retour 1000
T=1000
q=1/T
dep1000=fgev(max_SA,prob=q)
cat(" le montant qui sera depasser tous les 1000 ans est ", dep1000$estimate[1])

#IC à 95%
cat(
  " l'intervalle de confiance à 95%  de l'estimateur  du depassement de 100 ans  est:",
  "\n","[" ,dep1000$estimate[1]- 1.96*dep1000$std.err[1],"," ,
  dep1000$estimate[1]+ 1.96*dep1000$std.err[1],"]","\n"
    )

# Approche GPD
mrlplot

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
  " le montant qui sera depasser tous les 100 ans est ", depas100$estimate[1],
  "par l'autre methode on avait ",dep100$estimate[1]
  )

# niveau de retour associé à la période de retour 500
depas500=fpot(SA,threshold=th,npp=1,mper = 500*n,std.err = FALSE)
cat(
  " le montant qui sera depasser tous les 500 ans est ", depas500$estimate[1],
  "par lautre methode on avais ",dep500$estimate[1]
  )
# niveau de retour associé à la période de retour 1000
depas1000=fpot(SA,threshold=th,npp=1,mper = 1000*n,std.err = F)
cat(
  " le montant qui sera depasser tous les 1000 ans est  ", depas1000$estimate[1],
  "par l'autre methode on avais ",dep1000$estimate[1]
  )

# Cette fonction renvoie les indices des
# valeurs maximales des blocs endroit où on met les lignes rouges
GetIndexMaxBlock = function(df, m){
  d = dim(df)[1]
  n = d/m
  vIndexMaxBloc = c()
  comp = n
  while (comp <= d) {
    vIndexMaxBloc = c(vIndexMaxBloc,comp)
    comp = comp+n
  }
  return(vIndexMaxBloc)
}

# Cette fonction récupère les data
# de chaque blocs
GetListOfBloc = function(df, li){
  Data = df
  j = 1
  k = 1
  l_block = list()
  for(e in li){
    v_block = c()
    for (i in j:e) {
      v_block = c(v_block, Data[i,1])
    }
    l_block[[k]] = v_block 
    j = e+1
    k = k+1
  }
  return(l_block)
}

vIndexMaxBloc = GetIndexMaxBlock(SA, 159)
listOfBlock = GetListOfBloc(SA, vIndexMaxBloc)


# Cette fonction récupère les valeurs maximales
# de chaque blocs
GetListOfMax = function(liste){
  v_max = c()
  for (i in 1:(length(liste))) {
    vi = liste[[i]]
    max_val = max(vi)
    v_max = c(v_max, max_val)
  }
  return(v_max)
}

VecOfMax = GetListOfMax(listOfBlock)

# Cette fonction récupère l'indexe des valeurs maximales
# de chaque blocs
GetIndexOfMax = function(df, v_max){
  vIndexOfMax = c()
  for (e in v_max) {
    ind = match(e, df[,1])
    vIndexOfMax = c(vIndexOfMax, ind)
  }
  return(vIndexOfMax)
}

ind_max = GetIndexOfMax(SA, VecOfMax)

plot(SA[,1])
abline(v = vIndexMaxBloc, col="red", lwd=1.5, lty=1)

points(ind_max, SA[ind_max,1], pch = c(16), col='blue')

# Methode PLOT
#valUnderThreshold = function(threshold){
#}