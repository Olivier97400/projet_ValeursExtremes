# Exercice 1
library(ismev)
dataCars = read.csv(
  'C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\TP1\\claimsCars.csv'
  )
# simuGEV=gev.fit(dataCars)
# Methode par bloc
# séparer le dataset cars en 9 bloc de taille 71 car 9*71 = 639
dataCars[72:142, 1]

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

vIndexMaxBloc = GetIndexMaxBlock(dataCars, 9)
listOfBlock = GetListOfBloc(dataCars, vIndexMaxBloc)

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

GetIndexOfMax = function(df, v_max){
  vIndexOfMax = c()
  for (e in v_max) {
    ind = match(e, df[,1])
    vIndexOfMax = c(vIndexOfMax, ind)
  }
  return(vIndexOfMax)
}

ind_max = GetIndexOfMax(dataCars, VecOfMax)

plot(dataCars[,1])
abline(v = vIndexMaxBloc, col="red", lwd=1.5, lty=1)

points(ind_max, dataCars[ind_max,1], pch = c(16), col='blue')

# Methode PLOT
valUnderThreshold = function(threshold){
  
}