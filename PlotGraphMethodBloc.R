load(
  "C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\projet_test\\donneesStations.RData"
)
load(
  "C:\\Users\\olivi\\Documents\\Montpellier\\M2_SSD\\semestre2\\Valeurs_extremes\\projet_test\\donneesVagues.RData"
)
data1 <- donneesVague
data2 <- buoysInfos
attach(data1)

# SA est de taille 464280 on va utiliser une taille de bloc n = 159
# dans chaque bloc on aura 2920 données
SA = station2
SA = data.frame(SA)

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