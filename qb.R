library(dplyr)
library(lubridate)

cabals<-read.csv("sample.csv", header=TRUE,sep=',')

#Recuperem els anys per obtenir valors diaris consecutius per anys 
cabals<-mutate(cabals,any = year(as.Date(Fecha,"%d/%m/%Y")))

#Eliminem la columna Fecha 
cabals<-cabals[,-c(1)]

#Fem que Media siguin considerats com a numèrics després de substituir la , per .
cabals<-mutate(cabals,Media=as.numeric(sub(",",".",cabals$Media)))

#Per cada j=1 a 100 Creo un resum amb mitjanes mòbils d'ordre j
#Per cada any i escullo els mínims per cada any per aquestes mitjanes d'ordre j 
#Calculo la mitjana en un vector vs
Qba=NULL
for (an in 2011:2020){
  vr=NULL
  vj=NULL  
  vs=NULL
  for (j in 1:100){
    v=NULL
    ultim=365-j+1
    for (dia in 1:ultim){
      diafi=dia+j-1
      v[dia]=mean(cabals[cabals$any==an,]$Media[dia:diafi])}
    vs[j]=min(v)}
  for (j in 1:99){
    if (vs[j]!=0){vr[j]=(vs[j+1]-vs[j])/vs[j]}}
    
  #Calculem el màxim de les variacions relatives
  m=max(vr[!is.na(vr)])
  #Recuperem el màxim dels valors de mitjanes pels quals hem trobat aquest màxim
  posicio=which((vr==m))
  Qba[an-2010]=vs[posicio+1]
  }

Qb=mean(Qba)
  
print(paste('El cabal base és',Qb,'m3/s'))