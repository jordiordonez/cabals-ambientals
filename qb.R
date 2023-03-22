library(dplyr)
library(lubridate)

file="sample.csv"
Year_init=2011
Year_final=2020
Max_order=100

flows<-read.csv(file, header=TRUE,sep=',')


flows<-flows %>% 
  mutate(Year = year(as.Date(Fecha,"%d/%m/%Y")),
         Media=as.numeric(sub(",",".",flows$Media))) %>%
  select(Year,Media)



min_value<- function (Year,order,year_days,flows){
  means_order=NULL
  Last_day=year_days-order+1
  for (Day in 1:Last_day){
    End_day=Day+order-1
    means_order[Day]=mean(flows[flows$Year==Year,]$Media[Day:End_day])}
  min_value<-min(means_order)
  return(min_value)
}

Qb_Year<- function (min_values){
  for (order in 1:(Max_order-1)){
    if (min_values[order]!=0){
      ratio[order]=(min_values[order+1]-min_values[order])/min_values[order]}}
  Qb_Year=min_values[which((ratio==max(ratio[!is.na(ratio)])))+1]
    return(Qb_Year)
}

Qb<- function(flows,Year_init,Year_final){
  Qb_Years=NULL
  for (Year in Year_init:Year_final){
    ratio=NULL
    min_values=NULL  
    year_days=sum(flows$Year==Year)
    for (order in 1:Max_order){
      min_values[order]=min_value(Year,order,year_days,flows)}
    Qb_Years[Year-(Year_init)+1]=Qb_Year(min_values)
  }
  
  Qb=mean(Qb_Years)
  return(Qb)
}

Qb_value=Qb(flows,Year_init,Year_final)  
print(paste('Base flow is ',Qb_value,'m3/s'))
