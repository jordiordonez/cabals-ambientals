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




Qb_Year=NULL
for (Year in Year_init:Year_final){
  ratio=NULL
  min_values=NULL  
  year_days=sum(flows$Year==Year)
  for (order in 1:Max_order){
    means_order=NULL
    Last_day=year_days-order+1
    for (Day in 1:Last_day){
      End_day=Day+order-1
      means_order[Day]=mean(flows[flows$Year==Year,]$Media[Day:End_day])}
    min_values[order]=min(means_order)}
  for (order in 1:(Max_order-1)){
    if (min_values[order]!=0){
      ratio[order]=(min_values[order+1]-min_values[order])/min_values[order]}}
    


  Qb_Year[Year-(Year_init)+1]=
    min_values[which((ratio==max(ratio[!is.na(ratio)])))+1]
  }

Qb=mean(Qb_Year)
  
print(paste('Base flow is ',Qb,'m3/s'))
