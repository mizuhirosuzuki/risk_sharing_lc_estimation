setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))

#CREATE VECTORS OF INEQUALITY MEASURES USING DATA FROM ALL THREE VILLAGES AS MEASURED BY THE VARIANCE OF LOG CONSUMPTION
load("Preds1")
avlogc=numeric()
avlogcpnh=numeric()
avlogcp=numeric()
avlogclcnh=numeric()
avlogclc=numeric()
avlogc[1:6]=vlogc1
avlogcpnh[1:5]=vlogcpnh1
avlogcp[1:5]=vlogcp1
avlogclcnh[1:5]=vlogclcnh1
avlogclc[1:5]=vlogclc1
load("Preds2")
avlogc[7:12]=vlogc2
avlogcpnh[7:11]=vlogcpnh2
avlogcp[7:11]=vlogcp2
avlogclcnh[7:11]=vlogclcnh2
avlogclc[7:11]=vlogclc2
load("Preds3")
avlogc[13:18]=vlogc3
avlogcpnh[13:17]=vlogcpnh3
avlogcp[13:17]=vlogcp3
avlogclcnh[13:17]=vlogclcnh3
avlogclc[13:17]=vlogclc3

avlogc2=avlogc
avlogc2[1]=NA
avlogc2[7]=NA
avlogc2[13]=NA

avlogc3=avlogc[2:18]

#COMPUTE AVERAGE INEQUALITY AS MEASURED MY THE VARIANCE OF LOG CONSUMPTION

mean(avlogc2,na.rm=TRUE)
mean(avlogcpnh,na.rm=TRUE)
mean(avlogcp,na.rm=TRUE)
mean(avlogclcnh,na.rm=TRUE)
mean(avlogclc,na.rm=TRUE)

#COMPUTE THE CORRELATIONS BETWEEN TRUE AND PREDICTED INEQUALITY AS MEASURED MY THE VARIANCE OF LOG CONSUMPTION
     
cor(avlogc3,avlogcpnh,use="complete.obs")
cor(avlogc3,avlogcp,use="complete.obs")
cor(avlogc3,avlogclcnh,use="complete.obs")
cor(avlogc3,avlogclc,use="complete.obs")

#COMPUTE THE CORRELATIONS BETWEEN CHANGES IN TRUE INEQUALITY AND CHANGES IN PREDICTED INEQUALITY AS MEASURED MY THE VARIANCE OF LOG CONSUMPTION

Dvlogc=numeric()
Dvlogcpnh=numeric()
Dvlogcp=numeric()
Dvlogclcnh=numeric()
Dvlogclc=numeric()
for (i in 1:17) {
  Dvlogc[i]=avlogc[i+1]-avlogc[i]
  Dvlogcpnh[i]=avlogcpnh[i]-avlogc[i]
  Dvlogcp[i]=avlogcp[i]-avlogc[i]
  Dvlogclcnh[i]=avlogclcnh[i]-avlogc[i]
  Dvlogclc[i]=avlogclc[i]-avlogc[i]
}

cor(Dvlogc,Dvlogcpnh,use="complete.obs")
cor(Dvlogc,Dvlogcp,use="complete.obs")
cor(Dvlogc,Dvlogclcnh,use="complete.obs")
cor(Dvlogc,Dvlogclc,use="complete.obs")

#CREATE VECTORS OF INEQUALITY MEASURES USING DATA FROM ALL THREE VILLAGES AS MEASURED BY THE GINI INDEX

load("Preds1")
agini=numeric()
aginipnh=numeric()
aginip=numeric()
aginilcnh=numeric()
aginilc=numeric()
agini[1:6]=gini1
aginipnh[1:5]=pginipnh1
aginip[1:5]=pginip1
aginilcnh[1:5]=pginilcnh1
aginilc[1:5]=pginilc1
load("Preds2")
agini[7:12]=gini2
aginipnh[7:11]=pginipnh2
aginip[7:11]=pginip2
aginilcnh[7:11]=pginilcnh2
aginilc[7:11]=pginilc2
load("Preds3")
agini[13:18]=gini3
aginipnh[13:17]=pginipnh3
aginip[13:17]=pginip3
aginilcnh[13:17]=pginilcnh3
aginilc[13:17]=pginilc3

agini2=agini
agini2[1]=NA
agini2[7]=NA
agini2[13]=NA
agini3=agini[2:18]

#COMPUTE AVERAGE INEQUALITY AS MEASURED MY THE GINI INDEX

mean(agini2,na.rm=TRUE)
mean(aginipnh,na.rm=TRUE)
mean(aginip,na.rm=TRUE)
mean(aginilcnh,na.rm=TRUE)
mean(aginilc,na.rm=TRUE)

#COMPUTE THE CORRELATIONS BETWEEN TRUE AND PREDICTED INEQUALITY AS MEASURED MY THE GINI INDEX

cor(agini3,aginipnh,use="complete.obs")
cor(agini3,aginip,use="complete.obs")
cor(agini3,aginilcnh,use="complete.obs")
cor(agini3,aginilc,use="complete.obs")

#COMPUTE THE CORRELATIONS BETWEEN CHANGES IN TRUE INEQUALITY AND CHANGES IN PREDICTED INEQUALITY AS MEASURED MY THE GINI INDEX

Dgini=numeric()
Dginipnh=numeric()
Dginip=numeric()
Dginilcnh=numeric()
Dginilc=numeric()
for (i in 1:17) {
  Dgini[i]=agini[i+1]-agini[i]
  Dginipnh[i]=aginipnh[i]-agini[i]
  Dginip[i]=aginip[i]-agini[i]
  Dginilcnh[i]=aginilcnh[i]-agini[i]
  Dginilc[i]=aginilc[i]-agini[i]
}

cor(Dgini,Dginipnh,use="complete.obs")
cor(Dgini,Dginip,use="complete.obs")
cor(Dgini,Dginilcnh,use="complete.obs")
cor(Dgini,Dginilc,use="complete.obs")

save.image("PredsIneq")
