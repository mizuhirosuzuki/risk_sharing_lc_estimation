setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))
load("Preds1")

t(xPrsnhme)
t(xPrsme_nog)
t(xLcnh1incHme)
t(xLc1incHme_nog)

#GENERATING AFTER-TAX-AND-TRANSFER INCOMES
tau=0.3
incdatb2=(1-tau)*incdatb
meanincdat=numeric()
meanincdat2=numeric()
for (t in 1:tnum) {
  meanincdat[t]=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),t])
  meanincdat2[t]=mean(incdatb2[(hnumv0+1):(hnumv0+hnumv),t])
}
meanincdat
meanincdat2

for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (t in 1:tnum) {
    incdatb2[i,t]=incdatb2[i,t]+meanincdat[t]-meanincdat2[t]
  }}

for (t in 1:tnum) {
  meanincdat2[t]=mean(incdatb2[(hnumv0+1):(hnumv0+hnumv),t])
}
meanincdat2

poor=hlmean
rich=hlmean-1
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  if (poor[i]==2) {
    poor[i]=NA
  } else {
    rich[i]=NA
  }}

#COMPUTING DESCRIPTIVE STATISTICS

mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(incdatb2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(incdatb2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
(mean(incdatb2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
)/mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(log(incdatb[(hnumv0+1):(hnumv0+hnumv)])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(log(incdatb2[(hnumv0+1):(hnumv0+hnumv)])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(log(incdatb2[(hnumv0+1):(hnumv0+hnumv)])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(log(incdatb[(hnumv0+1):(hnumv0+hnumv)])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(incdatb2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(incdatb2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
(mean(incdatb2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE))/mean(incdatb[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(log(incdatb[(hnumv0+1):(hnumv0+hnumv)])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(log(incdatb2[(hnumv0+1):(hnumv0+hnumv)])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(log(incdatb2[(hnumv0+1):(hnumv0+hnumv)])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(log(incdatb[(hnumv0+1):(hnumv0+hnumv)])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

predconslcnh2=(1-tau)*predconslcnh
predconslc2=(1-tau)*predconslc
consdat22=(1-tau)*consdat
meanpredconslcnh=numeric()
meanpredconslcnh2=numeric()
meanpredconslc=numeric()
meanpredconslc2=numeric()
meanconsdat=numeric()
meanconsdat22=numeric()
for (t in 1:(tnum-1)) {
  meanpredconslcnh[t]=mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv),t])
  meanpredconslcnh2[t]=mean(predconslcnh2[(hnumv0+1):(hnumv0+hnumv),t])
  meanpredconslc[t]=mean(predconslc[(hnumv0+1):(hnumv0+hnumv),t])
  meanpredconslc2[t]=mean(predconslc2[(hnumv0+1):(hnumv0+hnumv),t])
  meanconsdat[t]=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
  meanconsdat22[t]=mean(consdat22[(hnumv0+1):(hnumv0+hnumv),t])
}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (t in 1:(tnum-1)) {
    predconslcnh2[i,t]=predconslcnh2[i,t]+meanpredconslcnh[t]-meanpredconslcnh2[t]
    predconslc2[i,t]=predconslc2[i,t]+meanpredconslc[t]-meanpredconslc2[t]
    consdat22[i,t]=consdat22[i,t]+meanconsdat[t]-meanconsdat22[t]
  }}

mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(consdat22[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(consdat22[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
(mean(consdat22[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
)/mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(consdat22[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(consdat22[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
(mean(consdat22[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE))/mean(consdat[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslcnh2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslcnh2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslcnh2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslcnh2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(predconslc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslc2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslc2[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(predconslc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(predconslc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslc2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(predconslc2[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-mean(predconslc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)/mean(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
min(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
max(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)/mean(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
min(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
max(sigPrs[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(sigLc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigLc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigLc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)/mean(sigLc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
min(sigLc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
max(sigLc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

mean(sigLc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigLc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
sd(sigLc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)/mean(sigLc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
min(sigLc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
max(sigLc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

#RESTIMATING THE INCOME PROCESSES

Lincdatb2=array(NA,c(hnum,tnum))
for (i in 1:hnum) {
  for (j in 1:(tnum-1)) {
    Lincdatb2[i,j+1]=incdatb2[i,j]
  }}

## low mean low cv

incdat2l=numeric()
consdat2l=numeric()
Lincdat2l=numeric()
tcode2=numeric()
hcode2=numeric()
for (i in 1:hnum) {
  for (j in 1:tnum) {
    if (villagedat[i,j]==kk) {
      if ((incdat[i,j]>low1) & (incdat[i,j]<high1)) {
        incdat2l[tnum*(i-1)+j]=incdatb2[i,j]
        consdat2l[tnum*(i-1)+j]=consdat[i,j]
        Lincdat2l[tnum*(i-1)+j]=Lincdatb2[i,j]
      }}}}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if (hlmean[i]==2 | hlcv[i]==2) {
      incdat2l[tnum*(i-1)+j]=NA
      Lincdat2l[tnum*(i-1)+j]=NA
    }}}

mu=mean(incdat2l,na.rm=TRUE)
rho=cor(incdat2l,Lincdat2l,use="complete.obs")
sigma2u=var(incdat2l,na.rm=TRUE)*(1-rho^2)
sigmau=sqrt(sigma2u)
sigmaeps=sqrt(sigma2u/(1-rho^2))
a=(1-rho)*mu

mu
rho
sqrt(sigma2u)

incdat02=array(NA,c(hnum,tnum))
consdat02=array(NA,c(hnum,tnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if ((villagedat[i,j]==kk) && (hlmean[i]==1) && (hlcv[i]==1) && (incdat[i,j]>low1) && (incdat[i,j]<high1)) {
      incdat02[i,j]=incdatb2[i,j]
      consdat02[i,j]=consdat[i,j]
    }}}

#Markov chain for income of households
#points
jj=0
sh1=numeric(length=Sh1)
sh1[1:2]=c(1,1)
while ((sh1[1]-sh1[2])^2<0.0000000000001) {
  for (i in (jj+1):(Sh1+jj)) {
    sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
  }
  jj=jj+1
}
jj=jj-1

sh1=numeric()
for (i in (jj+1):(Sh1+jj)) {
  sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
}
#transition probabilities
P1=array(NA,c(Sh1,Sh1))
for (j in 1:Sh1) {
  P1[j,1]=pnorm(((sh1[1])-a-rho*(sh1[j])+((sh1[2])-(sh1[1]))/2)/sigmau)
  P1[j,Sh1]=1-pnorm(((sh1[Sh1])-a-rho*(sh1[j])-((sh1[Sh1])-(sh1[(Sh1-1)]))/2)/sigmau)
}
for (j in 1:Sh1) {
  for (k in 2:(Sh1-1)) {
    P1[j,k]=pnorm(((sh1[k])-a-rho*(sh1[j])+((sh1[(k+1)])-(sh1[k]))/2)/sigmau)-pnorm(((sh1[k])-a-rho*(sh1[j])-((sh1[k])-(sh1[(k-1)]))/2)/sigmau)
  }}
eig[1,1,]=eigen(t(P1))$vector[,1]/sum(eigen(t(P1))$vector[,1])
scalerhh=mean(incdat02,na.rm=TRUE)/sh1%*%eig[1,1,]
sh1=scalerhh*sh1

sh1r[kk,1,1,]=sh1
P1r[kk,1,1,,]=P1


## low mean high cv

incdat2l=numeric()
consdat2l=numeric()
Lincdat2l=numeric()
tcode2=numeric()
hcode2=numeric()
for (i in 1:hnum) {
  for (j in 1:tnum) {
    if (villagedat[i,j]==kk) {
      if ((incdat[i,j]>low1) & (incdat[i,j]<high1)) {
        incdat2l[tnum*(i-1)+j]=incdatb2[i,j]
        consdat2l[tnum*(i-1)+j]=consdat[i,j]
        Lincdat2l[tnum*(i-1)+j]=Lincdatb2[i,j]
      }}}}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if (hlmean[i]==2 | hlcv[i]==1) {
      incdat2l[tnum*(i-1)+j]=NA
      Lincdat2l[tnum*(i-1)+j]=NA
    }}}

mu=mean(incdat2l,na.rm=TRUE)
rho=max(0,cor(incdat2l,Lincdat2l,use="complete.obs"))
sigma2u=var(incdat2l,na.rm=TRUE)*(1-rho^2)
sigmau=sqrt(sigma2u)
sigmaeps=sqrt(sigma2u/(1-rho^2))
a=(1-rho)*mu

mu
rho
sqrt(sigma2u)

incdat02=array(NA,c(hnum,tnum))
consdat02=array(NA,c(hnum,tnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if ((villagedat[i,j]==kk) && (hlmean[i]==1) && (hlcv[i]==2) && (incdat[i,j]>low1) && (incdat[i,j]<high1)) {
      incdat02[i,j]=incdatb2[i,j]
      consdat02[i,j]=consdat[i,j]
    }}}

#Markov chain for income of households
#points
jj=0
sh1=numeric(length=Sh1)
sh1[1:2]=c(1,1)
while ((sh1[1]-sh1[2])^2<0.0000000000001) {
  for (i in (jj+1):(Sh1+jj)) {
    sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
  }
  jj=jj+1
}
jj=jj-1

sh1=numeric()
for (i in (jj+1):(Sh1+jj)) {
  sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
}
#transition probabilities
P1=array(NA,c(Sh1,Sh1))
for (j in 1:Sh1) {
  P1[j,1]=pnorm(((sh1[1])-a-rho*(sh1[j])+((sh1[2])-(sh1[1]))/2)/sigmau)
  P1[j,Sh1]=1-pnorm(((sh1[Sh1])-a-rho*(sh1[j])-((sh1[Sh1])-(sh1[(Sh1-1)]))/2)/sigmau)
}
for (j in 1:Sh1) {
  for (k in 2:(Sh1-1)) {
    P1[j,k]=pnorm(((sh1[k])-a-rho*(sh1[j])+((sh1[(k+1)])-(sh1[k]))/2)/sigmau)-pnorm(((sh1[k])-a-rho*(sh1[j])-((sh1[k])-(sh1[(k-1)]))/2)/sigmau)
  }}
eigm1=eigen(t(P1))$vector[,1]/sum(eigen(t(P1))$vector[,1])
for (mm1 in 1:8) {
  eig[1,2,]=Re(eigm1[mm1])
}
scalerhh=mean(incdat02,na.rm=TRUE)/sh1%*%eig[1,2,]
sh1=scalerhh*sh1

sh1r[kk,1,2,]=sh1
P1r[kk,1,2,,]=P1

## high mean low cv

incdat2l=numeric()
consdat2l=numeric()
Lincdat2l=numeric()
tcode2=numeric()
hcode2=numeric()
for (i in 1:hnum) {
  for (j in 1:tnum) {
    if (villagedat[i,j]==kk) {
      if ((incdat[i,j]>low1) & (incdat[i,j]<high1)) {
        incdat2l[tnum*(i-1)+j]=incdatb2[i,j]
        consdat2l[tnum*(i-1)+j]=consdat[i,j]
        Lincdat2l[tnum*(i-1)+j]=Lincdatb2[i,j]
      }}}}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if (hlmean[i]==1 | hlcv[i]==2) {
      incdat2l[tnum*(i-1)+j]=NA
      Lincdat2l[tnum*(i-1)+j]=NA
    }}}

mu=mean(incdat2l,na.rm=TRUE)
rho=cor(incdat2l,Lincdat2l,use="complete.obs")
sigma2u=var(incdat2l,na.rm=TRUE)*(1-rho^2)
sigmau=sqrt(sigma2u)
sigmaeps=sqrt(sigma2u/(1-rho^2))
a=(1-rho)*mu

mu
rho
sqrt(sigma2u)

incdat02=array(NA,c(hnum,tnum))
consdat02=array(NA,c(hnum,tnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if ((villagedat[i,j]==kk) && (hlmean[i]==2) && (hlcv[i]==1) && (incdat[i,j]>low1) && (incdat[i,j]<high1)) {
      incdat02[i,j]=incdatb2[i,j]
      consdat02[i,j]=consdat[i,j]
    }}}

#Markov chain for income of households
#points
jj=0
sh1=numeric(length=Sh1)
sh1[1:2]=c(1,1)
while ((sh1[1]-sh1[2])^2<0.0000000000001) {
  for (i in (jj+1):(Sh1+jj)) {
    sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
  }
  jj=jj+1
}
jj=jj-1

sh1=numeric()
for (i in (jj+1):(Sh1+jj)) {
  sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
}
#transition probabilities
P1=array(NA,c(Sh1,Sh1))
for (j in 1:Sh1) {
  P1[j,1]=pnorm(((sh1[1])-a-rho*(sh1[j])+((sh1[2])-(sh1[1]))/2)/sigmau)
  P1[j,Sh1]=1-pnorm(((sh1[Sh1])-a-rho*(sh1[j])-((sh1[Sh1])-(sh1[(Sh1-1)]))/2)/sigmau)
}
for (j in 1:Sh1) {
  for (k in 2:(Sh1-1)) {
    P1[j,k]=pnorm(((sh1[k])-a-rho*(sh1[j])+((sh1[(k+1)])-(sh1[k]))/2)/sigmau)-pnorm(((sh1[k])-a-rho*(sh1[j])-((sh1[k])-(sh1[(k-1)]))/2)/sigmau)
  }}
eig[2,1,]=eigen(t(P1))$vector[,1]/sum(eigen(t(P1))$vector[,1])
scalerhh=mean(incdat02,na.rm=TRUE)/sh1%*%eig[2,1,]
sh1=scalerhh*sh1

sh1r[kk,2,1,]=sh1
P1r[kk,2,1,,]=P1

## high mean high cv

incdat2l=numeric()
consdat2l=numeric()
Lincdat2l=numeric()
tcode2=numeric()
hcode2=numeric()
for (i in 1:hnum) {
  for (j in 1:tnum) {
    if (villagedat[i,j]==kk) {
      if ((incdat[i,j]>low1) & (incdat[i,j]<high1)) {
        incdat2l[tnum*(i-1)+j]=incdatb2[i,j]
        consdat2l[tnum*(i-1)+j]=consdat[i,j]
        Lincdat2l[tnum*(i-1)+j]=Lincdatb2[i,j]
      }}}}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if (hlmean[i]==1 | hlcv[i]==1) {
      incdat2l[tnum*(i-1)+j]=NA
      Lincdat2l[tnum*(i-1)+j]=NA
    }}}

mu=mean(incdat2l,na.rm=TRUE)
rho=cor(incdat2l,Lincdat2l,use="complete.obs")
sigma2u=var(incdat2l,na.rm=TRUE)*(1-rho^2)
sigmau=sqrt(sigma2u)
sigmaeps=sqrt(sigma2u/(1-rho^2))
a=(1-rho)*mu

mu
rho
sqrt(sigma2u)

incdat02=array(NA,c(hnum,tnum))
consdat02=array(NA,c(hnum,tnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (j in 1:tnum) {
    if ((villagedat[i,j]==kk) && (hlmean[i]==2) && (hlcv[i]==2) && (incdat[i,j]>low1) && (incdat[i,j]<high1)) {
      incdat02[i,j]=incdatb2[i,j]
      consdat02[i,j]=consdat[i,j]
    }}}

#Markov chain for income of households
#points
jj=0
sh1=numeric(length=Sh1)
sh1[1:2]=c(1,1)
while ((sh1[1]-sh1[2])^2<0.0000000000001) {
  for (i in (jj+1):(Sh1+jj)) {
    sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
  }
  jj=jj+1
}
jj=jj-1

sh1=numeric()
for (i in (jj+1):(Sh1+jj)) {
  sh1[(i-jj)]=quantile(incdat02,((2*i-1)/(2*(Sh1+jj))),na.rm=TRUE)
}
#transition probabilities
P1=array(NA,c(Sh1,Sh1))
for (j in 1:Sh1) {
  P1[j,1]=pnorm(((sh1[1])-a-rho*(sh1[j])+((sh1[2])-(sh1[1]))/2)/sigmau)
  P1[j,Sh1]=1-pnorm(((sh1[Sh1])-a-rho*(sh1[j])-((sh1[Sh1])-(sh1[(Sh1-1)]))/2)/sigmau)
}
for (j in 1:Sh1) {
  for (k in 2:(Sh1-1)) {
    P1[j,k]=pnorm(((sh1[k])-a-rho*(sh1[j])+((sh1[(k+1)])-(sh1[k]))/2)/sigmau)-pnorm(((sh1[k])-a-rho*(sh1[j])-((sh1[k])-(sh1[(k-1)]))/2)/sigmau)
  }}
eig[2,2,]=eigen(t(P1))$vector[,1]/sum(eigen(t(P1))$vector[,1])
scalerhh=mean(incdat02,na.rm=TRUE)/sh1%*%eig[2,2,]
sh1=scalerhh*sh1

sh1r[kk,2,2,]=sh1
P1r[kk,2,2,,]=P1

#simulation of village income
ys=array(NA,c(hnumv,1000))
for (k in 1:hnumv) {
  ran1=runif(1)
  eig2=numeric()
  eig2[1]=as.numeric(eig[hlmean[k+hnumv0],hlcv[k+hnumv0],1])
  for (i in 2:Sh1) {
    eig2[i]=eig2[i-1]+as.numeric(eig[hlmean[k+hnumv0],hlcv[k+hnumv0],i])
  }
  if (ran1<eig2[1]) {
    jj=1
  }
  for (i in 2:Sh1) {
    if (ran1<eig2[i] & ran1>=eig2[i-1]) {
      jj=i
    }}
  sta=numeric()
  sta[1]=jj
  for (j in 2:1000) {
    ran1=runif(1)
    eig2[1]=P1r[kk,hlmean[k+hnumv0],hlcv[k+hnumv0],sta[j-1],1]
    for (i in 2:Sh1) {
      eig2[i]=eig2[i-1]+P1r[kk,hlmean[k+hnumv0],hlcv[k+hnumv0],sta[j-1],i]
    }
    if (ran1<eig2[1]) {
      sta[j]=1
    }
    for (i in 2:Sh1) {
      if (ran1<eig2[i] & ran1>=eig2[i-1]) {
        sta[j]=i
      }}
    for (i in 1:1000) {
      ys[k,i]=sh1r[kk,hlmean[k+hnumv0],hlcv[k+hnumv0],sta[i]]
    }}}
meanys=numeric()
meanysl=numeric()
Lmeanys=numeric()
for (i in 1:900) {
  meanys[i]=log(mean(ys[,i+100]))
  meanysl[i]=mean(ys[,i+100])
  Lmeanys[i]=log(mean(ys[,i+99]))
}

mu=mean(meanys,na.rm=TRUE)
rho=cor(meanys,Lmeanys,use="complete.obs")
sigma2u=var(meanys,na.rm=TRUE)*(1-rho^2)
sigmau=sqrt(sigma2u)
sigmaeps=sqrt(sigma2u/(1-rho^2))
a=(1-rho)*mu

#Markov chain for income of villages
#points
sh2=numeric()
for (i in 1:Sh2) {
  sh2[i]=quantile(meanysl,((2*i-1)/(2*Sh2)),na.rm=TRUE)
}
#transition probabilities
P2=array(NA,c(Sh2,Sh2))
for (j in 1:Sh2) {
  P2[j,1]=pnorm((log(sh2[1])-a-rho*log(sh2[j])+(log(sh2[2])-log(sh2[1]))/2)/sigmau)
  P2[j,Sh2]=1-pnorm((log(sh2[Sh2])-a-rho*log(sh2[j])-(log(sh2[Sh2])-log(sh2[(Sh2-1)]))/2)/sigmau)
}
for (j in 1:Sh2) {
  for (k in 2:(Sh2-1)) {
    P2[j,k]=pnorm((log(sh2[k])-a-rho*log(sh2[j])+(log(sh2[(k+1)])-log(sh2[k]))/2)/sigmau)-pnorm((log(sh2[k])-a-rho*log(sh2[j])-(log(sh2[k])-log(sh2[(k-1)]))/2)/sigmau)
  }}
eig2=eigen(t(P2))$vector[,1]/sum(eigen(t(P2))$vector[,1])
scalervil=mean(vilmeaninctb[kk,])/sh2%*%eig2
sh2=scalervil*sh2

sh2r[kk,]=sh2
P2r[kk,,]=P2

#SOLVE THE LIMITED COMMITMENT MODEL WITH HOMOGENOUS PREFERENCES WITH THE NEW INCOME PROCESS GIVEN THE ESTIMATED PARAMETERS

yy=xLcnh1incHme[1:3]

delta3=yy[1]
sigma3=yy[2]
phi3=yy[3]

delta=yy[1]
sigma=yy[2]
pcphi=yy[3]

xint2=array(NA,c(2,2,S,2))
for (jj5 in 1:2) {
  for (jj6 in 1:2) {
    
    #expected utility of autarky for households
    U1a0=numeric(length=Sh1)
    i=1
    done=FALSE
    while (!done) {
      i=i+1
      U1a1=uu((sh1*(1-pcphi)),sigma)+delta*P1%*%U1a0
      if (max(abs(U1a1-U1a0))<crit/10) {
        done=TRUE
      }
      U1a0=U1a1
    }
    Uaut1=U1a1
    
    #expected utility of autarky for the village
    U2a0=numeric(length=Sh2)
    i=1
    done=FALSE
    while (!done) {
      i=i+1
      U2a1=uu((sh2*(1-pcphi)),sigma)+delta*P2%*%U2a0
      if (max(abs(U2a1-U2a0))<crit/10) {
        done=TRUE
      }
      U2a0=U2a1
    }
    Uaut2=U2a1
    
    Uaut=array(c(0),c(S,2))
    Uaut[,1]=Uaut1
    Uaut[,1]=sort(Uaut[,1])
    Uaut[,2]=Uaut2
    
    xinta=array(c(0),c(S,2))
    xint=array(c(0),c(S,2))
    
    #the gridpoints are:
    q=numeric(length=(g+1))
    q[1]=uup(max(sh2),sigma)/uup(min(sh1*(1-pcphi)),sigma)
    q[(g+1)]=uup(min(sh2*(1-pcphi)),sigma)/uup(max(sh1),sigma)
    for (j in 2:g) {
      q[j]=exp(log(q[j-1])+(log(q[g+1])-log(q[1]))/g)
    }
    qmin=q[1]
    qmax=q[(g+1)]
    
    x=array(NA,c(S,(g+1),h))
    x[,,1]=matrix(c(q),nrow=S,ncol=(g+1),byrow=T)
    
    cls=matrix(nrow=S,ncol=(g+1))
    for (k in 1:S) {
      for (l in 1:(g+1)) {
        cls[k,l]=s0[k]/(1+(hnumv-1)*q[l]^(-1/sigma))
      }
    }
    
    #perfect risk sharing
    
    V1a0=array(NA,c(S,(g+1)))
    V2a0=array(NA,c(S,(g+1)))
    V1a1=array(NA,c(S,(g+1)))
    V2a1=array(NA,c(S,(g+1)))
    for (i in 1:(g+1)) {
      V1a0[,i]=Uaut[,1]
      V2a0[,i]=Uaut[,2]
    }
    j=1
    done=FALSE
    while (!done) {
      for (i in 1:(g+1)) {
        V10=uu(cls[,i],sigma)
        V20=uu(((s0-cls[,i])/(hnumv-1)),sigma)
        V1a1[,i]=V10+delta*R%*%V1a0[,i]
        V2a1[,i]=V20+delta*R%*%V2a0[,i]
      }
      if (max(max(abs(V1a1-V1a0)),max(abs(V2a1-V2a0)))<crit/100) {
        done=TRUE
      }
      V1a0=V1a1
      V2a0=V2a1
      j=j+1
    }
    
    V1=V1a1
    V2=V2a1
    
    xx=array(NA,c(S,(g+1)))
    xxint=array(1,c(S,2,h))
    
    #we use the above allocations and values as initial guesses
    
    #update policies and value functions disregarding the enforceability
    #constraints, then check if they are satisfied, change the policy
    #updates if they are not
    
    V1l=array(NA,c(S,(g+1),h))
    V1l[,,1]=V1
    V2l=array(NA,c(S,(g+1),h))
    V2l[,,1]=V2
    cl=array(NA,c(S,(g+1),h))
    cl[,,1]=cls
    dist=numeric()
    i=2
    done=FALSE
    while (!done) {
      count=0
      x[,,i]=x[,,1]
      cl[,,i]=cls
      for (j in 1:(g+1)) {
        V10=uu(cl[,j,i],sigma)
        V20=uu(((s0-cl[,j,i])/(hnumv-1)),sigma)
        V1l[,j,i]=V10+delta*R%*%V1l[,j,i-1]
        V2l[,j,i]=V20+delta*R%*%V2l[,j,i-1]
      }
      #now check enforceability
      for (k in 1:S) {
        ff1=function(qq) {
          cla=s0[k]/(1+(hnumv-1)*qq^(-1/sigma))
          jj=which.min((qq-q)^2)
          if (qq>q[jj]) {
            jj=jj+1
          }
          if (jj>1) {
            intp=(qq-q[jj-1])/(q[jj]-q[jj-1])
            Vs1=intp*(V1l[,jj,i-1]-V1l[,jj-1,i-1])+V1l[,jj-1,i-1]
          } else {
            Vs1=V1l[,1,i-1]
          }
          dif1=uu(cla,sigma)+delta*R[k,]%*%Vs1-Uaut[k,1]
          return(dif1)
        }
        if ((ff1(qmax)>0) && (ff1(qmin)<0)) {
          v=uniroot(ff1,c(qmin,qmax),tol=crit,maxiter=100)$root
          j=which.min(abs(v-q))
          if (q[j]>v) {
            j=j-1
          }
          x[k,1:j,i]=v
          V2s=numeric()
          if (j<(g+1)) {
            intp=(x[k,j,i]-q[j])/(q[j+1]-q[j])
            V2s=intp*(V2l[,j+1,i-1]-V2l[,j,i-1])+V2l[,j,i-1]
          } else {
            V2s=V2l[,g+1,i-1]
          }
          cl[k,1:j,i]=s0[k]/(1+(hnumv-1)*x[k,j,i]^(-1/sigma))
          V1l[k,1:j,i]=Uaut[k,1]
          V2l[k,1:j,i]=uu(((s0[k]-cl[k,j,i])/(hnumv-1)),sigma)+delta*R[k,]%*%V2s
        } else {
          if (ff1(qmax)<0) {
            count=count+1
            V1l[k,,i]=Uaut[k,1]
            V2l[k,,i]=Uaut[k,2]
            x[k,,i]=qmax
            j=g+2
          } else {
            j=0	
          }}
        
        if (j<(g+2)) {
          ff1=function(qq) {
            cla=s0[k]/(1+(hnumv-1)*qq^(-1/sigma))
            jj=which.min((qq-q)^2)
            if (qq>q[jj]) {
              jj=jj+1
            }
            if (jj>1) {
              intp=(qq-q[jj-1])/(q[jj]-q[jj-1])
              Vs2=intp*(V2l[,jj,i-1]-V2l[,jj-1,i-1])+V2l[,jj-1,i-1]
            } else {
              Vs2=V2l[,1,i-1]
            }
            dif1=uu(((s0[k]-cla)/(hnumv-1)),sigma)+delta*R[k,]%*%Vs2-Uaut[k,2]
            return(dif1)
          }
          if ((ff1(qmin)>0) && (ff1(qmax)<0)) {
            v=uniroot(ff1,c(qmin,qmax),tol=crit,maxiter=100)$root
            l=which.min(abs(v-q))
            if (q[l]<v) {
              l=l+1
            }
            x[k,l:(g+1),i]=v
            V1s=numeric()
            if (l>1) {
              intp=(x[k,l,i]-q[l-1])/(q[l]-q[l-1])
              V1s=intp*(V1l[,l,i-1]-V1l[,l-1,i-1])+V1l[,l-1,i-1]
            } else {
              V1s=V1l[,1,i-1]
            }
            cl[k,l:(g+1),i]=s0[k]/(1+(hnumv-1)*x[k,l,i]^(-1/sigma))
            V1l[k,l:(g+1),i]=uu(cl[k,l,i],sigma)+delta*R[k,]%*%V1s
            V2l[k,l:(g+1),i]=Uaut[k,2]
          } else {
            if (ff1(qmin)<0) {
              count=count+1
              V1l[k,,i]=Uaut[k,1]
              V2l[k,,i]=Uaut[k,2]
              x[k,,i]=qmin
              l=0
            } else {
              l=g+2
            }}}
        
        if ((j+1)<(l-1)) {
          for (jj in (j+1):(l-1)) {
            V1l[k,jj,i]=uu(cl[k,jj,i],sigma)+delta*R[k,]%*%V1l[,jj,i-1]
            V2l[k,jj,i]=uu(((s0[k]-cl[k,jj,i])/(hnumv-1)),sigma)+delta*R[k,]%*%V2l[,jj,i-1]
          }
        }
      }
      
      xx=x[,,i]
      maxx=numeric()
      minx=numeric()
      for (k in 1:S) {
        maxx[k]=max(xx[k,])
        minx[k]=min(xx[k,])
      }
      xxint[,1,i]=minx
      xxint[,2,i]=maxx
      xxint[,,i]
      
      dist[i]=max(c(abs(V1l[,,i]-V1l[,,i-1]),abs(V2l[,,i]-V2l[,,i-1]),abs(xxint[,,i]-xxint[,,i-1])))
      if (dist[i]<crit) {
        done=TRUE
      }
      if (i>=h) {
        done=TRUE
      }
      i=i+1		
    }
    xxint[,,i-1]
    
    maxx=numeric()
    minx=numeric()
    for (j in 1:S) {
      maxx[j]=max(xx[j,])
      minx[j]=min(xx[j,])
    }
    
    xint2[jj5,jj6,,1]=minx
    xint2[jj5,jj6,,2]=maxx
  }}

#COMPUTE THE PREDICTED CONSUMPTION ALLOCATION AT THE TIME OF INTRODUCTION OF THE POLICY

xt=array(NA,c(hnum,(tnum-1)))
xy=array(NA,c(hnum,(tnum-1)))
clc=array(NA,c(hnum,(tnum-1)))
clv=array(NA,c(hnum,(tnum-1)))
irl=array(NA,c(hnum,(tnum-1)))

for (t in 1:(tnum-1)) {
  irlz2=which.min((sh2-mean(incdatb2[(hnumv0+1):(hnumv0+hnumv),(t+1)]))^2)
  for (m in (hnumv0+1):(hnumv0+hnumv)) {
    xy[m,t]=uup(mean(consdat[(hnumv0+1):(hnumv0+hnumv),t]),sigma3)/uup(consdat[m,t],sigma3)
    irlz1=which.min((sh1-incdatb2[m,(t+1)])^2)
    irl[m,t]=Sh2*(irlz1-1)+irlz2
  }}

count3=numeric()
for (t in 1:(tnum-1)) {
  s02a=numeric()
  for (m in (hnumv0+1):(hnumv0+hnumv)) {
    s02a[m]=s0[irl[m,t]]
  }
  count=1
  count2=0
  itercount=1
  xtiter=rep(1,hnum)
  while (count>0) {
    count=0
    xtiter2=xtiter
    for (m in (hnumv0+1):(hnumv0+hnumv)) {
      if (xtiter[m]==1) {
        if (xy[m,t]>=xint2[hlmean[m],hlcv[m],irl[m,t],1]) {
          xt[m,t]=xy[m,t]
          xtiter[m]=1
        } else {
          count=count+1
          count2=count2+1
          xt[m,t]=xint2[hlmean[m],hlcv[m],irl[m,t],1]
          xtiter[m]=0
        }
        clc[m,t]=s0[irl[m,t]]/(1+(hnumv-1)*xt[m,t]^(-1/sigma3))
        clv[m,t]=(s0[irl[m,t]]-clc[m,t])/(hnumv-1)
      }}
    s02a=s02a+(xtiter[(hnumv0+1):(hnumv0+hnumv)]-1)%*%clc[(hnumv0+1):(hnumv0+hnumv),t]
    for (m in (hnumv0+1):(hnumv0+hnumv)) {
      if (xtiter[m]==1) {
        clcaa=s02a[m]/(1+(sum(xtiter[(hnumv0+1):(hnumv0+hnumv)])-1)*xt[m,t]^(-1/sigma3))
        clc[m,t]=max(clcaa,-1/clcaa)
        clv[m,t]=(s0[irl[m,t]]-clc[m,t])/(hnumv-1)
        xy[m,t]=uup(clv[m,t],sigma3)/uup(clc[m,t],sigma3)
      }}
    itercount=itercount+1
  }
  for (m in (hnumv0+1):(hnumv0+hnumv)) {
    clv[m,t]=s0[irl[m,t]]/hnumv
  }
  count3[t]=count2
}

trpredconslcnh=clc

meanpredc=numeric()
meanc2=numeric()
for (t in 1:(tnum-1)) {
  meanpredc[t]=mean(trpredconslcnh[(hnumv0+1):(hnumv0+hnumv),t])
  meanc2[t]=mean(incdatb2[(hnumv0+1):(hnumv0+hnumv),(t+1)])
}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (t in 1:(tnum-1)) {
    trpredconslcnh[i,t]=trpredconslcnh[i,t]*meanc2[t]/meanpredc[t]
  }}

#AVERAGE CHANGES IN CONSUMPTION

pcincrlcnh=numeric()
rcincrlcnh=numeric()
for (t in 1:5) {
  pcincrlcnh[t]=mean((trpredconslcnh[(hnumv0+1):(hnumv0+hnumv),t]-predconslcnh[(hnumv0+1):(hnumv0+hnumv),t])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
  rcincrlcnh[t]=mean((trpredconslcnh[(hnumv0+1):(hnumv0+hnumv),t]-predconslcnh[(hnumv0+1):(hnumv0+hnumv),t])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
}
pcincrlcnh
rcincrlcnh
mean(pcincrlcnh)
mean(rcincrlcnh)

#SOLVE THE LIMITED COMMITMENT MODEL WITH HETEROGENOUS PREFERENCES WITH THE NEW INCOME PROCESS GIVEN THE ESTIMATED PARAMETERS

yy=xLc1incHme_nog[1:7]

delta3=yy[1]
sigmav3=yy[2]
a1=yy[2]*yy[3]
a2=yy[2]*yy[4]
a3=yy[2]*yy[5]
a4=yy[2]*yy[6]
phi3=yy[7]

a33=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
  a33[m]=xLc1incHme_nog[5]
  if ((m==76) | (m==92) | (m==97)) {
    a33[m]=xLc1incHme_nog[5]/5
  }}

xint=array(NA,c(hnum,S,2))
maxsig=min(sigmav3+3,max(sig2))
minsig=max(0.1,sigmav3-3)

sigi=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
  sigi[m]=min(max(sigmav3+a1*educ[m]+a2*propf[m]+a33[m]*age[m]+a4*land1[m],minsig),maxsig) #sigi
}

sigi2=sigi
sigcount1=0
sigcount2=0
for (m in (hnumv0+1):(hnumv0+hnumv)) {
  if (sigi[m]==minsig) {
    sigi2[m]=NA
    sigcount1=sigcount1+1
  }
  if (sigi[m]==maxsig) {
    sigi2[m]=NA
    sigcount2=sigcount2+1
  }}
sigcount=sigcount1+sigcount2

meansig1=mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)*(hnumv-sigcount)/hnumv+minsig*sigcount1/hnumv+maxsig*sigcount2/hnumv
meansig2=mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-(mean(sigi[(hnumv0+1):(hnumv0+hnumv)])-sigmav3)*hnumv/(hnumv-sigcount)

sigma3=sigi
for (m in (hnumv0+1):(hnumv0+hnumv)) {
  if ((sigi[m]>minsig) & (sigi[m]<maxsig)) {
    sigma3[m]=sigi[m]*meansig2/mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
  }}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
  sigma3[m]=max(sigma3[m],minsig)
  sigma3[m]=min(sigma3[m],maxsig)
}

for (m in (hnumv0+1):(hnumv0+hnumv)) {
  
  sh1=sh1r[vilcode,hlmean[m],hlcv[m],]
  P1=P1r[vilcode,hlmean[m],hlcv[m],,]
  sh2=sh2r[vilcode,]
  P2=P2r[vilcode,,]
  
  R=matrix()
  R=kronecker(P1,P2)
  
  s01=matrix(nrow=S,ncol=1)
  s01[,1]=sh1
  s01[,1]=sort(s01[,1])
  s02=matrix(nrow=S,ncol=1)
  s02[,1]=sh2
  
  #aggregate income in the S states
  s=matrix(nrow=S,ncol=2)
  s[,1]=s01
  s[,2]=s02
  s0=matrix(nrow=S,ncol=1)
  s0=s01[,1]+(hnumv-1)*s02[,1]
  
  delta=delta3
  sigma=sigma3[m]
  sigmav=sigmav3
  pcphi=phi3
  
  #expected utility of autarky for households
  U1a0=numeric(length=Sh1)
  i=1
  done=FALSE
  while (!done) {
    i=i+1
    U1a1=uu((sh1*(1-pcphi)),sigma)+delta*P1%*%U1a0
    if (max(abs(U1a1-U1a0))<crit/10) {
      done=TRUE
    }
    U1a0=U1a1
  }
  Uaut1=U1a1
  
  #expected utility of autarky for the village
  U2a0=numeric(length=Sh2)
  i=1
  done=FALSE
  while (!done) {
    i=i+1
    U2a1=uu((sh2*(1-pcphi)),sigmav)+delta*P2%*%U2a0
    if (max(abs(U2a1-U2a0))<crit/10) {
      done=TRUE
    }
    U2a0=U2a1
  }
  Uaut2=U2a1
  
  Uaut=array(c(0),c(S,2))
  Uaut[,1]=Uaut1
  Uaut[,1]=sort(Uaut[,1])
  Uaut[,2]=Uaut2
  
  xinta=array(c(0),c(S,2))
  
  #the gridpoints are:
  q=numeric(length=(g+1))
  q[1]=uup(max(sh2),sigmav)/uup(min(sh1*(1-pcphi)),sigma)
  q[(g+1)]=uup(min(sh2*(1-pcphi)),sigmav)/uup(max(sh1),sigma)
  for (j in 2:g) {
    q[j]=exp(log(q[j-1])+(log(q[g+1])-log(q[1]))/g)
  }
  qmin=q[1]
  qmax=q[(g+1)]
  
  x=array(NA,c(S,(g+1),h))
  x[,,1]=matrix(c(q),nrow=S,ncol=(g+1),byrow=T)
  
  cls=matrix(nrow=S,ncol=(g+1))
  for (k in 1:S) {
    for (l in 1:(g+1)) {
      f=function(w) uup(((s0[k]-w)/(hnumv-1)),sigmav)/uup(w,sigma)-q[l]
      v=uniroot(f,c(0.000001,(s0[k]-0.000001)),tol=crit,maxiter=100)
      cls[k,l]=v$root
    }
  }
  
  #perfect risk sharing
  
  V1a0=array(NA,c(S,(g+1)))
  V2a0=array(NA,c(S,(g+1)))
  V1a1=array(NA,c(S,(g+1)))
  V2a1=array(NA,c(S,(g+1)))
  for (i in 1:(g+1)) {
    V1a0[,i]=Uaut[,1]
    V2a0[,i]=Uaut[,2]
  }
  j=1
  done=FALSE
  while (!done) {
    for (i in 1:(g+1)) {
      V10=uu(cls[,i],sigma)
      V20=uu(((s0-cls[,i])/(hnumv-1)),sigmav)
      V1a1[,i]=V10+delta*R%*%V1a0[,i]
      V2a1[,i]=V20+delta*R%*%V2a0[,i]
    }
    if (max(max(abs(V1a1-V1a0)),max(abs(V2a1-V2a0)))<crit/10) {
      done=TRUE
    }
    V1a0=V1a1
    V2a0=V2a1
    j=j+1
  }
  
  V1=V1a1
  V2=V2a1
  
  xx=array(NA,c(S,(g+1)))
  xxint=array(1,c(S,2,h))
  
  #we use the above allocations and values as initial guesses
  
  #update policies and value functions disregarding the enforceability
  #constraints, then check if they are satisfied, change the policy
  #updates if they are not
  
  V1l=array(NA,c(S,(g+1),h))
  V1l[,,1]=V1
  V2l=array(NA,c(S,(g+1),h))
  V2l[,,1]=V2
  cl=array(NA,c(S,(g+1),h))
  cl[,,1]=cls
  dist=numeric()
  i=2
  done=FALSE
  while (!done) {
    count=0
    x[,,i]=x[,,1]
    cl[,,i]=cls
    for (j in 1:(g+1)) {
      V10=uu(cl[,j,i],sigma)
      V20=uu(((s0-cl[,j,i])/(hnumv-1)),sigmav)
      V1l[,j,i]=V10+delta*R%*%V1l[,j,i-1]
      V2l[,j,i]=V20+delta*R%*%V2l[,j,i-1]
    }
    #now check enforceability
    for (k in 1:S) {
      ff1=function(qq) {
        f=function(w) uup(((s0[k]-w)/(hnumv-1)),sigmav)/uup(w,sigma)-qq
        cla=uniroot(f,c(0.000001,(s0[k]-0.000001)),tol=crit,maxiter=100)$root
        jj=which.min((qq-q)^2)
        if (qq>q[jj]) {
          jj=jj+1
        }
        if (jj>1) {
          intp=(qq-q[jj-1])/(q[jj]-q[jj-1])
          Vs1=intp*(V1l[,jj,i-1]-V1l[,jj-1,i-1])+V1l[,jj-1,i-1]
        } else {
          Vs1=V1l[,1,i-1]
        }
        dif1=uu(cla,sigma)+delta*R[k,]%*%Vs1-Uaut[k,1]
        return(dif1)
      }
      if ((ff1(qmax)>0) && (ff1(qmin)<0)) {
        v=uniroot(ff1,c(qmin,qmax),tol=crit,maxiter=100)$root
        j=which.min(abs(v-q))
        if (q[j]>v) {
          j=j-1
        }
        x[k,1:j,i]=v
        V2s=numeric()
        if (j<(g+1)) {
          intp=(x[k,j,i]-q[j])/(q[j+1]-q[j])
          V2s=intp*(V2l[,j+1,i-1]-V2l[,j,i-1])+V2l[,j,i-1]
        } else {
          V2s=V2l[,g+1,i-1]
        }
        f=function(w) uup(((s0[k]-w)/(hnumv-1)),sigmav)/uup(w,sigma)-x[k,j,i]
        cl[k,1:j,i]=uniroot(f,c(0.000001,(s0[k]-0.000001)),tol=crit,maxiter=100)$root
        V1l[k,1:j,i]=Uaut[k,1]
        V2l[k,1:j,i]=uu(((s0[k]-cl[k,j,i])/(hnumv-1)),sigmav)+delta*R[k,]%*%V2s
      } else {
        if (ff1(qmax)<0) {
          count=count+1
          V1l[k,,i]=Uaut[k,1]
          V2l[k,,i]=Uaut[k,2]
          x[k,,i]=qmax
          j=g+2
        } else {
          j=0	
        }}
      
      if (j<(g+2)) {
        ff1=function(qq) {
          f=function(w) uup(((s0[k]-w)/(hnumv-1)),sigmav)/uup(w,sigma)-qq
          cla=uniroot(f,c(0.000001,(s0[k]-0.000001)),tol=crit,maxiter=100)$root
          jj=which.min((qq-q)^2)
          if (qq>q[jj]) {
            jj=jj+1
          }
          if (jj>1) {
            intp=(qq-q[jj-1])/(q[jj]-q[jj-1])
            Vs2=intp*(V2l[,jj,i-1]-V2l[,jj-1,i-1])+V2l[,jj-1,i-1]
          } else {
            Vs2=V2l[,1,i-1]
          }
          dif1=uu(((s0[k]-cla)/(hnumv-1)),sigmav)+delta*R[k,]%*%Vs2-Uaut[k,2]
          return(dif1)
        }
        if ((ff1(qmin)>0) && (ff1(qmax)<0)) {
          v=uniroot(ff1,c(qmin,qmax),tol=crit,maxiter=100)$root
          l=which.min(abs(v-q))
          if (q[l]<v) {
            l=l+1
          }
          x[k,l:(g+1),i]=v
          V1s=numeric()
          if (l>1) {
            intp=(x[k,l,i]-q[l-1])/(q[l]-q[l-1])
            V1s=intp*(V1l[,l,i-1]-V1l[,l-1,i-1])+V1l[,l-1,i-1]
          } else {
            V1s=V1l[,1,i-1]
          }
          f=function(w) uup(((s0[k]-w)/(hnumv-1)),sigmav)/uup(w,sigma)-x[k,l,i]
          cl[k,l:(g+1),i]=uniroot(f,c(0.000001,(s0[k]-0.000001)),tol=crit,maxiter=100)$root
          V1l[k,l:(g+1),i]=uu(cl[k,l,i],sigma)+delta*R[k,]%*%V1s
          V2l[k,l:(g+1),i]=Uaut[k,2]
        } else {
          if (ff1(qmin)<0) {
            count=count+1
            V1l[k,,i]=Uaut[k,1]
            V2l[k,,i]=Uaut[k,2]
            x[k,,i]=qmin
            l=0
          } else {
            l=g+2
          }}}
      
      if ((j+1)<(l-1)) {
        for (jj in (j+1):(l-1)) {
          V1l[k,jj,i]=uu(cl[k,jj,i],sigma)+delta*R[k,]%*%V1l[,jj,i-1]
          V2l[k,jj,i]=uu(((s0[k]-cl[k,jj,i])/(hnumv-1)),sigmav)+delta*R[k,]%*%V2l[,jj,i-1]
        }
      }
    }
    
    xx=x[,,i]  
    dist[i]=max(c(abs(V1l[,,i]-V1l[,,i-1]),abs(V2l[,,i]-V2l[,,i-1])))
    if (dist[i]<crit) {
      done=TRUE
    }
    if (i>=h) {
      done=TRUE
    }
    i=i+1		
  }
  
  maxx=numeric()
  minx=numeric()
  for (k in 1:S) {
    maxx[k]=max(xx[k,])
    minx[k]=min(xx[k,])
  }
  
  xint[m,,1]=minx
  xint[m,,2]=maxx
  
}

#COMPUTE THE PREDICTED CONSUMPTION ALLOCATION AT THE TIME OF INTRODUCTION OF THE POLICY

xt=array(NA,c(hnum,(tnum-1)))
xy=array(NA,c(hnum,(tnum-1)))
clc=array(NA,c(hnum,(tnum-1)))
clv=array(NA,c(hnum,(tnum-1)))
irl=array(NA,c(hnum,(tnum-1)))

for (t in 1:(tnum-1)) {
  irlz2=which.min((sh2-mean(incdatb2[(hnumv0+1):(hnumv0+hnumv),(t+1)]))^2)
  for (m in (hnumv0+1):(hnumv0+hnumv)) {
    xy[m,t]=uup(mean(consdat[(hnumv0+1):(hnumv0+hnumv),t]),sigmav3)/uup(consdat[m,t],sigma3[m])
    irlz1=which.min((sh1-incdatb2[m,(t+1)])^2)
    irl[m,t]=Sh2*(irlz1-1)+irlz2
  }}

count3=numeric()
for (t in 1:(tnum-1)) {
  s02a=numeric()
  for (m in (hnumv0+1):(hnumv0+hnumv)) {
    s02a[m]=s0[irl[m,t]]
  }
  count=1
  count2=0
  itercount=1
  xtiter=rep(1,hnum)
  while (count>0) {
    count=0
    xtiter2=xtiter
    for (m in (hnumv0+1):(hnumv0+hnumv)) {
      if (xtiter[m]==1) {
        if (xy[m,t]>=xint[m,irl[m,t],1]) {
          xt[m,t]=xy[m,t]
          xtiter[m]=1
        } else {
          count=count+1
          count2=count2+1
          xt[m,t]=xint[m,irl[m,t],1]
          xtiter[m]=0
        }
        xtiter[(hnumv0+1):(hnumv0+hnumv)]
        poor[(hnumv0+1):(hnumv0+hnumv)]
        f=function(w) uup(((s0[irl[m,t]]-w)/(hnumv-1)),sigmav3)/uup(w,sigma3[m])-xt[m,t]
        clc[m,t]=uniroot(f,c(0.000001,(s0[irl[m,t]]-0.000001)),tol=crit,maxiter=100)$root
        clv[m,t]=(s0[irl[m,t]]-clc[m,t])/(hnumv-1)
      }}
    #subtract the consumption of constrained households
    s02a=s02a+(xtiter[(hnumv0+1):(hnumv0+hnumv)]-1)%*%clc[(hnumv0+1):(hnumv0+hnumv),t]
    for (m in (hnumv0+1):(hnumv0+hnumv)) {
      if (xtiter[m]==1) {
        f=function(w) uup(((s02a[m]-w)/(sum(xtiter[(hnumv0+1):(hnumv0+hnumv)])-1)),sigmav3)/uup(w,sigma3[m])-xt[m,t]
        if ((s02a[m]>0) && (f(0.000001)*f(s02a[m]-0.000001)<0)) {
          clcaa=uniroot(f,c(0.000001,(s02a[m]-0.000001)),tol=crit,maxiter=100)$root
          clc[m,t]=uniroot(f,c(0.000001,(s02a[m]-0.000001)),tol=crit,maxiter=100)$root
        } else {
          clc[m,t]=max(-1/s02a[m],s02a[m])
        }
        clv[m,t]=(s0[irl[m,t]]-clc[m,t])/(hnumv-1)
        xy[m,t]=uup(clv[m,t],sigmav3)/uup(clc[m,t],sigma3[m])
      }}
    itercount=itercount+1
  }
  for (m in (hnumv0+1):(hnumv0+hnumv)) {
    clv[m,t]=s0[irl[m,t]]/hnumv
  }
  count3[t]=count2
}

trpredconslc=clc
trpxintlc=xint
trpirllc=irl

meanpredc=numeric()
meanc2=numeric()
meanc1=numeric()
for (t in 1:(tnum-1)) {
  meanpredc[t]=mean(trpredconslc[(hnumv0+1):(hnumv0+hnumv),t])
  meanc2[t]=mean(incdatb2[(hnumv0+1):(hnumv0+hnumv),(t+1)])
  meanc1[t]=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  for (t in 1:(tnum-1)) {
    trpredconslc[i,t]=trpredconslc[i,t]*meanc2[t]/meanpredc[t]
  }}

#AVERAGE CHANGES IN CONSUMPTION

pcincrlc=numeric()
rcincrlc=numeric()
for (t in 1:5) {
  pcincrlc[t]=mean((trpredconslc[(hnumv0+1):(hnumv0+hnumv),t]-predconslc[(hnumv0+1):(hnumv0+hnumv),t])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
  rcincrlc[t]=mean((trpredconslc[(hnumv0+1):(hnumv0+hnumv),t]-predconslc[(hnumv0+1):(hnumv0+hnumv),t])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
}
pcincrlc
rcincrlc
mean(pcincrlc)
mean(rcincrlc)

#all
#lcnh
mean(pcincrlcnh)
mean(rcincrlcnh)
#lc
mean(pcincrlc)
mean(rcincrlc)

#PERCENTAGE CHANGES
mean(pcincrlcnh)/mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv),t]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(rcincrlcnh)/mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv),t]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(pcincrlc)/mean(predconslc[(hnumv0+1):(hnumv0+hnumv),t]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
mean(rcincrlc)/mean(predconslc[(hnumv0+1):(hnumv0+hnumv),t]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)

#COMPUTE CERTAINTY-EQUIVALENT CONSUMPTION CHANGES

uu3=function(c,sig) (c^(1-sig)-1)/(1-sig)
uuinv2=function(u,sig) ((1-sig)*u+1)^(1/(1-sig))
upredlcnh=numeric()
upredlc=numeric()
utrpredlcnh=numeric()
utrpredlc=numeric()
cepredlcnh=numeric()
cetrpredlcnh=numeric()
cepredlc=numeric()
cetrpredlc=numeric()
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	upredlcnh[i]=(uu3(predconslcnh[i,1],xLcnh1incHme[2])+uu3(predconslcnh[i,2],xLcnh1incHme[2])+uu3(predconslcnh[i,3],xLcnh1incHme[2])+uu3(predconslcnh[i,4],xLcnh1incHme[2])+uu3(predconslcnh[i,5],xLcnh1incHme[2]))/5
	cepredlcnh[i]=uuinv2(upredlcnh[i],xLcnh1incHme[2])
	utrpredlcnh[i]=(uu3(trpredconslcnh[i,1],xLcnh1incHme[2])+uu3(trpredconslcnh[i,2],xLcnh1incHme[2])+uu3(trpredconslcnh[i,3],xLcnh1incHme[2])+uu3(trpredconslcnh[i,4],xLcnh1incHme[2])+uu3(trpredconslcnh[i,5],xLcnh1incHme[2]))/5
	cetrpredlcnh[i]=uuinv2(utrpredlcnh[i],xLcnh1incHme[2])
	upredlc[i]=(uu3(predconslc[i,1],sigLc[i])+uu3(predconslc[i,2],sigLc[i])+uu3(predconslc[i,3],sigLc[i])+uu3(predconslc[i,4],sigLc[i])+uu3(predconslc[i,5],sigLc[i]))/5
	cepredlc[i]=uuinv2(upredlc[i],sigLc[i])
	utrpredlc[i]=(uu3(trpredconslc[i,1],sigLc[i])+uu3(trpredconslc[i,2],sigLc[i])+uu3(trpredconslc[i,3],sigLc[i])+uu3(trpredconslc[i,4],sigLc[i])+uu3(trpredconslc[i,5],sigLc[i]))/5
	cetrpredlc[i]=uuinv2(utrpredlc[i],sigLc[i])
	}

pceincrlcnh=numeric()
rceincrlcnh=numeric()
pceincrlc=numeric()
rceincrlc=numeric()
  pceincrlcnh=mean((cetrpredlcnh[(hnumv0+1):(hnumv0+hnumv)]-cepredlcnh[(hnumv0+1):(hnumv0+hnumv)])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
  rceincrlcnh=mean((cetrpredlcnh[(hnumv0+1):(hnumv0+hnumv)]-cepredlcnh[(hnumv0+1):(hnumv0+hnumv)])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
  pceincrlc=mean((cetrpredlc[(hnumv0+1):(hnumv0+hnumv)]-cepredlc[(hnumv0+1):(hnumv0+hnumv)])*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
  rceincrlc=mean((cetrpredlc[(hnumv0+1):(hnumv0+hnumv)]-cepredlc[(hnumv0+1):(hnumv0+hnumv)])*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
pceincrlcnh
rceincrlcnh
pceincrlc
rceincrlc
pceincrlcnh/mean(cepredlcnh[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
rceincrlcnh/mean(cepredlcnh[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
pceincrlc/mean(cepredlc[(hnumv0+1):(hnumv0+hnumv)]*poor[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
rceincrlc/mean(cepredlc[(hnumv0+1):(hnumv0+hnumv)]*rich[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)


save.image("Transfer1red")




