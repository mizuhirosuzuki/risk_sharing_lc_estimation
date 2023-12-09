# setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
# rm(list = ls(all = TRUE))
load("Laczo2015/allest")

vilcode=2
set.seed(81)

#define utility function and normalize values for numerical convenience
uu2=function(cc,sigi) {
if (sigi!=1) {
		ut=(cc^(1-sigi)-1)/(1-sigi)
		} else {
		ut=log(cc)
		}
	return(ut)
	}	
uup2=function(cc,sigi) cc^(-sigi)
uu=function(cc,sigi) uu2(cc,sigi)/uu2(400,sigi)
uup=function(cc,sigi) uup2(cc,sigi)/uup2(400,sigi)

time=c(1:tnum)
hid=c(1:hnum)
nv=max(villagedat)

#number of observations
nn=hnum*(tnum-1)


#CONSTRUCT VARIABLES

#compute village mean of income and consumption
vilmeaninct=array(NA,c(nv,tnum))
vilsuminct=array(NA,c(nv,tnum))
vilmeaninc=numeric()
vilmeanconst=array(NA,c(nv,tnum))
vilsumconst=array(NA,c(nv,tnum))
vilmeancons=numeric()
for (k in 1:nv) {
for (l in 1:tnum) {
	incda=array(NA,c(hnum,ta))
	consda=array(NA,c(hnum,ta))
	for (i in 1:hnum) {
	for (j in 1:tnum) {
		if (villagedat[i,j]==k & tcode[i,j]==l) {
			incda[i,j]=incdat[i,j]
			consda[i,j]=consdat[i,j]
	}}}
	vilmeaninct[k,l]=mean(incda,na.rm=TRUE)
	vilsuminct[k,l]=sum(incda,na.rm=TRUE)
	vilmeanconst[k,l]=mean(consda,na.rm=TRUE)
	vilsumconst[k,l]=sum(consda,na.rm=TRUE)
}
vilmeaninc[k]=mean(vilmeaninct[k,])
vilmeancons[k]=mean(vilmeanconst[k,])
}

#reshape
vilmeaninctij=array(NA,c(hnum,tnum))
vilsuminctij=array(NA,c(hnum,tnum))
vilmeanconstij=array(NA,c(hnum,tnum))
vilsumconstij=array(NA,c(hnum,tnum))
for (i in 1:hnum) {
for (j in 1:tnum) {
	for (k in 1:nv) {
	if (villagedat[i,j]==k) {
	vilmeaninctij[i,j]=vilmeaninct[k,j]
	vilsuminctij[i,j]=vilsuminct[k,j]
	vilmeanconstij[i,j]=vilmeanconst[k,j]
	vilsumconstij[i,j]=vilsumconst[k,j]
}}}}


#compute differences from the village mean
vilmeaneduc=numeric()
vilmeanpropf=numeric()
vilmeanage=numeric()
vilmeanland1=numeric()
for (i in 1:hnum) {
	vilmeaneduca=numeric()
	vilmeanpropfa=numeric()
	vilmeanagea=numeric()
	vilmeanland1a=numeric()
	for (j in 1:hnum) {
	if (villagedat[j,1]==villagedat[i,1]) {
		vilmeaneduca[j]=mean(educdat[j,],na.rm=TRUE)
		vilmeanpropfa[j]=mean(propfdat[j,],na.rm=TRUE)
		vilmeanagea[j]=mean(agedat[j,],na.rm=TRUE)
		vilmeanland1a[j]=mean(landdat1[j,],na.rm=TRUE)
	}}
	vilmeaneduc[i]=mean(vilmeaneduca,na.rm=TRUE)
	vilmeanpropf[i]=mean(vilmeanpropfa,na.rm=TRUE)
	vilmeanage[i]=mean(vilmeanagea,na.rm=TRUE)
	vilmeanland1[i]=mean(vilmeanland1a,na.rm=TRUE)
	}

educ=numeric()
propf=numeric()
age=numeric()
land1=numeric()
for (i in 1:hnum) {
	educ[i]=mean(educdat[i,],na.rm=TRUE)-vilmeaneduc[i]
	propf[i]=mean(propfdat[i,],na.rm=TRUE)-vilmeanpropf[i]
	age[i]=mean(agedat[i,],na.rm=TRUE)-vilmeanage[i]
	land1[i]=mean(landdat1[i,],na.rm=TRUE)-vilmeanland1[i]
	}	

#endowment shares

kk=vilcode

hnumv0=0
for (i in 1:hnum) {
	if (villagedat[i,1]<kk) {
		hnumv0=hnumv0+1
		}}
hnumv=0
for (i in 1:hnum) {
	if (villagedat[i,1]==kk) {
		hnumv=hnumv+1
		}}

incdata=array(NA,c(hnum,tnum))
consdata=array(NA,c(hnum,tnum))
for (i in 1:hnum) {
for (j in 1:tnum) {
	if (incdat[i,j]>0) {
		incdata[i,j]=incdat[i,j]
		consdata[i,j]=consdat[i,j]
		}}}

low1=quantile(incdata[((hnumv0+1):(hnumv0+hnumv)),],0.025,na.rm=TRUE)
high1=quantile(incdata[((hnumv0+1):(hnumv0+hnumv)),],0.975,na.rm=TRUE)
clow1=quantile(consdata[((hnumv0+1):(hnumv0+hnumv)),],0.025,na.rm=TRUE)
chigh1=quantile(consdata[((hnumv0+1):(hnumv0+hnumv)),],0.975,na.rm=TRUE)

#compute village mean income and consumption
vilmeaninct=array(NA,c(nv,tnum))
vilmeaninc=numeric()
vilmeanconst=array(NA,c(nv,tnum))
vilmeancons=numeric()
for (k in 1:nv) {
for (l in 1:tnum) {
	incda=array(NA,c(hnum,ta))
	consda=array(NA,c(hnum,ta))
	hhsizea=array(NA,c(hnum,ta))
	for (i in 1:hnum) {
	for (j in 1:tnum) {
		if (villagedat[i,j]==k & tcode[i,j]==l) {
			incda[i,j]=incdata[i,j]
			consda[i,j]=consdata[i,j]
	}}}
	vilmeaninct[k,l]=mean(incda,na.rm=TRUE)
	vilmeanconst[k,l]=mean(consda,na.rm=TRUE)
}
vilmeaninc[k]=mean(vilmeaninct[k,])
vilmeancons[k]=mean(vilmeanconst[k,])
}

#reshape
vilmeaninctij=array(NA,c(hnum,tnum))
vilmeanconstij=array(NA,c(hnum,tnum))
for (i in 1:hnum) {
for (j in 1:tnum) {
	for (k in 1:nv) {
	if (villagedat[i,j]==k) {
	vilmeaninctij[i,j]=vilmeaninct[k,j]
	vilmeanconstij[i,j]=vilmeanconst[k,j]
}}}}

#compute ratio of mean consumption to income in the data	
consperincvt=array(NA,c(nv,tnum))
for (k in 1:nv) {
for (l in 1:tnum) {
	consperincvt[k,l]=vilmeanconst[k,l]/vilmeaninct[k,l]
	}}

#rescale income
incdatb=array(NA,c(hnum,ta))
for (i in 1:hnum) {
for (j in 1:tnum) {
	for (k in 1:nv) {
	if (villagedat[i,j]==k) {
	incdatb[i,j]=incdat[i,j]*consperincvt[k,j]
}}}}

#reshape
vilmeaninctb=array(NA,c(nv,tnum))
for (k in 1:nv) {
for (j in 1:tnum) {
	incdatba=array(NA,c(hnum,ta))
for (i in 1:hnum) {
	if (villagedat[i,j]==k) {
	incdatba[i,j]=incdatb[i,j]
}}
vilmeaninctb[k,j]=mean(incdatba,na.rm=TRUE)
}}

#update village means
vilmeanincbij=array(NA,c(hnum,tnum))
vilmeanconsbij=array(NA,c(hnum,tnum))
for (i in 1:hnum) {
for (j in 1:tnum) {
	for (k in 1:nv) {
	if (villagedat[i,j]==k) {
	vilmeanincbij[i,j]=vilmeaninctb[k,j]
	vilmeanconsbij[i,j]=vilmeanconst[k,j]
}}}}


# ESTIMATE INCOME PROCESSES
	
Sh1=8
Sh2=5
#Sh1 number of income states for hh1
#Sh2 number of income states for hh2

S=Sh1*Sh2

#hh

#lagged income
Lincdatb=array(NA,c(hnum,tnum))
for (i in 1:hnum) {
for (j in 1:(tnum-1)) {
	Lincdatb[i,j+1]=incdatb[i,j]
	}}

#create 4 groups
sh1r=array(NA,c(nv,2,2,Sh1))
P1r=array(NA,c(nv,2,2,Sh1,Sh1))
sh2r=array(NA,c(nv,Sh2))
P2r=array(NA,c(nv,Sh2,Sh2))
meancor=numeric()

meanincv=numeric()	
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	meanincv[i]=mean(incdatb[i,])
	}
medinc=quantile(meanincv,0.5,na.rm=TRUE)
hlmean=numeric()
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	if (meanincv[i]<=medinc) {
	hlmean[i]=1
	} else {
	hlmean[i]=2
	}}
cvincv=numeric()	
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	cvincv[i]=sd(incdatb[i,])/mean(incdatb[i,])
	}
medcvinc=quantile(cvincv,0.5,na.rm=TRUE)
hlcv=numeric()
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	if (cvincv[i]<=medcvinc) {
	hlcv[i]=1
	} else {
	hlcv[i]=2	
	}}
	
eig=array(NA,c(2,2,Sh1))

bmedinc=incdatb
for (i in (hnumv0+1):(hnumv0+hnumv)) {
  bmedinc[i,]=-(hlmean[i]-2)*incdatb[i,]
}
medinc
mean(bmedinc[(hnumv0+1):(hnumv0+hnumv),],na.rm=TRUE)

#ESTIMATE AR(1) PROCESS AND THEN APPROXIMATE IT BY A MARKOV CHAIN FOR EACH OF THE FOUR GROUPS

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
	incdat2l[tnum*(i-1)+j]=incdatb[i,j]
	consdat2l[tnum*(i-1)+j]=consdat[i,j]
	Lincdat2l[tnum*(i-1)+j]=Lincdatb[i,j]
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
		incdat02[i,j]=incdatb[i,j]
		consdat02[i,j]=consdat[i,j]
		}}}

#Markov chain for income of households
#points
jj=0
sh1=numeric(length=Sh1)
sh1[1:2]=c(1,1)
while ((sh1[1]-sh1[2])^2<0.0000000000001) {
  print('test')
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
	incdat2l[tnum*(i-1)+j]=incdatb[i,j]
	consdat2l[tnum*(i-1)+j]=consdat[i,j]
	Lincdat2l[tnum*(i-1)+j]=Lincdatb[i,j]
}}}}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (j in 1:tnum) {
if (hlmean[i]==2 | hlcv[i]==1) {
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
	if ((villagedat[i,j]==kk) && (hlmean[i]==1) && (hlcv[i]==2) && (incdat[i,j]>low1) && (incdat[i,j]<high1)) {
		incdat02[i,j]=incdatb[i,j]
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
eig[1,2,]=eigen(t(P1))$vector[,1]/sum(eigen(t(P1))$vector[,1])
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
	incdat2l[tnum*(i-1)+j]=incdatb[i,j]
	consdat2l[tnum*(i-1)+j]=consdat[i,j]
	Lincdat2l[tnum*(i-1)+j]=Lincdatb[i,j]
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
		incdat02[i,j]=incdatb[i,j]
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
	incdat2l[tnum*(i-1)+j]=incdatb[i,j]
	consdat2l[tnum*(i-1)+j]=consdat[i,j]
	Lincdat2l[tnum*(i-1)+j]=Lincdatb[i,j]
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
		incdat02[i,j]=incdatb[i,j]
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

(0.1886173+0.6256918+0.5503409+0.3653036)/4

#SIMULATE INCOME PROCESS FOR VILLAGE

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

#ESTIMATE AR(1)

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
	
save.image("Laczo2015/allIncH1")

sh2rd=sh2r
