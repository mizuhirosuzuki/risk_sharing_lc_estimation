#Vuong tests
#village 1

setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))
load("Prs1me_nog")
mlipnh1me=mlipnh1
mlip1me=mlip1
grpnh1me=grpnh1
grp1me=grp1
Apnh1me=Apnh1
Ap1me=Ap1
t(xPrsnhme)
t(xPrsme_nog)
load("Hom1incHme")
grlcnh1me=grlcnh1
mlilcnh1me=mlilcnh1
Alcnh1me=Alcnh1
t(xLcnh1incHme)
xLcnh1incHmeme=xLcnh1incHme
load("Het1incHme_nog")
mlipnh1=mlipnh1me
mlip1=mlip1me
grpnh1=grpnh1me
grp1=grp1me
Apnh1=Apnh1me
Ap1=Ap1me
grlcnh1=grlcnh1me
mlilcnh1=mlilcnh1me
Alcnh1=Alcnh1me
xLcnh1incHme=xLcnh1incHmeme
t(xLc1incHme_nog)

sigPrs=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
  sigPrs[m]=1+xPrsme_nog[1]*educ[m]+xPrsme_nog[2]*propf[m]+xPrsme_nog[3]*age[m]+xPrsme_nog[4]*land1[m]
}
mean(sigPrs,na.rm=TRUE)
sd(sigPrs,na.rm=TRUE)
min(sigPrs,na.rm=TRUE)
max(sigPrs,na.rm=TRUE)

sigi=numeric()
maxsig=min(xLc1incHme_nog[2]+3,max(sig2))
minsig=max(0.1,xLc1incHme_nog[2]-3)
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigi[m]=min(max(xLc1incHme_nog[2]+xLc1incHme_nog[2]*xLc1incHme_nog[3]*educ[m]+xLc1incHme_nog[2]*xLc1incHme_nog[4]*propf[m]+xLc1incHme_nog[2]*xLc1incHme_nog[5]*age[m]+xLc1incHme_nog[2]*xLc1incHme_nog[6]*land1[m],minsig),maxsig)
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
meansig2=mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-(mean(sigi[(hnumv0+1):(hnumv0+hnumv)])-xLc1incHme_nog[2])*hnumv/(hnumv-sigcount)

sigLc=sigi
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	if ((sigi[m]>minsig) & (sigi[m]<maxsig)) {
 	sigLc[m]=sigi[m]*meansig2/mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
}}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigLc[m]=max(sigLc[m],minsig)
	sigLc[m]=min(sigLc[m],maxsig)
}

mean(sigLc,na.rm=TRUE)
sd(sigLc,na.rm=TRUE)
min(sigLc,na.rm=TRUE)
max(sigLc,na.rm=TRUE)

#p
Bb=array(NA,c(5,5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,,i,t]=grp1[,i+hnumv0,t]%*%t(grp1[,i+hnumv0,t])
}}
Bc=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {
Bc[m,mm]=sum(Bb[m,mm,,],na.rm=TRUE)
}}
Bp1=Bc

#pnh
Bb=array(NA,c(hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[i,t]=grpnh1[i+hnumv0,t]^2
}}
Bc=sum(Bb,na.rm=TRUE)
Bpnh1=Bc

#lcnh
Bb=array(NA,c(5,5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,,i,t]=grlcnh1[i+hnumv0,t,]%*%t(grlcnh1[i+hnumv0,t,])
}}
Bc=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {
Bc[m,mm]=sum(Bb[m,mm,,],na.rm=TRUE)
}}
Blcnh1=Bc

#lc
Bb=array(NA,c(9,9,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,,i,t]=grlc1[i+hnumv0,t,]%*%t(grlc1[i+hnumv0,t,])
}}
Bc=array(NA,c(9,9))
for (m in 1:9) {
for (mm in 1:9) {
Bc[m,mm]=sum(Bb[m,mm,,],na.rm=TRUE)
}}
Blc1=Bc

#p & pnh
Bb=array(NA,c(5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,i,t]=grp1[,i+hnumv0,t]*grpnh1[i+hnumv0,t]
}}
Bc=numeric()
for (m in 1:5) {
Bc[m]=sum(Bb[m,,],na.rm=TRUE)
}
Bppnh1=Bc

#lcnh & pnh
Bb=array(NA,c(5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,i,t]=grlcnh1[i+hnumv0,t,]*grpnh1[i+hnumv0,t]
}}
Bc=numeric()
for (m in 1:5) {
Bc[m]=sum(Bb[m,,],na.rm=TRUE)
}
Blcnhpnh1=Bc

#lc & pnh
Bb=array(NA,c(9,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,i,t]=grlc1[i+hnumv0,t,]*grpnh1[i+hnumv0,t]
}}
Bc=numeric()
for (m in 1:9) {
Bc[m]=sum(Bb[m,,],na.rm=TRUE)
}
Blcpnh1=Bc

#p & lcnh
Bb=array(NA,c(5,5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,,i,t]=grp1[,i+hnumv0,t]%*%t(grlcnh1[i+hnumv0,t,])
}}
Bc=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {
Bc[m,mm]=sum(Bb[m,mm,,],na.rm=TRUE)
}}
Blcnhp1=Bc

#lc & p
Bb=array(NA,c(9,5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,,i,t]=grlc1[i+hnumv0,t,]%*%t(grp1[,i+hnumv0,t])
}}
Bc=array(NA,c(9,5))
for (m in 1:9) {
for (mm in 1:5) {
Bc[m,mm]=sum(Bb[m,mm,,],na.rm=TRUE)
}}
Blcp1=Bc

#lc & lcnh
Bb=array(NA,c(9,5,hnumv,(tnum-1)))
for (i in 1:hnumv) {
for (t in 1:(tnum-1)) {
Bb[,,i,t]=grlc1[i+hnumv0,t,]%*%t(grlcnh1[i+hnumv0,t,])
}}
Bc=array(NA,c(9,5))
for (m in 1:9) {
for (mm in 1:5) {
Bc[m,mm]=sum(Bb[m,mm,,],na.rm=TRUE)
}}
Blclcnh1=Bc

#the tests

#p vs pnh

library(MASS)
W=array(NA,c(6,6))
W[1:5,1:5]=-Bp1%*%ginv(Ap1)
W[6,6]=Bpnh1/Apnh1
W[6,1:5]=Bppnh1%*%ginv(Ap1)
W[1:5,6]=-Bppnh1/Apnh1

ev=eigen(W,only.values=TRUE)
ev
eval=ev$values
evalues=numeric()
for (ii in 1:6) {
  evalues[ii]=Re(eval[ii])
}

xa=mlip1-mlipnh1
LR=2*sum(xa,na.rm=TRUE)
LR

aa2=numeric()
reject=numeric(length=100000)
for (i in 1:100000) {
	reject[i]=0
	aa1=rchisq(6,1,ncp=0)
	aa2[i]=evalues%*%aa1
	if (aa2[i]<LR) {
		reject[i]=1
		}}
pvPrsPrsnh=1-mean(reject)
LRPrsPrsnh=LR
LRPrsPrsnh
pvPrsPrsnh

#lcnh vs pnh

library(MASS)
W=array(NA,c(6,6))
W[1:5,1:5]=-Blcnh1%*%ginv(Alcnh1)
W[6,6]=Bpnh1/Apnh1
W[6,1:5]=Blcnhpnh1%*%ginv(Alcnh1)
W[1:5,6]=-Blcnhpnh1/Apnh1

ev=eigen(W,only.values=TRUE)
ev

xa=mlilcnh1[(hnumv0+1):(hnumv0+hnumv),]-mlipnh1[(hnumv0+1):(hnumv0+hnumv),]
LR=2*sum(xa,na.rm=TRUE)
LR

aa4=numeric()
reject=numeric(length=100000)
for (i in 1:100000) {
	reject[i]=0
	aa1=rchisq(6,1,ncp=0)
	aa4[i]=ev$values%*%aa1
	if (aa4[i]<LR) {
		reject[i]=1
		}}
pvLcnhPrsnh=1-mean(reject)
LRLcnhPrsnh=LR
LRLcnhPrsnh
pvLcnhPrsnh

#lc vs pnh

library(MASS)
W=array(NA,c(10,10))
W[1:9,1:9]=-Blc1%*%ginv(Alc1)
W[10,10]=Bpnh1/Apnh1
W[10,1:9]=Blcpnh1%*%ginv(Alc1)
W[1:9,10]=-Blcpnh1/Apnh1

ev=eigen(W,only.values=TRUE)
ev

xa=mlilc1[(hnumv0+1):(hnumv0+hnumv),]-mlipnh1[(hnumv0+1):(hnumv0+hnumv),]
LR=2*sum(xa,na.rm=TRUE)
LR

aa2=numeric()
reject=numeric(length=100000)
for (i in 1:100000) {
	reject[i]=0
	aa1=rchisq(10,1,ncp=0)
	aa2[i]=ev$values%*%aa1
	if (aa2[i]<LR) {
		reject[i]=1
		}}
pvLcPrsnh=1-mean(reject)
LRLcPrsnh=LR
LRLcPrsnh
pvLcPrsnh

#p vs lcnh

library(MASS)
W=array(NA,c(10,10))
W[1:5,1:5]=-Bp1%*%ginv(Ap1)
W[6:10,6:10]=Blcnh1%*%ginv(Alcnh1)
W[6:10,1:5]=t(Blcnhp1)%*%ginv(Ap1)
W[1:5,6:10]=-Blcnhp1%*%ginv(Alcnh1)

ev=eigen(W,only.values=TRUE)
ev

xa=mlip1[(hnumv0+1):(hnumv0+hnumv),]-mlilcnh1[(hnumv0+1):(hnumv0+hnumv),]
LR=2*sum(xa,na.rm=TRUE)
LR

aa2=numeric()
reject=numeric(length=100000)
for (i in 1:100000) {
	reject[i]=0
	aa1=rchisq(10,1,ncp=0)
	aa2[i]=ev$values%*%aa1
	if (aa2[i]<LR) {
		reject[i]=1
		}}
pvPrsLcnh=1-mean(reject)
LRPrsLcnh=LR
LRPrsLcnh
pvPrsLcnh


#lc vs lcnh

library(MASS)
W=array(NA,c(14,14))
W[1:9,1:9]=-Blc1%*%ginv(Alc1)
W[10:14,10:14]=Blcnh1%*%ginv(Alcnh1)
W[10:14,1:9]=t(Blclcnh1)%*%ginv(Alc1)
W[1:9,10:14]=-Blclcnh1%*%ginv(Alcnh1)

ev=eigen(W,only.values=TRUE)
ev

xa=mlilc1[(hnumv0+1):(hnumv0+hnumv),]-mlilcnh1[(hnumv0+1):(hnumv0+hnumv),]
LR=2*sum(xa,na.rm=TRUE)
LR

aa2=numeric()
reject=numeric(length=100000)
for (i in 1:100000) {
	reject[i]=0
	aa1=rchisq(14,1,ncp=0)
	aa2[i]=ev$values%*%aa1
	if (aa2[i]<LR) {
		reject[i]=1
		}}
pvLcLcnh=1-mean(reject)
LRLcLcnh=LR
LRLcLcnh
pvLcLcnh

#lc vs p

library(MASS)
W=array(NA,c(14,14))
W[1:9,1:9]=-Blc1%*%ginv(Alc1)
W[10:14,10:14]=Bp1%*%ginv(Ap1)
W[10:14,1:9]=t(Blcp1)%*%ginv(Alc1)
W[1:9,10:14]=-Blcp1%*%ginv(Ap1)

ev=eigen(W,only.values=TRUE)
ev

xa=mlilc1[(hnumv0+1):(hnumv0+hnumv),]-mlip1[(hnumv0+1):(hnumv0+hnumv),]
LR=2*sum(xa,na.rm=TRUE)
LR

aa2=numeric()
reject=numeric(length=100000)
for (i in 1:100000) {
	reject[i]=0
	aa1=rchisq(14,1,ncp=0)
	aa2[i]=ev$values%*%aa1
	if (aa2[i]<LR) {
		reject[i]=1
		}}
pvLcPrs=1-mean(reject)
LRLcPrs=LR
LRLcPrs
pvLcPrs

save.image("Het1incHallme")


