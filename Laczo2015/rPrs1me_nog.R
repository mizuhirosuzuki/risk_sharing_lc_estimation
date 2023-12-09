# setwd("~/Documents/GitHub/laczo_rep/quarto/Laczo2015/") #change appropriately
rm(list = ls(all = TRUE))
load("~/Documents/GitHub/laczo_rep/quarto/Laczo2015/allIncH1")

#with measurement error in consumption at time t-1 as well

set.seed(81)
NS=50
eps=array(NA,c(hnum,tnum,NS,3))
for (i in 1:hnum) {
for (t in 1:tnum) {
eps[i,t,,1]=rnorm(n=NS) #cons	
eps[i,t,,2]=rnorm(n=NS) #inc
	}
eps[i,t,,3]=rnorm(n=NS) #eta	
	}

#PERFECT RISK SHARING WITH HOMOGENOUS PREFERENCES

lML=function(yy) {
gamc=yy
mli1=array(0,c(hnum,tnum-1,NS))
mli=matrix(c(0),nrow=(hnum),ncol=(tnum-1))
for (sim in 1:NS) {

consdatstar=consdat
for (t in 1:(tnum-1)) {
for (i in (hnumv0+1):(hnumv0+hnumv)) {
consdatstar[i,t]=consdat[i,t]/exp(eps[i,t,sim,1]*sqrt(gamc))
}
meanconsstar1=mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meancons1=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
consdatstar[,t]=consdatstar[,t]*meancons1/meanconsstar1
}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	lhs=log(consdat[i,(t+1)])-log(vilmeanconsbij[i,(t+1)])
	gt=log(consdatstar[i,t])-log(vilmeanconsbij[i,t])
	seg1=((hnumv-1)/hnumv)^2+(hnumv-1)/hnumv^2
	mli1[i,t,sim]=dnorm(lhs,mean=gt,sd=sqrt(seg1*gamc))
	}}}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	mli[i,t]=log(mean(mli1[i,t,]))
}}
logML=sum(mli,na.rm=TRUE)
return(-logML)
}

minpar=0.02
maxpar=0.2

sol=optim(0.1,lML,method="L-BFGS-B",lower=minpar,upper=maxpar,control=list(trace=5,maxit=200),hessian=TRUE)
optp=sol$par
lhv=-sol$value
hessp1=sol$hessian

mlipnh1=array(NA,c(hnum,(tnum-1)))
grpnh1=array(NA,c((hnum),(tnum-1)))

for (mmh in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
		
lmli=function(yy) {
	
gamc=yy[1]

mli1=numeric()
for (sim in 1:NS) {

consdatstar=consdat
for (t in 1:(tnum-1)) {
for (i in (hnumv0+1):(hnumv0+hnumv)) {
consdatstar[i,t]=consdat[i,t]/exp(eps[i,t,sim,1]*sqrt(gamc))
}
meanconsstar1=mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meancons1=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
consdatstar[,t]=consdatstar[,t]*meancons1/meanconsstar1
}

	lhs=log(consdat[mmh,(t+1)])-log(vilmeanconsbij[mmh,(t+1)])
	gt=log(consdatstar[mmh,t])-log(vilmeanconsbij[mmh,t])
	seg1=((hnumv-1)/hnumv)^2+(hnumv-1)/hnumv^2
	mli1[sim]=dnorm(lhs,mean=gt,sd=sqrt(seg1*gamc))
	}
mli=log(mean(mli1))
return(mli)
}

mlipnh1[mmh,t]=lmli(optp)
gg=numericDeriv(quote(lmli(optp)),"optp")
grpnh1[mmh,t]=attr(gg,"gradient")

}}

grad1c=array(NA,c(hnum,(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
grad1c[i,t]=grpnh1[i,t]^2
}}
grad1d=array(NA,c(hnum))
for (i in 1:hnum) {
grad1d[i]=sum(grad1c[i,],na.rm=TRUE)
}
grad1e=sum(grad1d,na.rm=TRUE)

grad1f=array(NA,c(hnum,(tnum-1),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-2)) {
for (r in (t+1):(tnum-1)) {
grad1f[i,t,r]=2*grpnh1[i,t]*grpnh1[i,r]
}}}
grad1ff=array(NA,c(hnum,(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
grad1ff[i,t]=sum(grad1f[i,t,],na.rm=TRUE)
}}
grad1g=array(NA,c(hnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
grad1g[i]=sum(grad1ff[i,],na.rm=TRUE)
}
grad1h=sum(grad1g,na.rm=TRUE)

Bpnh1=grad1e+grad1h

Apnh1=hessp1
	
Covpnh1=1/Apnh1*Bpnh1/Apnh1

sdoptp=sqrt(Covpnh1)

xPrsnhme=c(optp,sdoptp,lhv,nn)
xPrsnhme

#PERFECT RISK SHARING WITH HETEROGENOUS PREFERENCES

lML=function(yy) {
a1=yy[1]
a2=yy[2]
a3=yy[3]
a4=yy[4]
gamc=yy[5]
mli1=array(0,c(hnum,tnum-1,NS))
mli=matrix(c(0),nrow=(hnum),ncol=(tnum-1))

maxsig=4 #not binding 
minsig=0.1 #not binding
sigi=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigi[m]=min(max(1+a1*educ[m]+a2*propf[m]+a3*age[m]+a4*land1[m],minsig),maxsig) #sigi
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
meansig2=mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-(mean(sigi[(hnumv0+1):(hnumv0+hnumv)])-1)*hnumv/(hnumv-sigcount)

sigma3=sigi
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	if ((sigi[m]>minsig) & (sigi[m]<maxsig)) {
 	sigma3[m]=sigi[m]*meansig2/mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
}}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigma3[m]=max(sigma3[m],minsig)
	sigma3[m]=min(sigma3[m],maxsig)
}
sigi=sigma3-1

for (sim in 1:NS) {

consdatstar=consdat
for (t in 1:(tnum-1)) {
for (i in (hnumv0+1):(hnumv0+hnumv)) {
consdatstar[i,t]=consdat[i,t]/exp(eps[i,t,sim,1]*sqrt(gamc))
}
meanconsstar1=mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meancons1=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
consdatstar[,t]=consdatstar[,t]*meancons1/meanconsstar1
}

for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	lhs=log(consdat[i,(t+1)])-log(vilmeanconsbij[i,(t+1)])
	gt=(1+sigi[i])*log(consdatstar[i,t])-log(vilmeanconsbij[i,t])-sigi[i]*log(consdat[i,(t+1)])
	seg1=((hnumv-1)/hnumv+sigi[i])^2+(hnumv-1)/hnumv^2
	mli1[i,t,sim]=dnorm(lhs,mean=gt,sd=sqrt(seg1*gamc))
	}}}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	mli[i,t]=log(mean(mli1[i,t,]))
}}
logML=sum(mli,na.rm=TRUE)
return(-logML)
}

xPrs=numeric()
xPrs[1:5]=c(0.4242611094730358023419,-0.1132560053156791424200,-0.0030469865039083081013,-0.0177119155358136347311,0.0940451640253737869424)
startpar=xPrs[1:5]
minpar=c(xPrs[1]-0.3,xPrs[2]-0.2,xPrs[3]-0.03,xPrs[4]-0.03,xPrs[5]-0.05)
maxpar=c(xPrs[1]+0.1,xPrs[2]+0.2,xPrs[3]+0.03,xPrs[4]+0.03,xPrs[5]+0.02)
lML(startpar)
sol=optim(startpar,lML,method="L-BFGS-B",lower=minpar,upper=maxpar,control=list(trace=5,maxit=200),hessian=FALSE)
optpa=sol$par
lhv=-sol$value
hessp1=sol$hessian
optpa

#0.438938542,-0.071866563,-0.003141415,-0.024138893,0.049043021
#-1.24412

#do some grid search
aa4=numeric()
aa4[1]=0
for (i in 2:51) {
  aa4[i]=aa4[i-1]+(0.5-aa4[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[i1],-0.071866563,-0.003141415,-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2,na.rm=TRUE)
ind4=which.min(lcMLgrid2)
ind4
aa4[ind4]

aa4=numeric()
aa4[1]=0.43
for (i in 2:51) {
  aa4[i]=aa4[i-1]+(0.45-aa4[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[i1],-0.071866563,-0.003141415,-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2,na.rm=TRUE)
ind4=which.min(lcMLgrid2)
ind4
aa4[ind4]
#value has improved

aa5=numeric()
aa5[1]=-1
for (i in 2:51) {
  aa5[i]=aa5[i-1]+(1-aa5[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[i1],-0.003141415,-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind5=which.min(lcMLgrid2)
ind5
aa5[ind5]
plot(aa5,lcMLgrid2)

aa5=numeric()
aa5[1]=-0.5
for (i in 2:51) {
  aa5[i]=aa5[i-1]+(0-aa5[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[i1],-0.003141415,-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind5=which.min(lcMLgrid2)
ind5
aa5[ind5]
plot(aa5,lcMLgrid2)

aa5=numeric()
aa5[1]=-0.1
for (i in 2:51) {
  aa5[i]=aa5[i-1]+(-0.06-aa5[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[i1],-0.003141415,-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind5=which.min(lcMLgrid2)
ind5
aa5[ind5]
minp5=aa5[ind5]
plot(aa5,lcMLgrid2)
#value has improved

aa6=numeric()
aa6[1]=-0.03
for (i in 2:51) {
  aa6[i]=aa6[i-1]+(0.03-aa6[1])/50
}	
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],aa6[i1],-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind6=which.min(lcMLgrid2)
ind6
aa6[ind6]
plot(aa6,lcMLgrid2)

aa6=numeric()
aa6[1]=-0.01
for (i in 2:51) {
  aa6[i]=aa6[i-1]+(0-aa6[1])/50
}	
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],aa6[i1],-0.024138893,0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind6=which.min(lcMLgrid2)
ind6
aa6[ind6]
plot(aa6,lcMLgrid2)
#value hasn't improved, stick with optp[3]

aa7=numeric()
aa7[1]=-0.4
for (i in 2:51) {
  aa7[i]=aa7[i-1]+(0.2-aa7[1])/50
}	
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],optpa[3],aa7[i1],0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind7=which.min(lcMLgrid2)
ind7
aa7[ind7]
minp7=aa7[ind7]
plot(aa7,lcMLgrid2)

aa7=numeric()
aa7[1]=-0.1
for (i in 2:51) {
  aa7[i]=aa7[i-1]+(0-aa7[1])/50
}	
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],optpa[3],aa7[i1],0.049043021)
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind7=which.min(lcMLgrid2)
ind7
aa7[ind7]
minp7=aa7[ind7]
plot(aa7,lcMLgrid2)
#value has improved

aa8=numeric()
aa8[1]=0.044
for (i in 2:51) {
  aa8[i]=aa8[i-1]+(0.054-aa8[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],optpa[3],aa7[ind7],aa8[i1])
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind8=which.min(lcMLgrid2)
ind8
aa8[ind8]
plot(aa8,lcMLgrid2)

aa8=numeric()
aa8[1]=0.048
for (i in 2:51) {
  aa8[i]=aa8[i-1]+(0.05-aa8[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],optpa[3],aa7[ind7],aa8[i1])
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind8=which.min(lcMLgrid2)
ind8
aa8[ind8]
plot(aa8,lcMLgrid2)

aa5=numeric()
aa5[1]=-0.08
for (i in 2:51) {
  aa5[i]=aa5[i-1]+(-0.07-aa5[1])/50
}
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[i1],optpa[3],aa7[ind7],aa8[ind8])
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind5=which.min(lcMLgrid2)
ind5
aa5[ind5]
minp5=aa5[ind5]
plot(aa5,lcMLgrid2)
#value has improved

aa7=numeric()
aa7[1]=-0.03
for (i in 2:51) {
  aa7[i]=aa7[i-1]+(-0.02-aa7[1])/50
}	
lcMLgrid2=numeric()	
for (i1 in 1:51) {
  yy1=c(aa4[ind4],aa5[ind5],optpa[3],aa7[i1],aa8[ind8])
  lcMLgrid2[i1]=lML(yy1)
}
min(lcMLgrid2)
ind7=which.min(lcMLgrid2)
ind7
aa7[ind7]
minp7=aa7[ind7]
plot(aa7,lcMLgrid2)
#value has improved

c(aa4[ind4],aa5[ind5],optpa[3],aa7[ind7],aa8[ind8])
#0.4388,-0.0748,-0.003141415,-0.0268,0.049
#-1.245141

startpar=c(aa4[ind4]-0.0001,aa5[ind5]-0.0001,optpa[3],aa7[ind7],aa8[ind8]-0.0001)
#experimenting with the starting values so that the optimization algorithm starts iterating

sol=optim(startpar,lML,method="L-BFGS-B",lower=minpar,upper=maxpar,control=list(trace=5,maxit=200),hessian=TRUE)
optp=sol$par
lhv=-sol$value
hessp1=sol$hessian

mlip1=array(NA,c(hnum,(tnum-1)))
grp1=array(NA,c(5,(hnum),(tnum-1)))

for (mmh in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
		
lmli=function(yy) {
	
a1=yy[1]
a2=yy[2]
a3=yy[3]
a4=yy[4]
gamc=yy[5]

mli1=numeric()

maxsig=4 #not binding 
minsig=0.1 #not binding
sigi=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigi[m]=min(max(1+a1*educ[m]+a2*propf[m]+a3*age[m]+a4*land1[m],minsig),maxsig) #sigi
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
meansig2=mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)-(mean(sigi[(hnumv0+1):(hnumv0+hnumv)])-1)*hnumv/(hnumv-sigcount)

sigma3=sigi
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	if ((sigi[m]>minsig) & (sigi[m]<maxsig)) {
 	sigma3[m]=sigi[m]*meansig2/mean(sigi2[(hnumv0+1):(hnumv0+hnumv)],na.rm=TRUE)
}}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigma3[m]=max(sigma3[m],minsig)
	sigma3[m]=min(sigma3[m],maxsig)
}
sigi=sigma3-1

for (sim in 1:NS) {

consdatstar=consdat
for (i in (hnumv0+1):(hnumv0+hnumv)) {
consdatstar[i,t]=consdat[i,t]/exp(eps[i,t,sim,1]*sqrt(gamc))
}
meanconsstar1=mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meancons1=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
consdatstar[,t]=consdatstar[,t]*meancons1/meanconsstar1

	lhs=log(consdat[mmh,(t+1)])-log(vilmeanconsbij[mmh,(t+1)])
	gt=(1+sigi[mmh])*log(consdatstar[mmh,t])-log(vilmeanconsbij[mmh,t])-sigi[mmh]*log(consdat[mmh,(t+1)])
	seg1=((hnumv-1)/hnumv+sigi[mmh])^2+(hnumv-1)/hnumv^2
	mli1[sim]=dnorm(lhs,mean=gt,sd=sqrt(seg1*gamc))
	}
mli=log(mean(mli1))
return(mli)
}

mlip1[mmh,t]=lmli(optp)
gg=numericDeriv(quote(lmli(optp)),"optp")
grp1[,mmh,t]=attr(gg,"gradient")

}}

grp1c=array(NA,c(5,5,(hnum),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
grp1c[,,i,t]=grp1[,i,t]%*%t(grp1[,i,t])
}}
grp1d=array(NA,c(5,5,(hnum)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (m in 1:5) {
for (mm in 1:5) {	
grp1d[m,mm,i]=sum(grp1c[m,mm,i,],na.rm=TRUE)
}}}
grp1e=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {	
grp1e[m,mm]=sum(grp1d[m,mm,],na.rm=TRUE)
}}

grp1f=array(NA,c(5,5,(hnum),(tnum-1),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-2)) {
if (t>1) {
for (r in 2:(t-1)) {
grp1f[,,i,t,r]=grp1[,i,t]%*%t(grp1[,i,r])
}}
for (r in (t+1):(tnum-1)) {
grp1f[,,i,t,r]=grp1[,i,t]%*%t(grp1[,i,r])
}}}

grp1ff=array(NA,c(5,5,(hnum),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
for (m in 1:5) {
for (mm in 1:5) {	
grp1ff[m,mm,i,t]=sum(grp1f[m,mm,i,t,],na.rm=TRUE)
}}}}

grp1g=array(NA,c(5,5,(hnum)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (m in 1:5) {
for (mm in 1:5) {	
grp1g[m,mm,i]=sum(grp1ff[m,mm,i,],na.rm=TRUE)
}}}
grp1h=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {	
grp1h[m,mm]=sum(grp1g[m,mm,],na.rm=TRUE)
}}

Bp1=grp1e+grp1h

Ap1=hessp1

library(MASS)
Covp1=ginv(Ap1)%*%Bp1%*%ginv(Ap1)

sdoptp=sqrt(diag(Covp1))

xPrsme_nog=c(optp,sdoptp,lhv,nn)

optp
sdoptp

xPrsme_nog

save.image("Prs1me_nog")



