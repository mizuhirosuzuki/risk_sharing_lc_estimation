setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))
load("Hom1incH")

fg=function(xx) {
	meancode=xx[1]
	cvcode=xx[2]
	del=xx[3]
	si=xx[4]
	ph=xx[5]
	l1=which.min(abs(d-del))
	if (d[l1]<=del & l1<length(d)) {
		l1=l1+1
		}
	intp1=(del-d[l1-1])/(d[l1]-d[l1-1])
	l2=which.min(abs(si-sig2))
	if (sig2[l2]<=si & l2<length(sig2)) {
		l2=l2+1
		}
	intp2=(si-sig2[l2-1])/(sig2[l2]-sig2[l2-1])
	l3=which.min(abs(ph-pphi))
	if (pphi[l3]<=ph & l3<length(pphi)) {
		l3=l3+1
		}
	intp3=(ph-pphi[l3-1])/(pphi[l3]-pphi[l3-1])
	
	xintc=array(0,c(S,2))
	for (ii4 in 1:S) {
	for (ii5 in 1:2) {
	xintaa=intp1*(xxint_all[meancode,cvcode,l1,l2-1,l3-1,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2-1,l3-1,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2-1,l3-1,ii4,ii5]
	xintba=intp1*(xxint_all[meancode,cvcode,l1,l2,l3-1,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2,l3-1,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2,l3-1,ii4,ii5]
	xintab=intp1*(xxint_all[meancode,cvcode,l1,l2-1,l3,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2-1,l3,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2-1,l3,ii4,ii5]
	xintbb=intp1*(xxint_all[meancode,cvcode,l1,l2,l3,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2,l3,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2,l3,ii4,ii5]
	xinta=intp2*(xintba-xintaa)+xintaa
	xintb=intp2*(xintbb-xintab)+xintab
	xintc[ii4,ii5]=intp3*(xintb-xinta)+xinta
	}}
	return(xintc)
	}

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

lcML=function(yy) {

delta3=yy[1]
sigma3=yy[2]
phi3=yy[3]
gamsqc=yy[4]
gamsqy=yy[5]

xint=array(NA,c(2,2,S,2))
for (jj5 in 1:2) {
for (jj6 in 1:2) {
yy3=c(jj5,jj6,delta3,sigma3,phi3)
xss=fg(yy3)
xint[jj5,jj6,,1]=xss[1:S]
xint[jj5,jj6,,2]=xss[(S+1):(2*S)]
}}

mli=array(NA,c(hnum,(tnum-1)))
mli1=array(NA,c(hnum,(tnum-1),NS))

for (sim in 1:NS) {
lhs=matrix(c(10000),nrow=hnum,ncol=(tnum-1))
rhs=matrix(c(10000),nrow=hnum,ncol=(tnum-1))
xt=array(NA,c(hnum,(tnum-1)))
xy=array(NA,c(hnum,(tnum-1)))
clc=array(NA,c(hnum,(tnum-1)))
clv=array(NA,c(hnum,(tnum-1)))
irl=array(NA,c(hnum,(tnum-1)))

consdatstar=consdat
incdatstar=incdatb

for (t in 1:(tnum-1)) {
for (m in (hnumv0+1):(hnumv0+hnumv)) {
consdatstar[m,t]=consdat[m,t]/exp(eps[m,t,sim,1]*sqrt(gamsqc))
incdatstar[m,t]=incdatb[m,(t+1)]/exp(eps[m,(t+1),sim,2]*sqrt(gamsqy))
}
meanconsstar1=mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meanincstar1=mean(incdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meancons1=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
meaninc1=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
consdatstar[,t]=consdatstar[,t]*meancons1/meanconsstar1
incdatstar[,t]=incdatstar[,t]*meaninc1/meanincstar1
}

irls0=numeric()
for (t in 1:(tnum-1)) {
irlz2=which.min((sh2-mean(incdatstar[(hnumv0+1):(hnumv0+hnumv),t]))^2)
for (m in (hnumv0+1):(hnumv0+hnumv)) {
xy[m,t]=uup(mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t]),sigma3)/uup(consdatstar[m,t],sigma3)
irlz1=which.min((sh1-incdatstar[m,t])^2)
irl[m,t]=Sh2*(irlz1-1)+irlz2
}
irls0[t]=irlz2
}

count3=numeric()
for (t in 1:(tnum-1)) {
s02a=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
s02a[m]=s0[irl[m,t]]
}
s02b=sh2[irls0[t]]*hnumv
count=1
count2=0
itercount=1
xtiter=rep(1,hnum)
while (count>0) {
	count=0
	xtiter2=xtiter
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	if (xtiter[m]==1) {
if (xy[m,t]>=xint[hlmean[m],hlcv[m],irl[m,t],1]) {
	xt[m,t]=xy[m,t]
	xtiter[m]=1
	} else {
	count=count+1
	count2=count2+1
	xt[m,t]=xint[hlmean[m],hlcv[m],irl[m,t],1]
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

seg1=((hnumv-1)/hnumv)^2+(hnumv-1)/hnumv^2
for (m in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
lhs[m,t]=(log(consdat[m,(t+1)])-log(mean(consdat[(hnumv0+1):(hnumv0+hnumv),(t+1)])))
rhs[m,t]=(log(clc[m,t])-log(clv[m,t]))
mli1[m,t,sim]=dnorm(lhs[m,t],mean=rhs[m,t],sd=sqrt(seg1*gamsqc))
}}}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
mli[m,t]=log(mean(mli1[m,t,]))
}}
logML=sum(mli,na.rm=TRUE)
rm(lhs,rhs,mli1,mli,xt,xy,clc,clv,irl)
return(-logML)
}

xLcnh1incH=numeric()
xLcnh1incH=c(0.9732148444570178,3.2138628546002050,0.3512388664510934,0.0653953699451726)

startpar=c(xLcnh1incH[1],xLcnh1incH[2],xLcnh1incH[3],xLcnh1incH[4],xLcnh1incH[4])
lcML(startpar)
minpar=c(xLcnh1incH[1]-0.03,xLcnh1incH[2]-0.3,max(xLcnh1incH[3]-0.03,0),0.03,0.05)
maxpar=c(min(xLcnh1incH[1]+0.03,0.98),xLcnh1incH[2]+0.3,min(xLcnh1incH[3]+0.03,max(pphi)),xLcnh1incH[4]+0.02,xLcnh1incH[4]+0.03)
lcML(minpar)
lcML(maxpar)

ptm <- proc.time()
sol=optim(startpar,lcML,method="L-BFGS-B",lower=minpar,upper=maxpar,control=list(trace=5,maxit=50),hessian=TRUE)
optp=sol$par
lhv=-sol$value
hesslcnh1=sol$hessian
optp

countslcnh=sol$counts
messagelcnh=sol$message

proc.time() - ptm

save.image("Hom1incHme")

mlilcnh1=array(NA,c(hnumv0+hnumv,(tnum-1)))
grlcnh1=array(NA,c(hnumv0+hnumv,(tnum-1),5))

for (mmh in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
		
lmli=function(yy) {
	
delta3=yy[1]
sigma3=yy[2]
phi3=yy[3]
gamsqc=yy[4]
gamsqy=yy[5]

xint=array(NA,c(2,2,S,2))
for (jj5 in 1:2) {
for (jj6 in 1:2) {
yy3=c(jj5,jj6,delta3,sigma3,phi3)
xss=fg(yy3)
xint[jj5,jj6,,1]=xss[1:S]
xint[jj5,jj6,,2]=xss[(S+1):(2*S)]
}}

mli1=numeric()
for (sim in 1:NS) {

consdatstar=consdat
incdatstar=incdatb

for (m in (hnumv0+1):(hnumv0+hnumv)) {
consdatstar[m,t]=consdat[m,t]/exp(eps[m,t,sim,1]*sqrt(gamsqc))
incdatstar[m,t]=incdatb[m,(t+1)]/exp(eps[m,(t+1),sim,2]*sqrt(gamsqy))
}
meanconsstar1=mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meanincstar1=mean(incdatstar[(hnumv0+1):(hnumv0+hnumv),t])
meancons1=mean(consdat[(hnumv0+1):(hnumv0+hnumv),t])
meaninc1=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
consdatstar[,t]=consdatstar[,t]*meancons1/meanconsstar1
incdatstar[,t]=incdatstar[,t]*meaninc1/meanincstar1

xt=numeric()
xy=numeric()
clc=numeric()
clv=numeric()
irl=numeric()

for (m in (hnumv0+1):(hnumv0+hnumv)) {
xy[m]=uup(mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t]),sigma3)/uup(consdatstar[m,t],sigma3)	
irlz=numeric()
irlz[1]=which.min((sh1-incdatstar[m,t])^2)
irlz[2]=which.min((sh2-mean(incdatstar[(hnumv0+1):(hnumv0+hnumv),t]))^2)
irl[m]=Sh2*(irlz[1]-1)+irlz[2]
}


s02a=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
s02a[m]=s0[irl[m]]
}
count=1
itercount=1
xtiter=rep(1,hnum)
while (count>0) {
	count=0
	xtiter2=xtiter
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	if (xtiter[m]==1) {
if (xy[m]>=xint[hlmean[m],hlcv[m],irl[m],1]) {
	xt[m]=xy[m]
	xtiter[m]=1
	} else {
	count=count+1
	xt[m]=xint[hlmean[m],hlcv[m],irl[m],1]
	xtiter[m]=0
	}
	clc[m]=s0[irl[m]]/(1+(hnumv-1)*xt[m]^(-1/sigma3))
	clv[m]=(s0[irl[m]]-clc[m])/(hnumv-1)
}}
s02a=s02a+(xtiter[(hnumv0+1):(hnumv0+hnumv)]-1)%*%clc[(hnumv0+1):(hnumv0+hnumv)]
for (m in (hnumv0+1):(hnumv0+hnumv)) {
if (xtiter[m]==1) {
	clcaa=s02a[m]/(1+(sum(xtiter[(hnumv0+1):(hnumv0+hnumv)])-1)*xt[m]^(-1/sigma3))
	clc[m]=max(clcaa,-1/clcaa)
	clv[m]=(s0[irl[m]]-clc[m])/(hnumv-1)
	xy[m]=uup(clv[m],sigma3)/uup(clc[m],sigma3)
	}}
itercount=itercount+1
}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
clv[m]=s0[irl[m]]/hnumv
}

seg1=((hnumv-1)/hnumv)^2+(hnumv-1)/hnumv^2
lhs=(log(consdat[mmh,(t+1)])-log(vilmeanconsbij[mmh,(t+1)]))
rhs=(log(clc[mmh])-log(clv[mmh]))
mli1[sim]=dnorm(lhs,mean=rhs,sd=sqrt(seg1*gamsqc))
}
mli=log(mean(mli1))
rm(xt,xy,clc,clv,irl)
return(mli)
}

mlilcnh1[mmh,t]=lmli(optp)
gg=numericDeriv(quote(lmli(optp)),"optp")
grlcnh1[mmh,t,]=attr(gg,"gradient")
}}

grfp1c=array(NA,c(5,5,hnum,(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
grfp1c[,,i,t]=grlcnh1[i,t,]%*%t(grlcnh1[i,t,])
}}
grfp1d=array(NA,c(5,5,hnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (m in 1:5) {
for (mm in 1:5) {	
grfp1d[m,mm,i]=sum(grfp1c[m,mm,i,],na.rm=TRUE)
}}}
grfp1e=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {	
grfp1e[m,mm]=sum(grfp1d[m,mm,],na.rm=TRUE)
}}

grfp1f=array(NA,c(5,5,hnum,(tnum-1),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-2)) {
if (t>1) {
for (r in 2:(t-1)) {
grfp1f[,,i,t,r]=grlcnh1[i,t,]%*%t(grlcnh1[i,r,])
}}
for (r in (t+1):(tnum-1)) {
grfp1f[,,i,t,r]=grlcnh1[i,t,]%*%t(grlcnh1[i,r,])
}}}
grfp1ff=array(NA,c(5,5,hnum,(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-2)) {
for (m in 1:5) {
for (mm in 1:5) {	
grfp1ff[m,mm,i,t]=sum(grfp1f[m,mm,i,t,],na.rm=TRUE)
}}}}
grfp1g=array(NA,c(5,5,hnum))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (m in 1:5) {
for (mm in 1:5) {	
grfp1g[m,mm,i]=sum(grfp1ff[m,mm,i,],na.rm=TRUE)
}}}
grfp1h=array(NA,c(5,5))
for (m in 1:5) {
for (mm in 1:5) {	
grfp1h[m,mm]=sum(grfp1g[m,mm,],na.rm=TRUE)
}}

Blcnh1=grfp1e+grfp1h

Alcnh1=hesslcnh1

library(MASS)	
Covlcnh1=ginv(Alcnh1)%*%Blcnh1%*%ginv(Alcnh1)

sdoptp=sqrt(diag(Covlcnh1))

xLcnh1incHme=c(optp,sdoptp,lhv,nn)

t(xLcnh1incHme)

save.image("Hom1incHme")



