setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))
load("Het1incHallme")

t(xPrsnhme)
t(xPrsme_nog)
t(xLcnh1incHme)
t(xLc1incHme_nog)

#COMPUTE INEQUALITY MEASURES FROM THE DATA

gini=numeric()
for (t in 1:tnum) {
  pdiff1=array(NA,c(hnumv,hnumv))
  for (i in (hnumv0+1):(hnumv0+hnumv)) {
    for (j in (hnumv0+1):(hnumv0+hnumv)) {
      pdiff1[i-hnumv0,j-hnumv0]=abs(consdat[i,t]-consdat[j,t])
    }}
  gini[t]=mean(pdiff1,na.rm=TRUE)/mean(consdat[,t],na.rm=TRUE)/2
}

#var of log consumption
vlogc=numeric()
for (t in 1:tnum) {
vlogc[t]=var(log(consdat[(hnumv0+1):(hnumv0+hnumv),t]))
}

#COMPUTE THE PREDICTED CONSUMPTION ALLOCATION AND THEN INEQUALITY FROM THE PERFECT RISK SHARING MODEL WITH HOMOGENOUS PREFERENCES

lML=function(gammasq) {
pcons=array(NA,c(hnum,tnum-1))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	for (t in 1:(tnum-1)) {
	xt=consdat[i,t]/vilmeanconsbij[i,t]
	pcons[i,t]=xt*vilmeanconsbij[i,t+1]
		}}
return(pcons)
}

predconspnh=lML(xPrsnhme[1])

meanpredc=numeric()
meanc2=numeric()
for (t in 1:(tnum-1)) {
	meanpredc[t]=mean(predconspnh[(hnumv0+1):(hnumv0+hnumv),t])
	meanc2[t]=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
	}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	predconspnh[i,t]=predconspnh[i,t]*meanc2[t]/meanpredc[t]
}}

pginipnh=numeric()
for (t in 1:(tnum-1)) {
pdiff1=array(NA,c(hnumv,hnumv))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (j in (hnumv0+1):(hnumv0+hnumv)) {
pdiff1[i-hnumv0,j-hnumv0]=abs(predconspnh[i,t]-predconspnh[j,t])
}}
pginipnh[t]=mean(pdiff1,na.rm=TRUE)/mean(predconspnh[,t],na.rm=TRUE)/2
}

vlogcpnh=numeric()
for (t in 1:(tnum-1)) {
vlogcpnh[t]=var(log(predconspnh[(hnumv0+1):(hnumv0+hnumv),t]))
}

#COMPUTE THE PREDICTED CONSUMPTION ALLOCATION AND THEN INEQUALITY FROM THE PERFECT RISK SHARING MODEL WITH HETEROGENOUS PREFERENCES

lML=function(yy) {
	a1=yy[1]
	a2=yy[2]
	a3=yy[3]
	a4=yy[4]
pcons=array(NA,c(hnum,tnum-1))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	sigi=1+a1*educ[i]+a2*propf[i]+a3*age[i]+a4*land1[i]
	for (t in 1:(tnum-1)) {
	xt=consdat[i,t]^(sigi)/vilmeanconsbij[i,t]
	pcons[i,t]=(xt*vilmeanconsbij[i,t+1])^(1/sigi)
		}}
return(pcons)
}

predconsp=lML(xPrsme_nog[1:4])

meanpredc=numeric()
meanc2=numeric()
for (t in 1:(tnum-1)) {
	meanpredc[t]=mean(predconsp[(hnumv0+1):(hnumv0+hnumv),t])
	meanc2[t]=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
	}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	predconsp[i,t]=predconsp[i,t]*meanc2[t]/meanpredc[t]
}}

pginip=numeric()
for (t in 1:(tnum-1)) {
pdiff1=array(NA,c(hnumv,hnumv))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (j in (hnumv0+1):(hnumv0+hnumv)) {
pdiff1[i-hnumv0,j-hnumv0]=abs(predconsp[i,t]-predconsp[j,t])
}}
pginip[t]=mean(pdiff1,na.rm=TRUE)/mean(predconsp[,t],na.rm=TRUE)/2
}


vlogcp=numeric()
for (t in 1:(tnum-1)) {
vlogcp[t]=var(log(predconsp[(hnumv0+1):(hnumv0+hnumv),t]))
}

#COMPUTE THE PREDICTED CONSUMPTION ALLOCATION AND THEN INEQUALITY FROM THE LIMITED COMMITMENT MODEL WITH HOMOGENOUS PREFERENCES

lcML=function(yy) {

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

lhs=matrix(c(10000),nrow=hnum,ncol=(tnum-1))
rhs=matrix(c(10000),nrow=hnum,ncol=(tnum-1))
mli1=array(NA,c(hnum,(tnum-1)))
mli=matrix(c(0),nrow=hnum,ncol=(tnum-1))
xt=array(NA,c(hnum,(tnum-1)))
xy=array(NA,c(hnum,(tnum-1)))
clc=array(NA,c(hnum,(tnum-1)))
clv=array(NA,c(hnum,(tnum-1)))
irl=array(NA,c(hnum,(tnum-1)))

irls0=numeric()
for (t in 1:(tnum-1)) {
irlz2=which.min((sh2-mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)]))^2)
for (m in (hnumv0+1):(hnumv0+hnumv)) {
xy[m,t]=uup(mean(consdat[(hnumv0+1):(hnumv0+hnumv),t]),sigma3)/uup(consdat[m,t],sigma3)
irlz1=which.min((sh1-incdatb[m,(t+1)])^2)
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

return(clc)
}

predconslcnh=lcML(xLcnh1incHme[1:3])

meanpredc=numeric()
meanc2=numeric()
for (t in 1:(tnum-1)) {
	meanpredc[t]=mean(predconslcnh[(hnumv0+1):(hnumv0+hnumv),t])
	meanc2[t]=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
	}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	predconslcnh[i,t]=predconslcnh[i,t]*meanc2[t]/meanpredc[t]
}}

pginilcnh=numeric()
for (t in 1:(tnum-1)) {
pdiff1=array(NA,c(hnumv,hnumv))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (j in (hnumv0+1):(hnumv0+hnumv)) {
pdiff1[i-hnumv0,j-hnumv0]=abs(predconslcnh[i,t]-predconslcnh[j,t])
}}
pginilcnh[t]=mean(pdiff1,na.rm=TRUE)/mean(predconslcnh[,t],na.rm=TRUE)/2
}

vlogclcnh=numeric()
for (t in 1:(tnum-1)) {
vlogclcnh[t]=var(log(predconslcnh[(hnumv0+1):(hnumv0+hnumv),t]))
}

#COMPUTE THE PREDICTED CONSUMPTION ALLOCATION AND THEN INEQUALITY FROM THE LIMITED COMMITMENT MODEL WITH HETEROGENOUS PREFERENCES

lcML=function(yy) {

delta3=yy[1]
sigmav3=yy[2]
a1=yy[2]*yy[3]
a2=yy[2]*yy[4]
a3=yy[2]*yy[5]
a4=yy[2]*yy[6]
phi3=yy[7]

xint=array(NA,c(hnum,S,2))
maxsig=min(sigmav3+3,max(sig2))
minsig=max(0.1,sigmav3-3)

sigi=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
	sigi[m]=min(max(sigmav3+a1*educ[m]+a2*propf[m]+a3*age[m]+a4*land1[m],minsig),maxsig) #sigi
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
mean(sigma3,na.rm=TRUE)

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

lhs=matrix(c(10000),nrow=hnum,ncol=(tnum-1))
mli=matrix(c(0),nrow=hnum,ncol=(tnum-1))
xt=array(NA,c(hnum,(tnum-1)))
xy=array(NA,c(hnum,(tnum-1)))
clc=array(NA,c(hnum,(tnum-1)))
clv=array(NA,c(hnum,(tnum-1)))
irl=array(NA,c(hnum,(tnum-1)))

for (t in 1:(tnum-1)) {
for (m in (hnumv0+1):(hnumv0+hnumv)) {

xy[m,t]=uup(mean(consdat[(hnumv0+1):(hnumv0+hnumv),t]),sigmav3)/uup(consdat[m,t],sigma3[m])
	
irlz=numeric()
irlz[1]=which.min((sh1-incdatb[m,(t+1)])^2)
irlz[2]=which.min((sh2-mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)]))^2)

irl[m,t]=Sh2*(irlz[1]-1)+irlz[2]

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

return(clc)
}

predconslc=lcML(xLc1incHme_nog[1:7])

meanpredc=numeric()
meanc2=numeric()
for (t in 1:(tnum-1)) {
	meanpredc[t]=mean(predconslc[(hnumv0+1):(hnumv0+hnumv),t])
	meanc2[t]=mean(incdatb[(hnumv0+1):(hnumv0+hnumv),(t+1)])
	}
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
	predconslc[i,t]=predconslc[i,t]*meanc2[t]/meanpredc[t]
}}

pginilc=numeric()
for (t in 1:(tnum-1)) {
pdiff1=array(NA,c(hnumv,hnumv))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (j in (hnumv0+1):(hnumv0+hnumv)) {
pdiff1[i-hnumv0,j-hnumv0]=abs(predconslc[i,t]-predconslc[j,t])
}}
pginilc[t]=mean(pdiff1,na.rm=TRUE)/mean(predconslc[,t],na.rm=TRUE)/2
}

vlogclc=numeric()
for (t in 1:(tnum-1)) {
vlogclc[t]=var(log(predconslc[(hnumv0+1):(hnumv0+hnumv),t]))
}

#does log income explain the difference between predicted and true log consumption?

Dptconspnh=numeric()
Dptconsp=numeric()
Dptconslcnh=numeric()
Dptconslc=numeric()
incdat2=numeric()
hcode2=numeric()
tcode2=numeric()
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	for (t in 1:(tnum-1)) {
	Dptconspnh[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1])-log(predconspnh[i,t])
	Dptconsp[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1])-log(predconsp[i,t])
	Dptconslcnh[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1])-log(predconslcnh[i,t])
	Dptconslc[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1])-log(predconslc[i,t])
	consdat2[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1])
	incdat2[(tnum-1)*(i-hnumv0-1)+t]=log(incdatb[i,t+1])
	hcode2[(tnum-1)*(i-hnumv0-1)+t]=i
	tcode2[(tnum-1)*(i-hnumv0-1)+t]=t
	}}
dDptconspnh=numeric()
dDptconsp=numeric()
dDptconslcnh=numeric()
dDptconslc=numeric()
dincdat2=numeric()
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	for (t in 1:(tnum-1)) {
	dDptconspnh[(tnum-1)*(i-hnumv0-1)+t]=Dcons[i,t]-Dpconspnh[i,t]
	dDptconsp[(tnum-1)*(i-hnumv0-1)+t]=Dcons[i,t]-Dpconsp[i,t]
	dDptconslcnh[(tnum-1)*(i-hnumv0-1)+t]=Dcons[i,t]-Dpconslcnh[i,t]
	dDptconslc[(tnum-1)*(i-hnumv0-1)+t]=Dcons[i,t]-Dpconslc[i,t]
	dincdat2[(tnum-1)*(i-hnumv0-1)+t]=Dinc[i,t]
	}}

library(dummies)
summary(lm(dDptconspnh~dincdat2+tdummies))
summary(lm(dDptconsp~dincdat2+tdummies))
summary(lm(dDptconslcnh~dincdat2+tdummies))
summary(lm(dDptconslc~dincdat2+tdummies))

XX=array(NA,c((hnumv*(tnum-1)),13))
XX[,1]=hcode2
XX[,2]=tcode2
XX[,3]=consdat2
XX[,4]=incdat2
XX[,5]=Dptconspnh
XX[,6]=Dptconsp
XX[,7]=Dptconslcnh
XX[,8]=Dptconslc
XX[,9]=dDptconspnh
XX[,10]=dDptconsp
XX[,11]=dDptconslcnh
XX[,12]=dDptconslc
XX[,13]=dincdat2
XX=as.data.frame(XX)
library(foreign)
write.dta(XX,file="vil1dat.dta")
#COMPUTE ROBUST STANDRAD ERRORS IN STATA

#do the models predict log consumption better than household fixed effects and time dummies?

consdat2=numeric()
incdat2=numeric()
predconspnh2=numeric()
predconsp2=numeric()
predconslcnh2=numeric()
predconslc2=numeric()
Dconsdat2=numeric()
Dincdat2=numeric()
Dpredconspnh2=numeric()
Dpredconsp2=numeric()
Dpredconslcnh2=numeric()
Dpredconslc2=numeric()
hcode2=numeric()
tcode2=numeric()
for (i in (hnumv0+1):(hnumv0+hnumv)) {
	for (t in 1:(tnum-1)) {
	consdat2[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1])
	incdat2[(tnum-1)*(i-hnumv0-1)+t]=log(incdatb[i,t+1])
	predconspnh2[(tnum-1)*(i-hnumv0-1)+t]=log(predconspnh[i,t])
	predconsp2[(tnum-1)*(i-hnumv0-1)+t]=log(predconsp[i,t])
	predconslcnh2[(tnum-1)*(i-hnumv0-1)+t]=log(predconslcnh[i,t])
	predconslc2[(tnum-1)*(i-hnumv0-1)+t]=log(predconslc[i,t])
	Dconsdat2[(tnum-1)*(i-hnumv0-1)+t]=log(consdat[i,t+1]/consdat[i,t])
	Dincdat2[(tnum-1)*(i-hnumv0-1)+t]=log(incdatb[i,t+1]/incdatb[i,t])
	Dpredconspnh2[(tnum-1)*(i-hnumv0-1)+t]=log(predconspnh[i,t]/consdat[i,t])
	Dpredconsp2[(tnum-1)*(i-hnumv0-1)+t]=log(predconsp[i,t]/consdat[i,t])
	Dpredconslcnh2[(tnum-1)*(i-hnumv0-1)+t]=log(predconslcnh[i,t]/consdat[i,t])
	Dpredconslc2[(tnum-1)*(i-hnumv0-1)+t]=log(predconslc[i,t]/consdat[i,t])
	hcode2[(tnum-1)*(i-hnumv0-1)+t]=i
	tcode2[(tnum-1)*(i-hnumv0-1)+t]=t
	}}

library(dummies)
tdummies=as.array(dummy(tcode2))
hdummies=as.array(dummy(hcode2))

#fe
reg=lm(consdat2~tdummies+hdummies)
residt=residuals(reg)
reginc=lm(incdat2~tdummies+hdummies)
residinc=residuals(reginc)
regpnh=lm(predconspnh2~tdummies+hdummies)
residpnh=residuals(regpnh)
regp=lm(predconsp2~tdummies+hdummies)
residp=residuals(regp)
reglcnh=lm(predconslcnh2~tdummies+hdummies)
residlcnh=residuals(reglcnh)
reglc=lm(predconslc2~tdummies+hdummies)
residlc=residuals(reglc)

summary(lm(residt~residinc-1))
summary(lm(residt~residpnh-1))
summary(lm(residt~residp-1))
summary(lm(residt~residlcnh-1))
summary(lm(residt~residlc-1))

#SAVE INEQUALITY MEASURES WITH UNIQUE NAME FOR VILLAGE 1

vlogc1=vlogc
vlogcpnh1=vlogcpnh
vlogcp1=vlogcp
vlogclcnh1=vlogclcnh
vlogclc1=vlogclc
gini1=gini
pginipnh1=pginipnh
pginip1=pginip
pginilcnh1=pginilcnh
pginilc1=pginilc

save.image("Preds1")

