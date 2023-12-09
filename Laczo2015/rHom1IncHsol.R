# setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))
load("Laczo2015/allIncH1")

d=numeric()
d[1]=0.8
for (i in 2:10) {
d[i]=d[i-1]+0.02
}

sig2=numeric()
sig2[1]=0.4
for (i in 2:13) {
sig2[i]=sig2[i-1]+0.3
}

pphi=numeric()
pphi[1]=0
for (i in 2:12) {
pphi[i]=pphi[i-1]+0.04
}

crit=0.000001
h=300 #max iter
g=199 #number of gridpoints on x - 1

save.image("Hom1incH")

##low mean low cv
			
codemcv=c(1,1)
sh1=sh1r[vilcode,1,1,]
P1=P1r[vilcode,1,1,,]
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

xxint_all=array(NA,c(10,13,12,S,2))

ptm <- proc.time()

for (ii3 in 1:12) {
for (ii1 in 1:10) {
for (ii2 in 1:13) {
	
delta=d[ii1]
sigma=sig2[ii2]
pcphi=pphi[ii3]

#crit is the stopping criterion on the policy function x
#(sum of squared differences with respect to the previous iteration of the x limits)
#h is the max number of iterations +1
#g is number of gridpoints on x -1 (g must be even)

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
xin=matrix(nrow=S,ncol=2)
xin[,1]=minx
xin[,2]=maxx
V1ll=V1l[,,(i-1)]
V2ll=V2l[,,(i-1)]

rm(V1l,V2l,xxint,cl,V1a1,V2a1,V1a0,V2a0,V10,V20,xx,V1ll,V2ll,x)

xxint_all[ii1,ii2,ii3,,]=xin

}
save.image("Hom1incH")
}}

proc.time() - ptm

xxint_all11=xxint_all

save.image("Hom1incH")


##low mean high cv
			
codemcv=c(1,2)
sh1=sh1r[vilcode,1,2,]
P1=P1r[vilcode,1,2,,]
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

xxint_all=array(NA,c(10,13,12,S,2))

ptm <- proc.time()

for (ii3 in 1:12) {
for (ii1 in 1:10) {
for (ii2 in 1:13) {
	
delta=d[ii1]
sigma=sig2[ii2]
pcphi=pphi[ii3]

#crit is the stopping criterion on the policy function x
#(sum of squared differences with respect to the previous iteration of the x limits)
#h is the max number of iterations +1
#g is number of gridpoints on x -1 (g must be even)

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
xin=matrix(nrow=S,ncol=2)
xin[,1]=minx
xin[,2]=maxx
V1ll=V1l[,,(i-1)]
V2ll=V2l[,,(i-1)]

rm(V1l,V2l,xxint,cl,V1a1,V2a1,V1a0,V2a0,V10,V20,xx,V1ll,V2ll,x)

xxint_all[ii1,ii2,ii3,,]=xin

}
save.image("Hom1incH")
}}

proc.time() - ptm

xxint_all12=xxint_all

save.image("Hom1incH")

##high mean low cv
			
codemcv=c(2,1)
sh1=sh1r[vilcode,2,1,]
P1=P1r[vilcode,2,1,,]
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

xxint_all=array(NA,c(10,13,12,S,2))

ptm <- proc.time()

for (ii3 in 1:12) {
for (ii1 in 1:10) {
for (ii2 in 1:13) {
	
delta=d[ii1]
sigma=sig2[ii2]
pcphi=pphi[ii3]

#crit is the stopping criterion on the policy function x
#(sum of squared differences with respect to the previous iteration of the x limits)
#h is the max number of iterations +1
#g is number of gridpoints on x -1 (g must be even)

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
xin=matrix(nrow=S,ncol=2)
xin[,1]=minx
xin[,2]=maxx
V1ll=V1l[,,(i-1)]
V2ll=V2l[,,(i-1)]

rm(V1l,V2l,xxint,cl,V1a1,V2a1,V1a0,V2a0,V10,V20,xx,V1ll,V2ll,x)

xxint_all[ii1,ii2,ii3,,]=xin

}
save.image("Hom1incH")
}}

proc.time() - ptm

xxint_all21=xxint_all

save.image("Hom1incH")

##high mean high cv
			
codemcv=c(2,2)
sh1=sh1r[vilcode,2,2,]
P1=P1r[vilcode,2,2,,]
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

xxint_all=array(NA,c(10,13,12,S,2))

ptm <- proc.time()

for (ii3 in 1:12) {
for (ii1 in 1:10) {
for (ii2 in 1:13) {
      
delta=d[ii1]
sigma=sig2[ii2]
pcphi=pphi[ii3]

#crit is the stopping criterion on the policy function x
#(sum of squared differences with respect to the previous iteration of the x limits)
#h is the max number of iterations +1
#g is number of gridpoints on x -1 (g must be even)

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
xin=matrix(nrow=S,ncol=2)
xin[,1]=minx
xin[,2]=maxx
V1ll=V1l[,,(i-1)]
V2ll=V2l[,,(i-1)]

rm(V1l,V2l,xxint,cl,V1a1,V2a1,V1a0,V2a0,V10,V20,xx,V1ll,V2ll,x)

xxint_all[ii1,ii2,ii3,,]=xin

}
save.image("Hom1incH")
}}

proc.time() - ptm

xxint_all22=xxint_all

xxint_all=array(NA,c(2,2,10,13,12,S,2))
xxint_all[1,1,,,,,]=xxint_all11
xxint_all[1,2,,,,,]=xxint_all12
xxint_all[2,1,,,,,]=xxint_all21
xxint_all[2,2,,,,,]=xxint_all22
xxint_alla=xxint_all

save.image("Hom1incH")



