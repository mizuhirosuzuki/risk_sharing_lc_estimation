setwd("/Users/slaczo/Dropbox/IndiaCodesNew/JEEA4411codes") #change appropriately
rm(list = ls(all = TRUE))
load("Het1incH")

fg=function(xx) {
	meancode=xx[1]
	cvcode=xx[2]
	del=xx[3]
	si=xx[4]
	siv=xx[5]
	ph=xx[6]
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
	l3=which.min(abs(siv-sig2))
	if (sig2[l3]<=siv & l3<length(sig2)) {
		l3=l3+1
		}
	intp3=(siv-sig2[l3-1])/(sig2[l3]-sig2[l3-1])
	l4=which.min(abs(ph-pphi))
	if (pphi[l4]<=ph & l4<length(pphi)) {
		l4=l4+1
		}
	intp4=(ph-pphi[l4-1])/(pphi[l4]-pphi[l4-1])
	
	xintc=array(0,c(S,2))
	for (ii4 in 1:S) {
	for (ii5 in 1:2) {
	xintaaa=intp1*(xxint_all[meancode,cvcode,l1,l2-1,l3-1,l4-1,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2-1,l3-1,l4-1,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2-1,l3-1,l4-1,ii4,ii5]
	xintbaa=intp1*(xxint_all[meancode,cvcode,l1,l2,l3-1,l4-1,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2,l3-1,l4-1,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2,l3-1,l4-1,ii4,ii5]
	xintaba=intp1*(xxint_all[meancode,cvcode,l1,l2-1,l3,l4-1,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2-1,l3,l4-1,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2-1,l3,l4-1,ii4,ii5]
	xintbba=intp1*(xxint_all[meancode,cvcode,l1,l2,l3,l4-1,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2,l3,l4-1,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2,l3,l4-1,ii4,ii5]
	xintaa=intp2*(xintbaa-xintaaa)+xintaaa
	xintba=intp2*(xintbba-xintaba)+xintaba
	xintca=intp3*(xintba-xintaa)+xintaa
	xintaab=intp1*(xxint_all[meancode,cvcode,l1,l2-1,l3-1,l4,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2-1,l3-1,l4,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2-1,l3-1,l4,ii4,ii5]
	xintbab=intp1*(xxint_all[meancode,cvcode,l1,l2,l3-1,l4,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2,l3-1,l4,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2,l3-1,l4,ii4,ii5]
	xintabb=intp1*(xxint_all[meancode,cvcode,l1,l2-1,l3,l4,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2-1,l3,l4,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2-1,l3,l4,ii4,ii5]
	xintbbb=intp1*(xxint_all[meancode,cvcode,l1,l2,l3,l4,ii4,ii5]-xxint_all[meancode,cvcode,l1-1,l2,l3,l4,ii4,ii5])+xxint_all[meancode,cvcode,l1-1,l2,l3,l4,ii4,ii5]
	xintab=intp2*(xintbab-xintaab)+xintaab
	xintbb=intp2*(xintbbb-xintabb)+xintabb
	xintcb=intp3*(xintbb-xintab)+xintab
	xintc[ii4,ii5]=intp4*(xintcb-xintca)+xintca
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
sigmav3=yy[2]
a1=yy[2]*yy[3]
a2=yy[2]*yy[4]
a3=yy[2]*yy[5]
a4=yy[2]*yy[6]
phi3=yy[7]
gamsqc=yy[8]
gamsqy=yy[9]

mli1=array(0,c(hnum,tnum-1,NS))
mli=matrix(c(0),nrow=hnum,ncol=(tnum-1))
maxsig=min(sigmav3+3,max(sig2))
minsig=max(0.1,sigmav3-3)
for (sim in 1:NS) {
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

xint=array(NA,c(hnum,S,2))
for (m in (hnumv0+1):(hnumv0+hnumv)) {
yy3=c(hlmean[m],hlcv[m],delta3,sigma3[m],sigmav3,phi3)
xss=fg(yy3)
xint[m,,1]=xss[1:S]
xint[m,,2]=xss[(S+1):(2*S)]
}

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

for (t in 1:(tnum-1)) {
for (m in (hnumv0+1):(hnumv0+hnumv)) {
xy[m,t]=uup(mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t]),sigmav3)/uup(consdatstar[m,t],sigma3[m])
irlz=numeric()
irlz[1]=which.min((sh1-incdatstar[m,t])^2)
irlz[2]=which.min((sh2-mean(incdatstar[(hnumv0+1):(hnumv0+hnumv),t]))^2)
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
#subtract the consumption of constraned households
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

seg1=numeric()
for (m in (hnumv0+1):(hnumv0+hnumv)) {
seg1[m]=((hnumv-1)/hnumv+(sigma3[m]-sigmav3)/sigmav3)^2+(hnumv-1)/hnumv^2
for (t in 1:(tnum-1)) {
lhs=(sigma3[m]/sigmav3)*log(consdat[m,(t+1)])-log(mean(consdat[(hnumv0+1):(hnumv0+hnumv),(t+1)]))
rhs=(sigma3[m]/sigmav3)*log(clc[m,t])-log(clv[m,t])
mli1[m,t,sim]=dnorm(lhs,mean=rhs,sd=sqrt(seg1[m]*gamsqc))
}}}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
mli[m,t]=log(mean(mli1[m,t,]))
}}
logML=sum(mli,na.rm=TRUE)
rm(mli1,mli,xt,xy,clc,clv,irl)
return(-logML)
}

#do grid search
#0.937354,1.36926,0.0403439,1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657
#-14.216
aa1=numeric()
aa1[1]=0.82
for (i in 2:31) {
  aa1[i]=aa1[i-1]+(0.98-aa1[1])/30
}	
lcMLgrid2=numeric()	
for (i1 in 1:31) {
  yy1=c(aa1[i1],1.36926,0.0403439,1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
ind1=which.min(lcMLgrid2)
aa1[ind1]
aa1=numeric()
aa1[1]=0.934
for (i in 2:11) {
  aa1[i]=aa1[i-1]+(0.944-aa1[1])/10
}	
lcMLgrid2=numeric()	
for (i1 in 1:11) {
  yy1=c(aa1[i1],1.36926,0.0403439,1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind1=which.min(lcMLgrid2)
ind1
aa1[ind1]

aa2=numeric()
aa2[1]=1
for (i in 2:31) {
  aa2[i]=aa2[i-1]+(3-aa2[1])/30
}	
lcMLgrid2=numeric()	
for (i1 in 1:31) {
  yy1=c(aa1[ind1],aa2[i1],0.0403439,1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
lcMLgrid22=lcMLgrid2
ind2=which.min(lcMLgrid2)
aa2[ind2]
minp2=aa2[ind2]
aa2=numeric()
aa2[1]=minp2-0.15
for (i in 2:16) {
  aa2[i]=aa2[i-1]+(minp2+0.15-aa2[1])/15
}	
lcMLgrid2=numeric()	
for (i1 in 1:16) {
  yy1=c(aa1[ind1],aa2[i1],0.0403439,1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind2=which.min(lcMLgrid2)
ind2
aa2[ind2]

aa3=numeric()
aa3[1]=0
for (i in 2:23) {
  aa3[i]=aa3[i-1]+(0.44-aa3[1])/22
}	
lcMLgrid2=numeric()	
for (i1 in 1:23) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,1.36726,-0.0366454,-0.178889,aa3[i1],0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind3=which.min(lcMLgrid2)
ind3
aa3[ind3]
minp3=aa3[ind3]
aa3=numeric()
aa3[1]=minp3-0.03
for (i in 2:13) {
  aa3[i]=aa3[i-1]+(min(max(pphi),minp3+0.03)-aa3[1])/12
}	
lcMLgrid2=numeric()	
for (i1 in 1:13) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,1.36726,-0.0366454,-0.178889,aa3[i1],0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind3=which.min(lcMLgrid2)
ind3
aa3[ind3]

aa4=numeric()
aa4[1]=-0.1
for (i in 2:21) {
  aa4[i]=aa4[i-1]+(0.2-aa4[1])/20
}
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],aa4[i1],1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2,na.rm=TRUE)
ind4=which.min(lcMLgrid2)
ind4
aa4[ind4]
minp4=aa4[ind4]
aa4=numeric()
aa4[1]=minp4-0.02
for (i in 2:21) {
  aa4[i]=aa4[i-1]+(minp4+0.02-aa4[1])/20
}
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],aa4[i1],1.36726,-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2,na.rm=TRUE)
ind4=which.min(lcMLgrid2)
ind4
aa4[ind4]

aa5=numeric()
aa5[1]=-1
for (i in 2:21) {
  aa5[i]=aa5[i-1]+(2.5-aa5[1])/20
}
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,aa5[i1],-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind5=which.min(lcMLgrid2)
ind5
aa5[ind5]
minp5=aa5[ind5]
aa5=numeric()
aa5[1]=minp5-0.25
for (i in 2:21) {
  aa5[i]=aa5[i-1]+(minp5+0.25-aa5[1])/20
}
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,aa5[i1],-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2,na.rm=TRUE)
ind5=which.min(lcMLgrid2)
ind5
aa5[ind5]

aa6=numeric()
aa6[1]=-0.07
for (i in 2:21) {
  aa6[i]=aa6[i-1]+(0.03-aa6[1])/20
}	
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,aa5[ind5],aa6[i1],-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind6=which.min(lcMLgrid2)
ind6
aa6[ind6]
minp6=aa6[ind6]
aa6=numeric()
aa6[1]=minp6-0.01
for (i in 2:21) {
  aa6[i]=aa6[i-1]+(minp6+0.01-aa6[1])/20
}	
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,aa5[ind5],aa6[i1],-0.178889,0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind6=which.min(lcMLgrid2)
ind6
aa6[ind6]

aa7=numeric()
aa7[1]=-0.3
for (i in 2:21) {
  aa7[i]=aa7[i-1]+(0.1-aa7[1])/20
}	
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,aa5[ind5],-0.0366454,aa7[i1],0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind7=which.min(lcMLgrid2)
ind7
aa7[ind7]
minp7=aa7[ind7]
aa7=numeric()
aa7[1]=minp7-0.02
for (i in 2:21) {
  aa7[i]=aa7[i-1]+(minp7+0.02-aa7[1])/20
}	
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind1],aa2[ind2],0.0403439,aa5[ind5],-0.0366454,aa7[i1],0.370969,0.0433436,0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2,na.rm=TRUE)
ind7=which.min(lcMLgrid2)
ind7
aa7[ind7]

lcMLgrid1=array(NA,c(51,51,51))  
for (i1 in (ind1-2):(ind1+2)) {
  for (i2 in (ind2-2):(ind2+2)) {
    for (i3 in (ind3-2):(ind3+2)) {
      yy1=c(aa1[i1],aa2[i2],0.0403439,aa5[ind5],-0.0366454,-0.178889,aa3[i3],0.0433436,0.0579657)
      lcMLgrid1[i1,i2,i3]=lcML(yy1)
    }}
}

  for (i2 in 5:10) {
      yy1=c(aa1[4],aa2[i2],0.0403439,aa5[ind5],-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
      lcMLgrid1[4,i2,5]=lcML(yy1)
      }

min(lcMLgrid1,na.rm=TRUE)
ind11=as.numeric(which(lcMLgrid1==min(lcMLgrid1,na.rm=TRUE),arr.ind=TRUE))
ind11

c(aa1[ind11[1]],aa2[ind11[2]],0.0403439,aa5[ind5],-0.0366454,-0.178889,0.370969,0.0433436,0.0579657)
#0.9370000,1.0833333,0.0403439,1.3750000,-0.0366454,-0.1788890,0.3709690,0.0433436,0.0579657
#-14.40740

aa8=numeric()
aa8[1]=0.035
for (i in 2:21) {
  aa8[i]=aa8[i-1]+(0.055-aa8[1])/20
}
lcMLgrid2=numeric()	
for (i1 in 1:21) {
  yy1=c(aa1[ind11[1]],aa2[ind11[2]],0.0403439,aa5[ind5],-0.0366454,-0.178889,0.370969,aa8[i1],0.0579657)
  lcMLgrid2[i1]=lcML(yy1)
}
min(lcMLgrid2)
ind8=which.min(lcMLgrid2)
ind8
aa8[ind8]

#use best parameter vector found by grid search as initial guess
startpar=c(0.9370000,1.0833333,0.0403439,1.3750000,-0.0366454,-0.1788890,0.3709690,0.0433436,0.0579657)
minpar=c(0.930,1.000,0.030,1.200,-0.047,-0.200,0.350,0.040,0.050)
maxpar=c(0.945,1.200,0.050,1.600,-0.026,-0.150,0.390,0.047,0.070)
lcML(startpar)
lcML(minpar)
lcML(maxpar)

save.image("Het1incHme_nog")

ptm <- proc.time()
sol=optim(startpar,lcML,method="L-BFGS-B",lower=minpar,upper=maxpar,control=list(trace=5,maxit=50),hessian=TRUE)
optp=sol$par
lhv=-sol$value
hesslc1=sol$hessian

countslc=sol$counts
messagelc=sol$message

proc.time() - ptm

save.image("Het1incHme_nog")

mlilc1=array(NA,c(hnumv0+hnumv,(tnum-1)))
grlc1=array(NA,c(hnumv0+hnumv,(tnum-1),9))

for (mmh in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
lmli=function(yy) {
	
delta3=yy[1]
sigmav3=yy[2]
a1=yy[2]*yy[3]
a2=yy[2]*yy[4]
a3=yy[2]*yy[5]
a4=yy[2]*yy[6]
phi3=yy[7]
gamsqc=yy[8]
gamsqy=yy[9]

mli1=numeric()
maxsig=min(sigmav3+3,max(sig2))
minsig=max(0.1,sigmav3-3)
for (sim in 1:4) {

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

xint=array(NA,c(hnum,S,2))
for (m in (hnumv0+1):(hnumv0+hnumv)) {
yy3=c(hlmean[m],hlcv[m],delta3,sigma3[m],sigmav3,phi3)
xss=fg(yy3)
xint[m,,1]=xss[1:S]
xint[m,,2]=xss[(S+1):(2*S)]
}

xt=numeric()
xy=numeric()
clc=numeric()
clv=numeric()
irl=numeric()

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

for (m in (hnumv0+1):(hnumv0+hnumv)) {
xy[m]=uup(mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t]),sigmav3)/uup(consdatstar[m,t],sigma3[m])
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
if (xy[m]>=xint[m,irl[m],1]) {
	xt[m]=xy[m]
	xtiter[m]=1
	} else {
	count=count+1
	xt[m]=xint[m,irl[m],1]
	xtiter[m]=0
	}
	f=function(w) uup(((s0[irl[m]]-w)/(hnumv-1)),sigmav3)/uup(w,sigma3[m])-xt[m]
	clc[m]=uniroot(f,c(0.000001,(s0[irl[m]]-0.000001)),tol=crit,maxiter=100)$root
	clv[m]=(s0[irl[m]]-clc[m])/(hnumv-1)
}}
s02a=s02a+(xtiter[(hnumv0+1):(hnumv0+hnumv)]-1)%*%clc[(hnumv0+1):(hnumv0+hnumv)]
for (m in (hnumv0+1):(hnumv0+hnumv)) {
if (xtiter[m]==1) {
	f=function(w) uup(((s02a[m]-w)/(sum(xtiter[(hnumv0+1):(hnumv0+hnumv)])-1)),sigmav3)/uup(w,sigma3[m])-xt[m]
	if ((s02a[m]>0) && (f(0.000001)*f(s02a[m]-0.000001)<0)) {
	clc[m]=uniroot(f,c(0.000001,(s02a[m]-0.000001)),tol=crit,maxiter=100)$root
	} else {
	clc[m]=max(-1/s02a[m],s02a[m])
	}
	clv[m]=(s0[irl[m]]-clc[m])/(hnumv-1)
	xy[m]=uup(clv[m],sigmav3)/uup(clc[m],sigma3[m])
	}}
itercount=itercount+1
}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
clv[m]=s0[irl[m]]/hnumv
}

seg1=((hnumv-1)/hnumv+(sigma3[mmh]-sigmav3)/sigmav3)^2+(hnumv-1)/hnumv^2
lhs=(sigma3[mmh]/sigmav3)*log(consdat[mmh,(t+1)])-log(mean(consdat[(hnumv0+1):(hnumv0+hnumv),(t+1)]))
rhs=(sigma3[mmh]/sigmav3)*log(clc[mmh])-log(clv[mmh])
mli1[sim]=dnorm(lhs,mean=rhs,sd=sqrt(seg1*gamsqc))
}
for (sim in 6:NS) {

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

xint=array(NA,c(hnum,S,2))
for (m in (hnumv0+1):(hnumv0+hnumv)) {
yy3=c(hlmean[m],hlcv[m],delta3,sigma3[m],sigmav3,phi3)
xss=fg(yy3)
xint[m,,1]=xss[1:S]
xint[m,,2]=xss[(S+1):(2*S)]
}

xt=numeric()
xy=numeric()
clc=numeric()
clv=numeric()
irl=numeric()

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

for (m in (hnumv0+1):(hnumv0+hnumv)) {
xy[m]=uup(mean(consdatstar[(hnumv0+1):(hnumv0+hnumv),t]),sigmav3)/uup(consdatstar[m,t],sigma3[m])
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
if (xy[m]>=xint[m,irl[m],1]) {
	xt[m]=xy[m]
	xtiter[m]=1
	} else {
	count=count+1
	xt[m]=xint[m,irl[m],1]
	xtiter[m]=0
	}
	f=function(w) uup(((s0[irl[m]]-w)/(hnumv-1)),sigmav3)/uup(w,sigma3[m])-xt[m]
	clc[m]=uniroot(f,c(0.000001,(s0[irl[m]]-0.000001)),tol=crit,maxiter=100)$root
	clv[m]=(s0[irl[m]]-clc[m])/(hnumv-1)
}}
s02a=s02a+(xtiter[(hnumv0+1):(hnumv0+hnumv)]-1)%*%clc[(hnumv0+1):(hnumv0+hnumv)]
for (m in (hnumv0+1):(hnumv0+hnumv)) {
if (xtiter[m]==1) {
	f=function(w) uup(((s02a[m]-w)/(sum(xtiter[(hnumv0+1):(hnumv0+hnumv)])-1)),sigmav3)/uup(w,sigma3[m])-xt[m]
	if ((s02a[m]>0) && (f(0.000001)*f(s02a[m]-0.000001)<0)) {
	clc[m]=uniroot(f,c(0.000001,(s02a[m]-0.000001)),tol=crit,maxiter=100)$root
	} else {
	clc[m]=max(-1/s02a[m],s02a[m])
	}
	clv[m]=(s0[irl[m]]-clc[m])/(hnumv-1)
	xy[m]=uup(clv[m],sigmav3)/uup(clc[m],sigma3[m])
	}}
itercount=itercount+1
}
for (m in (hnumv0+1):(hnumv0+hnumv)) {
clv[m]=s0[irl[m]]/hnumv
}

seg1=((hnumv-1)/hnumv+(sigma3[mmh]-sigmav3)/sigmav3)^2+(hnumv-1)/hnumv^2
lhs=(sigma3[mmh]/sigmav3)*log(consdat[mmh,(t+1)])-log(mean(consdat[(hnumv0+1):(hnumv0+hnumv),(t+1)]))
rhs=(sigma3[mmh]/sigmav3)*log(clc[mmh])-log(clv[mmh])
mli1[sim-1]=dnorm(lhs,mean=rhs,sd=sqrt(seg1*gamsqc))
}
mli=log(mean(mli1,na.rm=TRUE))
rm(xt,xy,clc,clv,irl)
return(mli)
}

mlilc1[mmh,t]=lmli(optp)
gg=numericDeriv(quote(lmli(optp)),"optp")
grlc1[mmh,t,]=attr(gg,"gradient")
}}

grfp1c=array(NA,c(9,9,(hnum),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-1)) {
grfp1c[,,i,t]=grlc1[i,t,]%*%t(grlc1[i,t,])
}}
grfp1d=array(NA,c(9,9,(hnum)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (m in 1:9) {
for (mm in 1:9) {	
grfp1d[m,mm,i]=sum(grfp1c[m,mm,i,],na.rm=TRUE)
}}}
grfp1e=array(NA,c(9,9))
for (m in 1:9) {
for (mm in 1:9) {	
grfp1e[m,mm]=sum(grfp1d[m,mm,],na.rm=TRUE)
}}

grfp1f=array(NA,c(9,9,hnum,(tnum-1),(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-2)) {
if (t>1) {
for (r in 2:(t-1)) {
grfp1f[,,i,t,r]=grlc1[i,t,]%*%t(grlc1[i,r,])
}}
for (r in (t+1):(tnum-1)) {
grfp1f[,,i,t,r]=grlc1[i,t,]%*%t(grlc1[i,r,])
}}}
grfp1ff=array(NA,c(9,9,hnum,(tnum-1)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (t in 1:(tnum-2)) {
for (m in 1:9) {
for (mm in 1:9) {	
grfp1ff[m,mm,i,t]=sum(grfp1f[m,mm,i,t,],na.rm=TRUE)
}}}}
grfp1g=array(NA,c(9,9,(hnum)))
for (i in (hnumv0+1):(hnumv0+hnumv)) {
for (m in 1:9) {
for (mm in 1:9) {	
grfp1g[m,mm,i]=sum(grfp1ff[m,mm,i,],na.rm=TRUE)
}}}
grfp1h=array(NA,c(9,9))
for (m in 1:9) {
for (mm in 1:9) {	
grfp1h[m,mm]=sum(grfp1g[m,mm,],na.rm=TRUE)
}}

Blc1=grfp1e+grfp1h

Alc1=hesslc1

library(MASS)	
Covlc1=ginv(Alc1)%*%Blc1%*%ginv(Alc1)

xLc1incHme_nog=c(optp,sdoptp,lhv,nn)

t(xLc1incHme_nog)

save.image("Het1incHme_nog")

