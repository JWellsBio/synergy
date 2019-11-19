bivarN <-
function(job=1,mINF=-5,N=60,dr="c")
{
dump("bivarN",paste(dr,":\\Projects\\ToddMiller\\Synergy\\bivarN.r",sep=""))
#install.packages("mvtnorm")
library(mvtnorm)

bmn2=function(x,y,ab11,ab12,ab21,ab22,ro)
{
	covm=matrix(c(1,ro,ro,1),2,2)
	ar1=ab11+ab12*x;ar2=ab21+ab22*y
	pnorm(ar1)+pnorm(ar2)-pmvnorm(upper=c(ar1,ar2),sigma=covm)	
}
bmn2.rc=function(x,y,ab11,ab12,ab21,ab22,ro)
{
	covm=matrix(c(1,ro,ro,1),2,2)
	ar1=ab11+ab12*x;ar2=ab21+ab22*y
	
	n=length(ar1)
	pred=rep(0,n)
	for(i in 1:n)
		pred[i]=pnorm(ar1[i])+pnorm(ar2[i])-pmvnorm(upper=c(ar1[i],ar2[i]),sigma=covm)
	return(pred)
}


if(job==1) # 1:5 mixture Finney data
{
	#Rotenon (R)
	dR1=c(.1,.15,.2,.25,.35);KR1=c(.24,.44,.63,.81,.9);LdR1=log(dR1)
	dR2=c(.1,.15,.2,.25,.35);KR2=c(.28,.51,.72,.82,.89);LdR2=log(dR2)
	xR=c(LdR1,LdR2);yR=c(KR1,KR2)
	#o=glm(yR~xR,family=binomial(probit));aR=as.vector(coef(o))
	#plot(aR[1]+aR[2]*xR,log(y/(1-y)))
	#print(summary(o))
	
	#Pyrethrins (P)
	dP1=c(.5,.75,1,1.5,2);KP1=c(.2,.35,.53,.8,.88);LdP1=log(dP1)
	dP2=c(.5,.75,1,1.5,2);KP2=c(.23,.44,.55,.72,.9);LdP2=log(dP2)
	
	xP=c(LdP1,LdP2);yP=c(KP1,KP2)	
	#o=glm(yP~xP,family=binomial(probit));aP=as.vector(coef(o))
#	plot(aP[1]+aP[2]*xP,log(y/(1-y)))
	#print(summary(o))	
	
	# 1:5 mixture
	d15=c(.3,.45,.6,.875,1.175);Kd15=c(.27,.53,.64,.82,.93)
	Ld15R=log(d15/6);Ld15P=log(d15*5/6)
	x1C=c(xR,rep(mINF,10),Ld15R);x2C=c(rep(mINF,10),xP,Ld15P)
	yC=c(yR,yP,Kd15)
	oC=nls(yC~bmn2.rc(x=x1C,y=x2C,ab11,ab12,ab21,ab22,ro),start=c(ab11=3,ab12=1.5,ab21=.15,ab22=1.5,ro=-.6))
	aC=coef(oC)
	print(summary(oC))
	
	xs=seq(from=mINF,to=0,length=N)
	ys=seq(from=mINF,to=1,length=N)
	surf=matrix(NA,N,N)
	for(i in 1:N)
	for(j in 1:N)
		surf[i,j]=bmn2(x=xs[i],y=ys[j],ab11=aC[1],ab12=aC[2],ab21=aC[3],ab22=aC[4],ro=aC[5])
	
	par(mar=c(2,0,0,0))
	op=persp(xs,ys,surf,theta=30,phi=60,r=10,ticktype="detailed",zlab="Proportion killed",xlab="log Rotenon",ylab="log Pyrethrins")
	
	m2=bmn2.rc(x=x1C,y=x2C,ab11=aC[1],ab12=aC[2],ab21=aC[3],ab22=aC[4],ro=aC[5])
	p3=trans3d(x=x1C,y=x2C,m2,pmat=op)
	points(p3$x,p3$y,pch=16,cex=1,col=2)
		
	p3=trans3d(x=x1C,y=x2C,yC,pmat=op)
	points(p3$x,p3$y,pch=16,cex=1,col=3)
	
	
	p2=trans3d(x=x1C, y=x2C, z=rep(min(surf),length(x1C)),pmat=op)
	points(p2$x,p2$y,pch=3,cex=1,col=4)
	segments(p3$x,p3$y,p2$x,p2$y,col=4)	
	#legend("topleft",c("Data","Fitted"),pch=16,col=c(2,3),cex=1.5)
	legend(-.14,.12,c("Data","Fitted"),pch=16,col=c(2,3),cex=1.5)
	LL=contourLines(xs,ys,surf,levels=.5)
	
	p50=trans3d(x=LL[[1]]$x,y=LL[[1]]$y,rep(.5,length(LL[[1]]$y)),pmat=op)
	lines(p50$x,p50$y,lwd=3)
	p50=trans3d(x=LL[[1]]$x,y=LL[[1]]$y,rep(0,length(LL[[1]]$y)),pmat=op)
	lines(p50$x,p50$y,lwd=3,lty=2)
return()	
	par(mfrow=c(1,1),mar=c(4,4,3,1))
	x=mx=seq(from=0.0001,to=0.05,length=1000)
	y=.6-x
	for(i in 1:1000)
		mx[i]=bmn2(x=log(x[i]),y=log(y[i]),aC[1],aC[2],aC[3],aC[4],ro=aC[5])
	plot(x,mx,type="l")
	print(min(mx))
}
if(job==2) # 1:15 mixture
{
	#Rotenon (R)
	dR1=c(.1,.15,.2,.25,.35);KR1=c(.24,.44,.63,.81,.9);LdR1=log(dR1)
	dR2=c(.1,.15,.2,.25,.35);KR2=c(.28,.51,.72,.82,.89);LdR2=log(dR2)
	xR=c(LdR1,LdR2);yR=c(KR1,KR2)
	#o=glm(yR~xR,family=binomial(probit));aR=as.vector(coef(o))
	#plot(aR[1]+aR[2]*xR,log(y/(1-y)))
	#print(summary(o))
	
	#Pyrethrins (P)
	dP1=c(.5,.75,1,1.5,2);KP1=c(.2,.35,.53,.8,.88);LdP1=log(dP1)
	dP2=c(.5,.75,1,1.5,2);KP2=c(.23,.44,.55,.72,.9);LdP2=log(dP2)
	
	xP=c(LdP1,LdP2);yP=c(KP1,KP2)	
	#o=glm(yP~xP,family=binomial(probit));aP=as.vector(coef(o))
	#plot(aP[1]+aP[2]*xP,log(y/(1-y)))
	#print(summary(o))	
	
	# 1:15 mixture
	d115=c(.4,.6,.8,1.2,1.6);Kd115=c(.23,.48,.61,.76,.93)
	Ld115R=log(d115/16);Ld115P=log(d115*15/6)	
	
	x1C=c(xR,rep(mINF,10),Ld115R);x2C=c(rep(mINF,10),xP,Ld115P)
	yC=c(yR,yP,Kd115)
	oC=nls(yC~bmn2.rc(x=x1C,y=x2C,ab11,ab12,ab21,ab22,ro),start=c(ab11=2.4,ab12=1.2,ab21=.18,ab22=1.2,ro=.2))
	aC=coef(oC)
	print(summary(oC))
	
	xs=seq(from=mINF,to=0,length=N)
	ys=seq(from=mINF,to=1,length=N)
	surf=matrix(NA,N,N)
	for(i in 1:N)
	for(j in 1:N)
		surf[i,j]=bmn2(x=xs[i],y=ys[j],ab11=aC[1],ab12=aC[2],ab21=aC[3],ab22=aC[4],ro=aC[5])
	
	par(mar=c(2,0,0,0))
	op=persp(xs,ys,surf,theta=30,phi=60,r=10,ticktype="detailed",zlab="Proportion killed",xlab="log Rotenon",ylab="log Pyrethrins")
	
	m2=bmn2.rc(x=x1C,y=x2C,ab11=aC[1],ab12=aC[2],ab21=aC[3],ab22=aC[4],ro=aC[5])
	p3=trans3d(x=x1C,y=x2C,m2,pmat=op)
	points(p3$x,p3$y,pch=16,cex=1,col=2)
		
	p3=trans3d(x=x1C,y=x2C,yC,pmat=op)
	points(p3$x,p3$y,pch=16,cex=1,col=3)
	
	
	p2=trans3d(x=x1C, y=x2C, z=rep(min(surf),length(x1C)),pmat=op)
	points(p2$x,p2$y,pch=2,cex=1,col=4)
	segments(p3$x,p3$y,p2$x,p2$y,col=4)	
	
}	
if(job==3) #testing ro
{
	ro=.4;x=1;y=4
	covm=matrix(c(1,ro,ro,1),2,2)
	d1=pmvnorm(upper=c(x,y),sigma=covm)
	
	int1=function(v,U,ro)
	 pnorm((U-ro*v)/sqrt(1-ro^2))*dnorm(v)
	d2=integrate(int1,U=x,lower=-Inf,upper=y,ro=ro)$value 
	
	ros=dd=seq(from=-.9,to=.9,length=100)
	for(i in 1:100)
	dd[i]=integrate(int1,U=x,lower=-Inf,upper=y,ro=ros[i])$value 
	plot(ros,dd,type="l")
}
if(job==4) #contours
{
	N=200
	int1=function(v,U,ro)
	 pnorm((U-ro*v)/sqrt(1-ro^2))*dnorm(v)
	 
	dA=seq(from=0.001,to=0.41,length=N)
	dB=seq(from=0.001,to=0.5,length=N)
	ED50A=.4;ED50B=.5
	c2=matrix(ncol=N,nrow=N)
	ros=c(-.5,0,.5);cl=c(2,3,4)
	ms=c(1,2)
	par(mfrow=c(1,2),mar=c(4,4,3,1))
	for(im in 1:2)
	{
		m=ms[im]
		for(iro in 1:3)
		{
			for(i in 1:N)
			for(j in 1:N)
			{
				MAx=(dA[i]/ED50A)^m
				MAx=MAx/(1+MAx)
						
				MBy=(dB[j]/ED50B)^m
				MBy=MBy/(1+MBy)
			
				I1=integrate(int1,U=qnorm(MAx),lower=-Inf,upper=qnorm(MBy),ro=ros[iro])$value 
				c2[i,j]=MAx+MBy-I1
			}
			if(iro==1) a=FALSE else a=TRUE
			contour(dA,dB,c2,xlim=c(0,max(dA)),ylim=c(0,max(dB)),levels=0.5,add=a,col=cl[iro],lwd=3,drawlabels=F)				
		}	
		segments(0,ED50B,ED50A,0,lwd=3)
		mtext(side=1,"drug A dose",cex=1.5,line=2.75)
		mtext(side=2,"drug B dose",cex=1.5,line=2.5)
		mtext(side=3,paste("m =",m),cex=1.75,line=1,font=2)			
		if(im==1) ll=c(.17,.5) else ll=c(0,.14)
		legend(ll[1],ll[2],c("Loewe independence",paste("Copula ro=",ros)),lty=1,col=c(1,cl),lwd=3,bg="gray90",cex=1.35)
		if(im==1)
		{
			text(.07,.1,"Synergy",font=4,cex=1.5)
			text(.33,.25,"Antagonism",font=4,cex=1.5)		
		}
		else
		{
			text(.1,.25,"Synergy",font=4,cex=1.5)
			text(.35,.43,"Antagonism",font=4,cex=1.5)		
		}
	}
}
if(job==5)
{
	int1=function(v,U,ro)
	pnorm((U-ro*v)/sqrt(1-ro^2))*dnorm(v)
	
	ros=M2=seq(from=-.999,to=.999,length=1000)
	Mx=.6;My=.5
	for(i in 1:1000)
	M2[i]=Mx+My-integrate(int1,U=qnorm(Mx),lower=-Inf,upper=qnorm(My),ro=ros[i])$value 
	plot(ros,M2,type="l",ylim=c(0,1),main=paste(Mx+My))
	segments(-1,Mx+My,1,Mx+My,col=2)
	segments(-1,max(Mx,My),1,max(Mx,My),col=3)

}

return()
}
