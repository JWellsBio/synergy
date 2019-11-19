synergy.surv <-
function(job=1,dr="c")
{
dump("synergy.surv",paste(dr,":\\Projects\\ToddMiller\\Synergy\\synergy.surv.r",sep=""))
#install.packages("survival")
library(survival)
if(job==1) #Larkin, Fig 1A Intention-to-Treat Population
{
	ni=c(316,292,271,177,170,160,147,136,132,124,106,86,50,38,14,9,6,2,1,1,1,0)
	niip=c(314,293,275,219,208,191,173,164,163,151,137,116,65,54,18,11,7,2,1,0,0,0)
	ip=c(315,285,265,137,118,95,77,68,63,54,47,42,24,17,7,4,3,0,0,0,0,0)
	ti=0:21
	sni=ni/ni[1];sip=ip/ip[1];sniip=niip/niip[1]
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	matplot(ti,cbind(sni,sip,sniip),type="s",lty=1,lwd=3,col=c(2,3,4),xlab="",ylab="",axes=F)
	axis(side=1,0:21);axis(side=2,seq(from=0,to=1,by=.1),srt=90)
	sind=1-(1-sni)*(1-sip)	
	lines(ti,sind,type="s",lwd=3)
	mtext(side=1,"Months",line=2.75,cex=1.5)
	mtext(side=2,"Progression-free survival, proportion",line=2.5,cex=1.5)
	legend(13,.9,c("Nivolumab","Ipilimumab","Nivolumab+ipilimumab","Drug independence"),cex=1.5,lty=1,col=c(2,3,4,1),lwd=3,bg="gray90")
	# Drug independence p-value
	
	nti=length(niip)
	p.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) p.nii=c(p.nii,rep(ti[i],niip[i-1]-niip[i])) 
	
	nti=length(sind)
	s.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) s.nii=c(s.nii,rep(ti[i],round(315*(sind[i-1]-sind[i])))) 
	da=as.data.frame(cbind(c(p.nii,s.nii),rep(1,length(p.nii)+length(s.nii)),c(rep(1,length(p.nii)),rep(0,length(s.nii)))))
	names(da)=c("dyy","rec","tr")
	fl.surv <- survfit(Surv(dyy, rec) ~ tr, data = da) 
	ss=survdiff(Surv(dyy, rec) ~ tr, data = da)
	pv=1-pchisq(ss[[5]],df=1)
	text(0,.05,paste("Drugs independence p-value =",round(pv,2)),cex=1.5,adj=0,font=2)	
}

if(job==2) #Larkin, Fig 1C Patients with PD-L1-Negative Tumors
{
	ni=c(208,192,178,108,105,98,88,80,76,74,63,50,31,24,9,5,4,2,1,1,1,0)
	niip=c(210,195,181,142,134,123,112,106,105,96,88,79,42,36,13,9,6,2,1,0,0,0)
	ip=c(202,183,166,82,72,59,44,39,35,26,22,12,8,3,1,0,0,0,0,0,0,0)
	#return(cbind(ni,niip,ip))
	ti=0:21
	sni=ni/ni[1];sip=ip/ip[1];sniip=niip/niip[1]
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	matplot(ti,cbind(sni,sip,sniip),type="s",lty=1,lwd=3,col=c(2,3,4),xlab="",ylab="",axes=F)
	axis(side=1,0:21);axis(side=2,seq(from=0,to=1,by=.1),srt=90)
	legend(13,.9,c("Nivolumab","Ipilimumab","Nivolumab+ipilimumab","Drug independence"),cex=1.5,lty=1,col=c(2,3,4,1),lwd=3,bg="gray90")
	
	sind=1-(1-sni)*(1-sip)
	lines(ti,sind,type="s",lwd=3)
	mtext(side=1,"Months",line=2.75,cex=1.5)
	mtext(side=2,"Progression-free survival, proportion",line=2.5,cex=1.5)
	# Drug independence p-value
	
	nti=length(niip)
	p.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) p.nii=c(p.nii,rep(ti[i],niip[i-1]-niip[i])) 
	
	nti=length(sind)
	s.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) s.nii=c(s.nii,rep(ti[i],round(315*(sind[i-1]-sind[i])))) 
	da=as.data.frame(cbind(c(p.nii,s.nii),rep(1,length(p.nii)+length(s.nii)),c(rep(1,length(p.nii)),rep(0,length(s.nii)))))
	names(da)=c("dyy","rec","tr")
	fl.surv <- survfit(Surv(dyy, rec) ~ tr, data = da) 
	ss=survdiff(Surv(dyy, rec) ~ tr, data = da)
	pv=1-pchisq(ss[[5]],df=1)	
	text(0,.05,paste("Drugs independence p-value =",round(pv,2)),cex=1.5,adj=0,font=2)	
}
if(job==3) #Two graphs side-by-side
{
	par(mfrow=c(1,2),mar=c(4,4,1,1))
	
	ni=c(316,292,271,177,170,160,147,136,132,124,106,86,50,38,14,9,6,2,1,1,1,0)
	niip=c(314,293,275,219,208,191,173,164,163,151,137,116,65,54,18,11,7,2,1,0,0,0)
	ip=c(315,285,265,137,118,95,77,68,63,54,47,42,24,17,7,4,3,0,0,0,0,0)
	ti=0:21
	sni=ni/ni[1];sip=ip/ip[1];sniip=niip/niip[1]
	matplot(ti,cbind(sni,sip,sniip),type="s",lty=1,lwd=3,col=c(2,3,4),xlab="",ylab="",main="",axes=F)
	mtext(side=3,"Intention-to-treat population",cex=1.5,line=-1)
	axis(side=1,0:21);axis(side=2,seq(from=0,to=1,by=.1),srt=90)
	sind=1-(1-sni)*(1-sip)	
	lines(ti,sind,type="s",lwd=3)
	mtext(side=1,"Months",line=2.75,cex=1.25)
	mtext(side=2,"Progression-free survival, proportion",line=2.5,cex=1.25)
	legend(10,.9,c("Nivolumab","Ipilimumab","Nivolumab+ipilimumab","Drug independence"),cex=1.25,lty=1,col=c(2,3,4,1),lwd=3,bg="gray90")
	# Drug independence p-value
	
	nti=length(niip)
	p.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) p.nii=c(p.nii,rep(ti[i],niip[i-1]-niip[i])) 
	
	nti=length(sind)
	s.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) s.nii=c(s.nii,rep(ti[i],round(315*(sind[i-1]-sind[i])))) 
	da=as.data.frame(cbind(c(p.nii,s.nii),rep(1,length(p.nii)+length(s.nii)),c(rep(1,length(p.nii)),rep(0,length(s.nii)))))
	names(da)=c("dyy","rec","tr")
	fl.surv <- survfit(Surv(dyy, rec) ~ tr, data = da) 
	ss=survdiff(Surv(dyy, rec) ~ tr, data = da)
	pv=1-pchisq(ss[[5]],df=1)
	text(0,.02,paste("Drugs independence p-value =",round(pv,2)),cex=1.25,adj=0,font=2)	


	ni=c(208,192,178,108,105,98,88,80,76,74,63,50,31,24,9,5,4,2,1,1,1,0)
	niip=c(210,195,181,142,134,123,112,106,105,96,88,79,42,36,13,9,6,2,1,0,0,0)
	ip=c(202,183,166,82,72,59,44,39,35,26,22,12,8,3,1,0,0,0,0,0,0,0)
	#return(cbind(ni,niip,ip))
	ti=0:21
	sni=ni/ni[1];sip=ip/ip[1];sniip=niip/niip[1]
	matplot(ti,cbind(sni,sip,sniip),type="s",lty=1,lwd=3,col=c(2,3,4),xlab="",ylab="",axes=F)
	axis(side=1,0:21);axis(side=2,seq(from=0,to=1,by=.1),srt=90)
	legend(10,.9,c("Nivolumab","Ipilimumab","Nivolumab+ipilimumab","Drug independence"),cex=1.25,lty=1,col=c(2,3,4,1),lwd=3,bg="gray90")
	mtext(side=3,"Patients with PD-L1-negative tumors",cex=1.5,line=-1)
	
	sind=1-(1-sni)*(1-sip)
	lines(ti,sind,type="s",lwd=3)
	mtext(side=1,"Months",line=2.75,cex=1.25)
	mtext(side=2,"Progression-free survival, proportion",line=2.5,cex=1.25)
	# Drug independence p-value
	
	nti=length(niip)
	p.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) p.nii=c(p.nii,rep(ti[i],niip[i-1]-niip[i])) 
	
	nti=length(sind)
	s.nii=NULL
	for(i in 2:nti)
	if(niip[i]<niip[i-1]) s.nii=c(s.nii,rep(ti[i],round(315*(sind[i-1]-sind[i])))) 
	da=as.data.frame(cbind(c(p.nii,s.nii),rep(1,length(p.nii)+length(s.nii)),c(rep(1,length(p.nii)),rep(0,length(s.nii)))))
	names(da)=c("dyy","rec","tr")
	fl.surv <- survfit(Surv(dyy, rec) ~ tr, data = da) 
	ss=survdiff(Surv(dyy, rec) ~ tr, data = da)
	pv=1-pchisq(ss[[5]],df=1)	
	text(0,.02,paste("Drugs independence p-value =",round(pv,2)),cex=1.25,adj=0,font=2)	


}

}
