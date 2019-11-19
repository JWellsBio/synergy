daphnia <-
function(dr="c")
{
dump("daphnia",paste(dr,":\\Projects\\ToddMiller\\Synergy\\daphnia.r",sep=""))
da=read.csv("Daphnia.csv")
namgr=c("NiCl=1.8","CuSO4=7","NiCl=1.8 & CuSO4=7","Independence") # names to be displayed
tr=da[,1]
A=da[tr=="A",2];B=da[tr=="B",2];D=da[tr=="AB",2] #SFs
n1=length(A);n2=length(B);n3=length(D)  #n
lA=log(A);lB=log(B);lD=log(D) #log(SF)
y1=mean(lA);y2=mean(lB);y3=mean(lD)
s1=var(lA)*(n1-1);s2=var(lB)*(n2-1);s3=var(lD)*(n3-1)
sy=s1+s2+s3 # total suam of squares
dft=n1+n2+n3-3;denf=1/n1+1/n2+1/n3 #df
tss=(y1+y2-y3)/sqrt(sum(sy)/dft)/denf	# t-test statistic
pv=2*(1-pt(abs(tss),df=dft)) # p-value for Bliss independence hypothesis
pvP=1-pt(tss,df=dft)

print("Testing for Bliss independence:")
print(paste("T-stat=",round(tss,3),", y1+y2-y3=",round(y1+y2-y3,3),", exp(y1+y2-y3)=",round(exp(y1+y2-y3),3),", p-value=",round(pv,3),sep=""))

# Box plot	
m=matrix(ncol=3,nrow=4)
m[1:3,1]=lA;m[,2]=lB;m[,3]=lD
indep=rep(0,n1*n2)
k=0
for(i1 in 1:n1)
for(i2 in 1:n2)
{
	k=k+1
	indep[k]=m[i1,1]+m[i2,2]
}

par(mfrow=c(1,1),mar=c(4,4,1,1))
yl=c(.3,.4,.5,.6,.7,.8,.9) # tickmarks for the original SF
lyl=log(yl) # log tickmarks
boxplot(list(lA,lB,lD,indep),names=namgr,xlab="",ylab="",ylim=range(lyl),axes=F)
mtext(side=1,"Treatment groups",cex=1.25,line=2.75)
mtext(side=2,"Surviving fraction, %",cex=1.25,line=2.5)
axis(side=2,at=lyl,as.character(yl*100))
axis(side=1,at=1:4,namgr)
lines(x=c(3,3,4,4),y=c(log(.55),log(.6),log(.6),log(.55)),lwd=3)
text(3.5,log(.64),paste("P-value =",round(pv,3)),font=2)
yn=log(c(70,50,32,36.5)/100)
text(1:4,yn,paste("n=",c(n1,n2,n3,n1*n2),sep=""),cex=1.25)
}
