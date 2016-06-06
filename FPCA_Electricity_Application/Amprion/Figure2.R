
curve1 = as.numeric(Load_mat[354,-1]) # plot "high demand" curve (354=20.12.2010) 
curve1 = as.numeric(Load_mat[620,-1]) # plot "low demand" curve  (620=12.09.2011) 

x<-seq(1/96,1,length.out=96) # generate x


y = expectreg.ls(curve1~rb(x,"pspline"),estimate="sheet",expectiles=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99),smooth="gcv") # ci=TRUE
expectiles = y$fitted

# plot mean of expectiles
par(mar=c(5.2, 6.1, 2, 0.5)) 
plot(x,curve1,type="l",col=2,lwd=2,lty=2,cex.lab=1.5,xaxt='n', cex.axis=1.5,xlab="Time of day",ylab="Load in MW",ylim=c(14000,30000))
polygon(c(x,rev(x)),c(expectiles[,1],rev(expectiles[,7])),col="grey65",border="grey65")
polygon(c(x,rev(x)),c(expectiles[,2],rev(expectiles[,6])),col="grey55",border="grey55")
polygon(c(x,rev(x)),c(expectiles[,3],rev(expectiles[,5])),col="grey45",border="grey45")
lines(x,expectiles[,4],type="l",lwd=3)
lines(x,curve1,type="l",col=2,lty=2,lwd=3)
axis(1,c(0,0.25,0.5,0.75,1),c("00:00","06:00","12:00","18:00","24:00"),cex.axis=1.5)
