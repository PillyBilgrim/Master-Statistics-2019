#for plotting the number of shots in intervals simulation
#need to have run TmSvNr.R first

n.s <- 4000
m.s <- 1000

sim.nr <- matrix(rep(0, 19*m.s), ncol=19)
sim.ind <- matrix(rep(1:19, m.s), ncol=19, byrow=T)

sim.dist <- matrix(rep(0, 19*m.s), ncol=19)
sim.diff <- matrix(rep(0, 19*m.s), ncol=19)
sim.qual <- matrix(rep(0, 19*m.s), ncol=19)
sim.eff <- matrix(rep(0, 19*m.s), ncol=19)
sim.perg <- matrix(rep(0, 19*m.s), ncol=19)


for(i in 1:m.s){
	
	sim <- runif(n.s)*95
	sim.f <- ifelse(sim<46, pmin(floor(sim/45*9),8),floor((sim-1)/90*18))
	
	sim.rnr <- sample(1:4064, n.s, replace=T)
	sim.rf <- timefives[sim.rnr]

	for(j in 0:18){
		sim.nr[i,j+1] <- length(sim[sim.f==j])
		sim.dist[i,j+1] <- mean(dt$r[sim.rf==j])
		sim.diff[i,j+1] <- sum(diff.all[sim.rf==j])
		sim.qual[i,j+1] <- mean(fit.all[sim.rf==j])
		sim.eff[i,j+1] <- sum(dt$G[sim.rf==j])/length(dt$G[sim.rf==j])
		sim.perg[i,j+1] <- length(dt$G[which(sim.rf==j & fit.all<0.1)])
	}
}

jpeg('diff5int.jpg')

ggplot()+
xlab('Index of interval')+
ylab('Number of good shots')+
geom_point(aes(x=as.vector(sim.ind), y=as.vector(sim.perg)), alpha=0.02)+
geom_point(aes(x=1:19, y=five.perg), size=3 )+
geom_vline(xintercept=9.5)+
labs(title='Number of good shots per time interval')


dev.off()





