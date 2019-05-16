#finding models for crosses, must have run Crosses.R first

################################
#first its for all crosses faced

free.kicks <- faced[faced$set.p == 1, ]
faced <- faced[faced$set.p == 0, ]

set.seed(123)
nn <- length(faced$x)
train.ind <- sample(nn,0.75*nn)

train.faced <- faced[train.ind,]
test.faced <- faced[-train.ind,]

crossfac.model <- gam(y.n ~ te(x, y)+r+a+as, data=train.faced, family=binomial())

fit.fac <- predict(crossfac.model, type='response')
pred.fac <- predict(crossfac.model, newdata=test.faced, type='response')

dev.fac <- 2*-sum(ifelse(train.faced$y.n==1, log(fit.fac), log(1-fit.fac)))
dev.test.fac <- 2*-sum(ifelse(test.faced$y.n==1, log(pred.fac), log(1-pred.fac)))

#######

crossfac.model.mb <- gamboost(y.n ~ bspatial(x,y) + bols(r,a, as),
 data=train.faced, family=Binomial(), control=boost_control(mstop=300))
cvm <- cvrisk(crossfac.model.mb) 
crossfac.model.mb[mstop(cvm)]

fit.fac.mb <- predict(crossfac.model.mb, type='response')
pred.fac.mb <- predict(crossfac.model.mb, newdata=test.faced, type='response')

dev.fac.mb <- 2*-sum(ifelse(train.faced$y.n==1, log(fit.fac.mb), log(1-fit.fac.mb)))
dev.test.fac.mb <- 2*-sum(ifelse(test.faced$y.n==1, log(pred.fac.mb), log(1-pred.fac.mb)))

######################################################################
#Models for the attempted claims, not usable

set.seed(123)
mm <- length(claim.att$x)
train.ind <- sample(mm,0.75*mm)

train.claim.att <- claim.att[train.ind,]
test.claim.att <- claim.att[-train.ind,]

crossatt.model <- gam(y.n ~ te(x, y)+r+a+as, data=train.claim.att, family=binomial())

fit.att <- predict(crossatt.model, type='response')
pred.att <- predict(crossatt.model, newdata=test.claim.att, type='response')

dev.att <- 2*-sum(ifelse(train.claim.att$y.n==1, log(fit.att), log(1-fit.att)))
dev.test.att <- 2*-sum(ifelse(test.claim.att$y.n==1, log(pred.att), log(1-pred.att)))

#########

crossatt.model.mb <- gamboost(y.n ~ bspatial(x,y) + bols(r,a,as),
 data=train.claim.att, family=Binomial(), control=boost_control(mstop=300))
cvm <- cvrisk(crossatt.model.mb) 
crossatt.model.mb[mstop(cvm)]

fit.att.mb <- predict(crossatt.model.mb, type='response')
pred.att.mb <- predict(crossatt.model.mb, newdata=test.claim.att, type='response')

dev.att.mb <- 2*-sum(ifelse(train.claim.att$y.n==1, log(fit.att.mb), log(1-fit.att.mb)))
dev.test.att.mb <- 2*-sum(ifelse(test.claim.att$y.n==1, log(pred.att.mb), log(1-pred.att.mb)))


###################################################################

tr.cl.yn <- (as.integer(train.faced$y.n)-1)
te.cl.yn <- (as.integer(test.faced$y.n)-1)
diff.tr <- tr.cl.yn-fit.fac
diff.te <- te.cl.yn-pred.fac


