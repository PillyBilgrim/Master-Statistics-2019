#Different little bits from the regression theory chapter

######################################
#Linear moodel
l.model <- lm(1-G~r, data=train)

ggplot()+
xlim(0,70)+
ylim(-0.2, 1.2)+

labs(title= 'Univariate Linear Regression on Probability of Save')+
theme(plot.title=element_text(size=10))+
labs(x = 'Distance', y= 'Save or Not')+





ggplot()+
xlim(0,70)+
ylim(-0.2, 1.2)+

labs(title= 'Simplest Regression Model',
subtitle= 'Mapping shots perfectly')+
theme(plot.title=element_text(size=15))+
theme(plot.subtitle=element_text(size=10))+
labs(x = 'Distance', y= 'Save or Not')+

geom_line(mapping=aes(x= test$r, y=1-test$G))

###########################################################
#GLM

g.model <- glm(1-G~r, data=train, family=binomial())

g.map <- data.frame(r = test$r, s = 1-test$G, p =  predict(g.model,newdata=test, type='response'))




ggplot(data=g.map, aes(x=r, y=s))+
xlim(0,70)+
ylim(-0.2, 1.2)+

labs(title= 'Univariate General Linear Regression on Probability of Save')+
theme(plot.title=element_text(size=10))+
labs(x = 'Distance', y= 'Save or Not')+

geom_point()+
geom_line(mapping=aes(x= r, y=p) )

###################################################
#GAM


r.1 <- train$r[train$r < median(train$r)]
r.2 <- train$r[train$r >= median(train$r)]

s.1 <- 1-train$G[train$r < median(train$r)]
s.2 <- 1-train$G[train$r >= median(train$r)]

l1.model <- lm(s.1~r.1)
l2.model <- lm(s.2~r.2)

rt.1 <- test$r[test$r < median(train$r)]
rt.2 <- test$r[test$r >= median(train$r)]

st.1 <- 1-test$G[test$r < median(train$r)]
st.2 <- 1-test$G[test$r >= median(train$r)]

p.1 <- predict(l1.model,newdata=list(r.1=rt.1), type='response')
p.2 <- predict(l2.model,newdata=list(r.2=rt.2), type='response')

ggplot()+
xlim(0,70)+
ylim(-0.2, 1.2)+

labs(title= 'Naive piecewise linear approximation')+
theme(plot.title=element_text(size=10))+
labs(x = 'Distance', y= 'Save or Not')+

geom_point(mapping=aes(x=c(rt.1,rt.2), y=c(st.1,st.2)))+
geom_line(mapping=aes(x= rt.1, y=p.1))+
geom_line(mapping=aes(x= rt.2, y=p.2))



ex.splines <- glm(1-G~ns(r, knots = c(5, 15, 30), Boundary.knots=c(0,65)), data=train, family=binomial())

s.map <- data.frame(r =  test$r, s = 1-test$G,
 p = predict(ex.splines, newdata=test, type='response'))



ggplot(data=s.map, aes(x=r, y=p))+
xlim(0,70)+
ylim(-0.2, 1.2)+

labs(title= 'Natural Cubic Spline Regression')+
theme(plot.title=element_text(size=10))+
labs(x = 'Distance', y= 'Save or Not')+

geom_point(mapping=aes(x=r, y=s))+
geom_line()

########################################################
#Two dimensional splines

ggplot(ex.plot)+
xlim(-5, 50)+
annotate_pitch()+
geom_segment(x=ex.plot$x, y=ex.plot$y, xend=ex.plot$x.e,
 yend=ex.plot$y.e, arrow=arrow(angle = 15, type='closed'))

#two dimensional (tdsp) and two marginal (twspp) spline models

tdsp.model <- gam(save~ te(x,y, k=4), data=train, family=binomial())
twsp.model <- gam(save~ns(x, df=4)+ns(y, df=4), data=train, family=binomial())

n1 <- 30  #number of boxes in each direction
n2 <- 60  #to make a plotting grid

gr.x <- rep((n1-1):0 *30/n1, each=n2)  
gr.y <- rep(20:(n2+19)*60/n2, n1)
pltw.grid <- data.frame(x = gr.x, y = gr.y)
pltw.grid$Prob = predict(twsp.model, newdata=pltw.grid, type='response')

pltd.grid <- data.frame(x = gr.x, y = gr.y)
pltd.grid$Prob = predict(tdsp.model, newdata=pltd.grid, type='response')

ggplot(pltd.grid,aes(x=x, y=y, z=Prob))+
 annotate_pitch()+
 xlim(0,30)+
 ylim(20,80)+
labs(title= 'Two-Dimensional Spline Regression')+
theme(plot.title=element_text(size=10))+
labs(x = 'X-value', y= 'Y-value')+
 geom_raster(aes(fill=Prob), interpolate=T)








