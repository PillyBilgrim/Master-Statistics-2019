###Analysing crosses
#source('Crosses.R')

library(ggplot2)
library(ggsoccer)
library(mgcv)
library(splines)
library(mboost)
source('shotsdataframe.R')



d <- read.csv('AllShotsOnGoalTippeligaen.csv', header=T)
dt <- shotplot(d)
corner.goal <- dt[which((dt$y<3 | dt$y>97) & dt$corner == 1 & dt$G==1),]

rm(list = c('d','dt'))

passes <- read.csv('CSVs/AllOffPasses.csv', header=T)
crosses <- read.csv('Keepere/AllCrosses.csv', header = T)
claimed <- read.csv('Keepere/AllClaims.csv', header = T)
corners <- read.csv('Keepere/AllCorners.csv', header = T)

f.crosses <- crosses[crosses$outcome == 'False',]
f.crosses$set.p  <- ifelse(grepl('"5": true', f.crosses$qualifiers),1,0)

c.crosses <- crosses[crosses$outcome == 'True',]

f <- length(f.crosses$x)
n <- length(c.crosses$x)
m <- length(claimed$x)

c.crosses$starting.x <- 100 - c.crosses$x
c.crosses$starting.y <- 100 - c.crosses$y
c.crosses$length <- rep(0,n)
c.crosses$angle <- rep(0,n)
c.crosses$set.p  <- ifelse(grepl('"5": true', c.crosses$qualifiers),1,0)

##############################################################################
for(i in 1:n){
	
	current <- c.crosses[i,]
	pl.x <- regexpr('\"140\":', toString(current$qualifiers)) + 6
	pl.y <- regexpr('\"141\":', toString(current$qualifiers)) + 6	
	pl.l <- regexpr('\"212\":', toString(current$qualifiers)) + 6
	pl.a <- regexpr('\"213\":', toString(current$qualifiers)) + 6
	
	X <- sub(',', '', substr(toString(current$qualifiers), pl.x, pl.x+4))
	Y <- sub(',', '', substr(toString(current$qualifiers), pl.y, pl.y+4))
	L <- sub(',', '', substr(toString(current$qualifiers), pl.l, pl.l+4))
	A <- sub(',', '', substr(toString(current$qualifiers), pl.a, pl.a+3))

	
	c.crosses$x[i] <- 100 - as.numeric(X)	
	c.crosses$y[i] <- 100 - as.numeric(Y)
	c.crosses$length[i] <- as.numeric(L)
	c.crosses$angle[i] <- as.numeric(A)

}

#dropping crosses that fall outside the box
c.crosses <- c.crosses[c.crosses$y < 79,]
c.crosses <- c.crosses[c.crosses$y > 21,]
c.crosses <- c.crosses[c.crosses$x < 17,]

n <- length(c.crosses$x)
##################################################################################

claimed$starting.x <- rep(0,m)
claimed$starting.y <- rep(0,m)
claimed$length <- rep(0,m)
claimed$angle <- rep(0,m)
claimed$iscorner <- rep(0,m)
claimed$related <- rep(0,m)
claimed$set.p <- rep(0,m)

snm <- 0
passes$time <- passes$min * 60 + passes$sec
f.crosses$time <- f.crosses$min * 60 + f.crosses$sec
corners$time <- corners$min * 60 + corners$sec

for(i in 1:m){


		pl.i <- regexpr('\"233\":', toString(claimed$qualifiers[i])) + 6
		I <- as.numeric(sub(',', '', substr(toString(claimed$qualifiers[i]), pl.i, pl.i+3)))
		

		#finding the id of the corresponding cross
		c.i <- which(f.crosses$match_id == claimed$match_id[i] & f.crosses$event_id == I)
		
		if(length(c.i) == 0){
			time <- 60 * claimed$min[i] + claimed$sec[i]
			c.i <- which(f.crosses$time < time & f.crosses$time>time-7 & f.crosses$match_id == claimed$match_id[i])
			c.i <- c.i[length(c.i)]
			
		}else{}	

		if(length(c.i) == 0){
			c.i <- which(corners$match_id == claimed$match_id[i] & corners$event_id == I)	

			if(length(c.i)==0){
				time <- 60 * claimed$min[i] + claimed$sec[i]
				c.i <- which(corners$time < time & corners$time>time-7 & corners$match_id == claimed$match_id[i])
				c.i <- c.i[length(c.i)]
					
			}else{}
			
			if(length(c.i)==0){
				c.i <- which(passes$match_id == claimed$match_id[i] & corners$event_id == I)		
				if(length(c.i)==0){
					time <- 60 * claimed$min[i] + claimed$sec[i]
					c.i <- which(passes$time < time & passes$time>time-7 & passes$match_id == claimed$match_id[i])
					c.i <- c.i[length(c.i)]
					
				}else{}	
				
				if(length(c.i)==0){
					

				}else{
					pl.l <- regexpr('\"212\":', toString(passes$qualifiers[c.i])) + 6
					pl.a <- regexpr('\"213\":', toString(passes$qualifiers[c.i])) + 6
		
					L <- sub(',', '', substr(toString(passes$qualifiers[c.i]), pl.l, pl.l+4))
					A <- sub(',', '', substr(toString(passes$qualifiers[c.i]), pl.a, pl.a+3))
				
	
					claimed$starting.x[i] <- 100 - passes$x[c.i]		
					claimed$starting.y[i] <- 100 - passes$y[c.i]
					claimed$length[i] <- as.numeric(L)
					claimed$angle[i] <- as.numeric(A)
					claimed$related[i] <- passes$id[c.i]
				}


			}else{
				claimed$iscorner[i] <- 1
				pl.l <- regexpr('\"212\":', toString(corners$qualifiers[c.i])) + 6
				pl.a <- regexpr('\"213\":', toString(corners$qualifiers[c.i])) + 6
		
				L <- sub(',', '', substr(toString(corners$qualifiers[c.i]), pl.l, pl.l+4))
				A <- sub(',', '', substr(toString(corners$qualifiers[c.i]), pl.a, pl.a+3))
				
	
				claimed$starting.x[i] <- 100 - corners$x[c.i]		
				claimed$starting.y[i] <- 100 - corners$y[c.i]
				claimed$length[i] <- as.numeric(L)
				claimed$angle[i] <- as.numeric(A)
				claimed$related[i] <- corners$id[c.i]

			}
		}else{
			
			pl.l <- regexpr('\"212\":', toString(f.crosses$qualifiers[c.i])) + 6
			pl.a <- regexpr('\"213\":', toString(f.crosses$qualifiers[c.i])) + 6
	

			L <- sub(',', '', substr(toString(f.crosses$qualifiers[c.i]), pl.l, pl.l+4))
			A <- sub(',', '', substr(toString(f.crosses$qualifiers[c.i]), pl.a, pl.a+3))

	
			claimed$starting.x[i] <- 100 - f.crosses$x[c.i]	
			claimed$starting.y[i] <- 100 - f.crosses$y[c.i]
			claimed$length[i] <- as.numeric(L)
			claimed$angle[i] <- as.numeric(A)	
			claimed$related[i] <- f.crosses$id[c.i]
			claimed$set.p[i] <- f.crosses$set.p[c.i]
		}

}

w.claimed <- claimed[claimed$length == 0,] #weird-claimed, cant find a matching cross
a.claimed <- claimed[claimed$length != 0,]



c.claimed <- a.claimed[a.claimed$iscorner == 1,]  #corners claimed
a.claimed <- a.claimed[a.claimed$iscorner == 0,]  #crosses claimed


##################################################################################
m <- length(a.claimed$x)

claim.y.n <- as.factor(c(rep(1, m), rep(0,n)))

nn <- length(claim.y.n)

faced <- data.frame( x = c(a.claimed$x, c.crosses$x),
			y = c(a.claimed$y, c.crosses$y),
			y.n = claim.y.n, 
			set.p = c(a.claimed$set.p, c.crosses$set.p),
			r = c(a.claimed$length, c.crosses$length),
			a = abs(3.15-c(a.claimed$angle, c.crosses$angle)))
faced$as <- I(faced$a)^2

claim.att <- data.frame(x=a.claimed$x, y=a.claimed$y,
		y.n= as.factor(ifelse(a.claimed$outcome=='True', 1,0)),
		r=a.claimed$length, a=abs(3.15-a.claimed$angle),
		set.p = a.claimed$set.p)
claim.att$as <- I(claim.att$a)^2

#angle 0 is straight backwards, 3.15 is straigth forward.
###############################################################



