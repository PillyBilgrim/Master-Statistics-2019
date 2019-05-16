

#'AllSaveDataTippeligaen.csv'
#'AllShotsOnGoalTippeligaen.csv'

#x: 0 is at goal-line of defending keeper
#y: 0 is to the left of defending keeper

#shotplot-function
#input:
#mydata: original data read from a csv-file, contains all info about shot
#savedata: data about the save, if added, will be paired with the shot-data
#saves: boolean for if savedata should be considered
#name: name of the jpeg if plot is true
#plot: boolean for if a plot of the shots should be made

#output:
#a dataframe of shot-details for all the shots, with added savedata if wanted

shotplot <- function(mydata, savedata = {}, saves = F){





n <- length(mydata$x)



x <- {}       #x-coordinate of origin of shot
y <- {}	  #y-coordinate of origin of shot
G <- {}	  #goal or no-goal

hea <- {}      #type of body-part
foot.l <- {}
foot.r <- {}

pen <- {}       #binary grouped variables for type of shot
free.k <- {}
set.p <- {}
corner <- {}
fast.break <- {}
oneone <- {}

strong <- {}    #other variable groups
weak <- {}
big.ch <- {}
deflection <- {}

xk  <- {}	  #x-coordinate for save point, if any
yk <- {} 	  #y-coordinate for save point, if any

xg <- {}     #x-coordinate for placement on shot on goal
yg <- {}	 #y-coordinate for placement on shot on goal

r <- {}  #distance from shot to point on goal
save.n <- {}   #number of save made already in match

team <- {}
id <- {}

period <- {}
min <- {}
sec <- {}
half <- {}

m.id <- 0
score <- {}


for(i in 1:n){
	ok <- 0                            #to check if I should add the qualifiers, 
	team <- c(team, mydata$team_id[i]) # this is mostly for if I added saves, which 
	id <- c(id, mydata$id[i])          #I abandonded because it diid not add information

	if(m.id != mydata$match_id[i]){	
		m.id <- mydata$match_id[i]
		save.1 <- 0
		save.2 <- 0
		score.1 <- 0
		id.1 <- team[i]
	}

	


	
	##########################
	if(mydata$type_id[i] == 16){  #type id 16 is a goal
		x <- c(x, 100-mydata$x[i])
		y <- c(y, 100-mydata$y[i])
		G <- c(G, 1)
		xk <- c(xk, 0)
		yk <- c(yk, 0)
		if(id.1 == mydata$team_id[i]){   #the goal is for team 1, keeper from team 2
			save.n <- c(save.n, save.2)
			score <- c(score, -score.1)
			score.1 <- score.1+1
		}else{
			save.n <- c(save.n, save.1)
			score <- c(score, score.1)
			score.1 <- score.1-1
		}		
		ok <- 1

	}else if(mydata$type_id[i] == 15 & saves){ #type id 15 is a saved shot
		
		if(id.1 == mydata$team_id[i]){   #the goal is for team 1, keeper from team 2
			save.n <- c(save.n, save.2)
			save.2 <- save.2 + 1
			
			
		}else{
			save.n <- c(save.n, save.1)
			save.1 <- save.1 + 1
		}


		pass <- 0
		j <- {}
		k <- {}
		l <- {}
		
		#this is to match save evnent with shot event
		#only 4 events are removed with this method

		j <- which(savedata$match_id %in% mydata$match_id[i])
		k <- which(savedata$min %in% mydata$min[i])
		l <- which(savedata$sec %in% mydata$sec[i])
				
		j <- Reduce(intersect, list(j,k,l)) #problem when two events in same sec, need qualifier
		


		if(length(j) == 1){   #if length = 1 then there is a unique match
			xk <- c(xk, savedata$x[j])
			yk <- c(yk, 100 - savedata$y[j])

			x <- c(x, 100-mydata$x[i])
			y <- c(y, 100-mydata$y[i])
			G <- c(G, 0)
			ok <- 1
		}else if(length(j) == 2 & pass == 0){
			xk <- c(xk, savedata$x[j[1]])
			yk <- c(yk, 100 - savedata$y[j[1]])

			x <- c(x, 100-mydata$x[i])
			y <- c(y, 100-mydata$y[i])
			G <- c(G, 0)
			ok <- 1
			pass <- 1
		}else if(length(j) == 2 & pass == 1){
			xk <- c(xk, savedata$x[j[2]])
			yk <- c(yk, savedata$y[j[2]])

			x <- c(x, 100-mydata$x[i])
			y <- c(y, mydata$y[i])
			G <- c(G, 0)
			ok <- 1
			

		}else{ print(i)}
	
	}else if(mydata$type_id[i] == 15){
		x <- c(x, 100-mydata$x[i])
		y <- c(y, 100-mydata$y[i])
		G <- c(G, 0)
		ok <- 1

		if(id.1 == mydata$team_id[i]){   #the shot is for team 1, keeper from team 2
			save.n <- c(save.n, save.2)
			save.2 <- save.2 + 1
			score <- c(score, -score.1)
		}else{
			save.n <- c(save.n, save.1)
			save.1 <- save.1 + 1
			score <- c(score, score.1)
		}	


	#13 is missed shot and 14 is shot off the post
	}else if (mydata$type_id[i] == 13 | mydata$type_id[i] == 14){
		x <- c(x, 100-mydata$x[i])
		y <- c(y, mydata$y[i])
		G <- c(G, 0)
		ok <- 1
		if(id.1 == mydata$team_id[i]){   #the goal is for team 1, keeper from team 2
			save.n <- c(save.n, save.2)
		}else{
			save.n <- c(save.n, save.1)
		}
	}else{
	}
	
	if(ok == 1){
		##First I record which body-part is used##

		if(grepl('"20": true', mydata$qualifiers[i])){
			hea <- c(hea,0)
			foot.l <- c(foot.l, 0)
			foot.r <- c(foot.r,1)

		}else if(grepl('"15": true', mydata$qualifiers[i])){
			hea <- c(hea,1)
			foot.l <- c(foot.l, 0)
			foot.r <- c(foot.r,0)

		}else if(grepl('"72": true', mydata$qualifiers[i])){
			hea <- c(hea,0)
			foot.l <- c(foot.l, 1)
			foot.r <- c(foot.r,0)
		}else{
			hea <- c(hea,0)
			foot.l <- c(foot.l, 0)
			foot.r <- c(foot.r,0)
		}
		######################
		#Then I check for other qualifiers

		if(grepl('"9": true', mydata$qualifiers[i])){
			pen <- c(pen,1)
		}else{
			pen <- c(pen,0)
		}

		if(grepl('"26": true', mydata$qualifiers[i])){
			free.k <- c(free.k,1)
		}else{
			free.k <- c(free.k,0)
		}

		if(grepl('"24": true', mydata$qualifiers[i])){
			set.p <- c(set.p,1)
		}else{
			set.p <- c(set.p,0)
		}
		
		if(grepl('"25": true', mydata$qualifiers[i])){
			corner <- c(corner,1)
		}else{
			corner <- c(corner,0)
		}	

		if(grepl('"23": true', mydata$qualifiers[i])){
			fast.break <- c(fast.break,1)
		}else{
			fast.break <- c(fast.break,0)
		}

		if(grepl('"89": true', mydata$qualifiers[i])){
			oneone <- c(oneone,1)
		}else{
			oneone <- c(oneone,0)
		}

		if(grepl('"113": true', mydata$qualifiers[i])){
			strong <- c(strong,1)
		}else{
			strong <- c(strong,0)
		}

		if(grepl('"133": true', mydata$qualifiers[i])){
			deflection <- c(deflection,1)
		}else{
			deflection <- c(deflection,0)
		}

		if(grepl('"114": true', mydata$qualifiers[i])){
			weak <- c(weak,1)
		}else{
			weak <- c(weak,0)
		}

		if(grepl('"214": true', mydata$qualifiers[i])){
			big.ch <- c(big.ch,1)
		}else{
			big.ch <- c(big.ch,0)
		}
	
		period <- c(period, mydata$period_id[i])
		min <- c(min, mydata$min[i])		
		sec <- c(sec, mydata$sec[i])	

		##########
		#Then I add shot location
		#############
		
		pl.x <- regexpr('\"102\":', toString(mydata$qualifiers[i])) + 6
		pl.y <- regexpr('\"103\":', toString(mydata$qualifiers[i]))+ 6
	
		Xg <- sub(',', '', substr(toString(mydata$qualifiers[i]), pl.x, pl.x+4))
		Yg <- sub(',', '', substr(toString(mydata$qualifiers[i]), pl.y, pl.y+4))
	 	
		#r <- c(r,1)
		r <- c(r, sqrt((x[i]*105/100)^2 + ((y[i]-as.numeric(Xg))*68/100)^2))

		xg <- c(xg, 1-(as.numeric(Xg)-45.2)/(54.8-45.2))  #x between 45.2-54.8 y between 0-38
		yg <- c(yg, as.numeric(Yg)/38)		      #from right-to-left and down-to-up
		
		
	}else{
	}
	

}
print(c('Raw data',n))       #for checking how much data has been lost to 
print(c('Processed data', length(x)))   #the same-second-problem

other <- rep(1, n)- foot.r - foot.l - hea
foot <- foot.l + foot.r
save <- as.factor(1-G)

if(saves){
	D <- data.frame(x,y,G, xk, yk, hea, foot.l,
	 foot.r, pen, free.k, set.p, corner, fast.break, xg, yg,
	 oneone, team, save.n, other, r, id, period, min, sec, foot, save, score, deflection)

}else{
	D <- data.frame(x,y,G, hea, foot.l, foot.r, pen, free.k, 
	set.p, corner, fast.break, xg, yg, oneone, team, save.n, other, r, id, strong, weak, big.ch,
	period, min, sec, foot, save, score, deflection)

}

return(D)
}


	




