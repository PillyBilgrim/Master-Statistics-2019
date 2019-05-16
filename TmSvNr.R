##################################
#This script is to analyse the time, score and number of saves made
#using the MB model.





##### TIME
#################################
d.ot <- read.csv('CSVs/Overtimes.csv')
d.ot <- d.ot[seq(1, length(d.ot$min), 2),]

ot.period <- ifelse(d.ot$min<55, 1,2)
ot.time <- d.ot$min+ floor(d.ot$sec/60+0.5) - ifelse(ot.period==1, 45, 90)

#fit all the datapoints to the MB model
fit.all <- predict(model.mb, newdata=dt, type='response')
diff.all <- (1-dt$G)- fit.all

#finding the interval numbers for the time intervals
timefives <- {}
for(i in 1:length(dt$G)){
	if(dt$period[i]==1){
	timefives <- c(timefives, min(floor(dt$min[i]*9/45),8))
	}else{timefives <- c(timefives, min(max(floor(dt$min[i]*18/90),9),18))
}}
