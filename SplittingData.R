#splitting the data
#source("SplittingData.R")

#split.data is a function to split the data according to input

#input:
#dt: data to be split, as a data-frame or matrix
#seed: number to remove randomness of sampling for repeatability
#ind:vector of natural numbers for splitting data

#output: none, this function assigns variables globally since there are to many to 
#return in a nice way

split.data <- function(dt, seed = 0, ind={}){
	if(seed != 0){
		set.seed(seed)
	}
	if(length(ind) == 0){
		n <- length(dt$x)
		

		smpsize <- floor(0.75*n)

		train_ind <- sample(seq(n), size = smpsize)

		assign("train", dt[train_ind,], envir = .GlobalEnv)
		assign("test", dt[-train_ind,], envir = .GlobalEnv)
	
	}else {

		assign("train", dt[ind,], envir = .GlobalEnv)
		assign("test", dt[-ind,], envir = .GlobalEnv)
		smpsize <- length(train$X)
	}
	

	assign("z", 1 - train$G, envir = .GlobalEnv)
	assign("z", 1 - train$G, envir = .GlobalEnv)
	assign("x", train$x, envir = .GlobalEnv)
	assign("y", train$y, envir = .GlobalEnv)
	assign("ys", y^2, envir = .GlobalEnv)

	assign("xg", train$xg, envir = .GlobalEnv)
	assign("yg", train$yg, envir = .GlobalEnv)

	assign("hea", train$hea, envir = .GlobalEnv)
	assign("foot.l", train$foot.l, envir = .GlobalEnv)
	assign("foot.r", train$foot.r, envir = .GlobalEnv)
	assign("other" , train$other, envir = .GlobalEnv)

	assign("free.k", train$free.k, envir = .GlobalEnv)
	assign("set.p", train$set.p, envir = .GlobalEnv)
	assign("corner", train$corner, envir = .GlobalEnv)
	assign("fast.break", train$fast.break, envir = .GlobalEnv)
	assign("pen", train$pen, envir = .GlobalEnv)
	assign("oneone", train$oneone, envir= .GlobalEnv)
	assign("save.n", train$save.n, envir=.GlobalEnv)

	
	assign("z.new", 1-test$G, envir = .GlobalEnv)
	assign("x.new", test$x, envir = .GlobalEnv)
	assign("y.new", test$y, envir = .GlobalEnv)
	assign("ys.new", y.new^2, envir = .GlobalEnv)

	assign("xg.new", test$xg, envir = .GlobalEnv)
	assign("yg.new", test$yg, envir = .GlobalEnv)


	assign("hea.new", test$head, envir = .GlobalEnv)
	assign("foot.l.new", test$foot.l, envir = .GlobalEnv)
	assign("foot.r.new", test$foot.r, envir = .GlobalEnv)
	assign("other.new", test$other, envir = .GlobalEnv)

	assign("free.k.new", test$free.k, envir = .GlobalEnv)
	assign("set.p.new", test$set.p, envir = .GlobalEnv)
	assign("corner.new", test$corner, envir = .GlobalEnv)
	assign("fast.break.new", test$fast.break, envir = .GlobalEnv)
	assign("pen.new", test$pen, envir = .GlobalEnv)
	assign("oneone.new", test$oneone, envir=.GlobalEnv)
	assign("save.n.new", test$save.n, envir=.GlobalEnv)
}