Avg_HD_KDTree <- function(f2, f1, threads) {
	f1t <- Rvcg::vcgCreateKDtree(f1)
	f2t <- Rvcg::vcgCreateKDtree(f2)
	clost1 <- Rvcg::vcgSearchKDtree(f1t,f2,1,threads=threads)
	clost2 <- Rvcg::vcgSearchKDtree(f2t,f1,1,threads=threads)
	Avg1 <- mean(clost1$distance)
	Avg2 <- mean(clost2$distance)
	Avg <- max(Avg1,Avg2)
	return(Avg)
}

Max_HD_KDTree <- function(f2, f1, threads) {
	f1t <- Rvcg::vcgCreateKDtree(f1)
	f2t <- Rvcg::vcgCreateKDtree(f2)
	clost1 <- Rvcg::vcgSearchKDtree(f1t,f2,1,threads=threads)
	clost2 <- Rvcg::vcgSearchKDtree(f2t,f1,1,threads=threads)
	Max1 <- clost1$distance[which.max(clost1$distance)]
	Max2 <- clost2$distance[which.max(clost2$distance)]
	Max <- max(Max1, Max2)
	return(max)
}

HD_KDTree_Ind <- function(f1, f2, threads,k=1) {
	f1t <- Rvcg::vcgCreateKDtree(f1)
	clost <- Rvcg::vcgSearchKDtree(f1t,f2,k,threads=threads)
	if(k > 1) {return(cbind(matrix(clost$distance), matrix(clost$index)))}
	return(cbind(clost$distance, clost$index))
}
