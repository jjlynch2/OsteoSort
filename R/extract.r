extract <- function(L1, L2) {
	points_1 <- unique(L1[L1[,5] != 0,5])
	points_2 <- unique(L2[L2[,5] != 0,5])
	first <- matrix(0,1,3)
	colnames(first) <- colnames(L1[,1:3])
	second <- matrix(0,1,3)
	colnames(second) <- colnames(L2[,1:3])
	if(length(points_1) > 0 && length(points_2) > 0) {
		for(n1 in points_1) {
			for(n2 in points_2) {
				if(n1 == n2) {
					first <- rbind(first, L1[L1[,5] == n1,1:3] )
					second <- rbind(second, L2[L2[,5] == n2,1:3] )
				}
			}
		}
		first <- first[-1,]
		second <- second[-1,]
		ind <- ind[-1,]
		if(nrow(first) < 3) {
			return(FALSE)
		}
		return(list(first, second))
	} else {
		return(FALSE)
	}
}
