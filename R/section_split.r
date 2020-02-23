section_split <- function(m1, m2) {
	m1a <- m1[,4]
	m2a <- m2[,4]
	m1 <- OsteoSort:::pca_align(m1[,1:3])
	m2 <- OsteoSort:::pca_align(m2[,1:3])
	m1 <- cbind(m1,m1a)
	m2 <- cbind(m2,m2a)
	m1_rows <- nrow(m1)
	m2_rows <- nrow(m2)
	if(m1_rows < m2_rows) {
		m1t <- m1
		m1 <- m2
		m2 <- m1t
		m1t <- m1_rows
		m1_rows <- m2_rows
		m2_rows <- m1t
	}
	n_splits <- list()
	ro <- round(m1_rows / m2_rows, digits=0)
	if (ro >= 2) {
		m_range_min <- min(m1[,1])
		m_range_max <- max(m1[,1])
		m_dist <- (m_range_max - m_range_min) / ro
		lower <- m_range_min
		upper <- m_range_min + m_dist
		for(i in 1:ro) {
			temp <- m1[m1[,1] >= lower,]
			n_splits[[i]] <- temp[temp[,1] <= upper,]
			lower <- lower + m_dist
			upper <- upper + m_dist
		}
	} else {
		n_splits[[1]] <- m1
	}
	return(list(n_splits, m2))
}
