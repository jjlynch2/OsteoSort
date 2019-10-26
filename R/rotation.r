rotation <-function(Z2,Z1) {
	k<-ncol(Z2)
	td <- t( Z2 ) %*% Z1
	sig <- sign( det( td ) )
	sv <- svd( td )

	U<-sv$v 
	V<-sv$u 

	V[,k] <- sig * V[,k]
	Gam <- U %*% t(V)

	return( list(Z1%*%Gam, Z2, rotation=Gam) )
}
