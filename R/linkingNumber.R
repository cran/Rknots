# Script comments and history
# 2011
# Feb 11, 2011
# 8:39:54 AM

# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################

skeinSign <- function (points3D, edge.indices) 
{
	x <- (points3D[edge.indices[1] + 1, ] - points3D[edge.indices[1], ])[1:2]
	y <- (points3D[edge.indices[2] + 1, ] - points3D[edge.indices[2], ])[1:2]
	A <- rbind(x,y)
	cp <- A[1, 1] * A[2, 2] - A[1, 2] * A[2, 1]
	pointsij <- points3D[rep(edge.indices, each = 2) + c(0, 1), ]
	return(sign(cp) * singleIntersection(pointsij, "sign"))
}

linkingNumber <- function(points3D, ends, M = c())
{
	nedge <- nrow(points3D) - 1
	if (is.null(M)) {
		M <- intersectionMatrix(points3D)		
	}
	subM <- M[1 : (ends[1] - 1), (ends[1] + 1) : nedge]
	ones <- which(subM != 0, arr.ind = TRUE)
	ones[, 2] <- ones[, 2] + ends[1]
	skein.signs <- apply(ones, 1, skeinSign, points3D = points3D)
	lk <- sum(skein.signs) / 2
	return (lk)	
}
