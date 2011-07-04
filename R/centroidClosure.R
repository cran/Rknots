##Script generated in:
# 2011
# 6:29:12 PM
#by: 
# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################

centroidClosure <- function (points3D, n.times = 2) {
	
	normV <- function (v)	return(sqrt(sum(v ^ 2)))
	
	findRadius <- function(points3D, G) {
		D <- apply(points3D, 1, function(x) normV(x - G))
		return(max(D))
	}
	
	n <- nrow(points3D)
	G <- colMeans(points3D)
	radius <- n.times * findRadius(points3D, G)
	
	first.point <- G + radius * ((points3D[1, ] - G) / 
				(normV(points3D[1,	] - G)))
	last.point <- G + radius * ((points3D[n, ] - G) / 
				(normV(points3D[n, ] - G)))
	points3D <- rbind(as.numeric(first.point), 
			points3D, as.numeric(last.point))
	return(points3D)
}
