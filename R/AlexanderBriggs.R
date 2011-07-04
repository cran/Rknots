# Script comments and history
# 2011
# Feb 28, 2011
# 8:04:50 AM

# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################

cross <- function(v1, v2) {
	V <- rbind(v1, v2)
	A <- V[ ,2:3]
	B <- V[ ,c(1,3)]
	C <- V[ ,1:2]
	cx <- A[1, 1] * A[2, 2] - A[1, 2]* A[2, 1]	
	cy <- B[1, 1] * B[2, 2] - B[1, 2]* B[2, 1]
	cz <- C[1, 1] * C[2, 2] - C[1, 2]* C[2, 1]
	return(c(cx, -cy, cz))
}

triangleIntersection <- function(triangle3D, segment) {
	v1 <- triangle3D[3, ] - triangle3D[1, ]
	v2 <- triangle3D[2, ] - triangle3D[1, ]
	n <- cross(v1, v2)	
	t <- (n %*% (segment[1, ] - triangle3D[1, ])) / (n %*% (segment[1, ] - segment[2, ]))
	P <- segment[1, ] + t * (segment[2, ] - segment[1, ])
	inside <- interiorTriangle(triangle3D[, 1 : 2], P[1 : 2])
	return(inside)
} 

interiorTriangle <- function(triangle2D, point) {
	vectors2D <- rbind(triangle2D[2, ] - triangle2D[1, ], triangle2D[3, ] - triangle2D[1, ])
	det.v <- vectors2D[1, 1] * vectors2D[2, 2] - vectors2D[1, 2] * vectors2D[2, 1]
	Ct <- matrix(c(vectors2D[2, 2], 
			-vectors2D[1, 2], -vectors2D[2, 1], vectors2D[1, 1]), ncol = 2)
	xs <- (1 / det.v) * Ct %*% (point - triangle2D[1, ])
	inside <- (xs[1] >0 & xs[2] > 0 & sum(xs) < 1)
	return(inside)
}

move3D <- function (points3D) {
	n <- nrow(points3D)

	repeat {
		n.in <- n
		perm <- c(3, 1, 2, 3)
		while (max(perm) <= n) {
			triangle <- points3D[perm, ]
			prev.int <- c()
			if((perm[2] - 2) >= 1) {
				for(i in 1 : (perm[2] - 2)) {
					segment <- rbind(points3D[i, ], points3D[i + 1, ])
					prev.int <- c(prev.int, triangleIntersection(triangle, segment))
				}
			}
			if(length(intersect(unique(prev.int), TRUE)) != 0) {
				perm <- perm + 1
				next
			}
			next.int <- c()
			if((perm[1] + 1) < nrow(points3D)) {
				for(i in (perm[1] + 1) : (nrow(points3D) - 1)) {
					segment <- rbind(points3D[i, ], points3D[i + 1, ])
					next.int <- c(next.int, triangleIntersection(triangle, segment))
				}
			}
			pooled.int <- c(prev.int, next.int)	
			total <- unique(pooled.int)		
			if(identical(total, FALSE) | identical(total, c())) 
				points3D <- points3D[-perm[3], ]
			perm <- perm + 1
			n <- nrow(points3D)
		}
		if(n == n.in)
			break
	}
	return(points3D)
}

AlexanderBriggs <- function(points3D) {
	points3D <- move3D(points3D)
	return(points3D)
}