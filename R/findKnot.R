##Script generated in:
# 2011
# 9:21:59 PM
#by: 
# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################

findKnot <- function(points3D) #, fast = FALSE)
{
	points3D <- AlexanderBriggs(points3D)
#	if(fast) {
#		poly <- mVA(points3D)
#		return(poly)
#	}
	tree.leaves <- skeinIterator(points3D, c())
	poly <- HOMFLYpolynomial(tree.leaves[[1]], tree.leaves[[2]], -1)
	return(poly)
}

findProteinKnot <- function(pdbID, join.gaps = FALSE) #, fast = FALSE)
{
	require(bio3d)
	data.out <- data.frame("NoName", Inf, Inf, stringsAsFactors = FALSE)
	colnames(data.out) <- c("PDB ID-(Part)", "#Aminoacids", "HOMFLY polynomial")
			#ifelse(fast, "Alexander polynomial", "HOMFLY polynomial"))
	
	points3D <- fileImport(pdbID)
	n <- nrow(points3D)
	
	if(!join.gaps) {
		split.points3D <- findGaps(points3D, 7)
		for (i in 1 : length(split.points3D)) {
			points3D <- split.points3D[[i]]
			if(is.vector(points3D)) {
				data.out[i, ] <- c(paste(pdbID, i, sep = "-"), 1, "ND")	
				next
			}
			n <- nrow(points3D)
			points3D <- centroidClosure(points3D, 2)
			poly <- findKnot(points3D) #, fast)
			data.out[i, ] <- c(paste(pdbID, i, sep = "-"), n, poly)
		}
		return(data.out)
	}
	points3D <- centroidClosure(points3D, 2)
	poly <- findKnot(points3D) #, fast)
	data.out[1, ] <- c(pdbID, n, poly)
	return(data.out)
}


