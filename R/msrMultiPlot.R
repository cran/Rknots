# Script comments and history
# 2011
# Feb 11, 2011
# 2:00:51 PM

# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################


msrMultiPlot <-
		function (points3D, ends, ...)
{
	n <- nrow(points3D)
	ncomp <- length(ends) + 1
	colors <- 1 : ncomp
	exends <- c(0, ends, n)
	intervals <- list()
	for (k in 1 : ncomp) 
		intervals[[k]] <- (exends[k] + 1) : (exends[k + 1])
	for (i in 1 : length(intervals)) 
		msrPlot(points3D[intervals[[i]], ],
				col = colors[i],  ...)
	print(intervals)
}
