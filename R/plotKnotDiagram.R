##Script generated in:
# 2011
# 9:25:11 AM
#by: 
# Author: Maurizio Rinaldi @ University of Piemonte Orientale
###############################################################################

plotKnotDiagram <-
		function(points3D, ends, text = FALSE, verbose = TRUE, ...) {
	points2D <- points3D[, 1:2]
	npoints <- nrow(points3D)
	edeleted <- sort(c(0, ends, npoints+1))
	restrictionInt <- vector("list", npoints-1) 
	for (i in 1 : (npoints-1)) {
		restrictionInt[[i]] <- edgeIntersectionsK(points3D, 1:npoints, i)
	}
	print(restrictionInt)
	length(restrictionInt) <- npoints-1;
	restInt <- restrictionInt
	ncc <- diff(edeleted) - 1 #points of each component
	colors <- rep(hsv(seq(0, 1, length = length(ncc) + 1))[-1], ncc)
	ir <- setdiff(1 : npoints, edeleted) #last edeleted does not play any role
	sneg <- vector("list", npoints-1)
	for (i in 1:(npoints-2)) {
		temp <- nrow(restrictionInt[[i]]);
		if (length(temp) > 0)
		{
			int <- c()
			for (j in 1:temp)
			{
				#edge split condition
				if(restInt[[i]][j,]$sign == -1 &&
						is.element(restInt[[i]][j, ]$edge, edeleted) == FALSE)
					int = c(int, restrictionInt[[i]][j, ]$k)	
			}
			if (length(int) > 0) {
				sneg[[i]] <- sort(c(sneg[[i]], int))
			} 
		}
	}
	splot <- vector("list", npoints-1)
	for (i in 1:length(sneg)) sneg[[i]] <- c(0, sneg[[i]], 1)
	for (i in 1:(npoints-1))
	{
		if(length(sneg[[i]]) > 2)
		{
			for (j in 2:(length(sneg[[i]])-1))
			{ 
				s <- c(sneg[[i]] [j], sneg[[i]][ j - 1], sneg[[i ]][j+1]); 
				temp <- s[1] - (1/20)*(s[1] - s[2]); 
				temp2 <- s[1] + (1/20)*(s[3] - s[1]);
				if (temp < 0) { 
					temp <- s[1] - (1/30)*s[1];
					temp2 <- s[1] + (1/30)*s[1];
				}
				splot[[i]] <- c(temp, temp2, splot[[i]]);  
			}
		}
		splot[[i]] <- sort(c(0, splot[[i]], 1) )
	}
	plot(points2D, type="n", xlab = "", ylab = "", bty = "n")
	for (i in seq(length(ir)-1)) {
		kk <- ir[i]
		points2D <- points3D[, 1:2]
		for (j in seq(to=length(splot[[kk]])-1, by = 2)) { 
			P0 <- points2D[kk, ] +  splot[[kk]][j] * (points2D[kk + 1, ] - points2D[kk, ])
			P1 <- points2D[kk, ] +  splot[[kk]][j + 1] * (points2D[kk + 1, ] - points2D[kk, ])
			lines(c(P0[1], P1[1]), c(P0[2], P1[2]), col = colors[i], ...)
		} 
	}
	if (text) {
		text(points3D[1: npoints, ], 
				labels = as.character(1: npoints), offset = 0.75)
	}
	if (verbose) {
	return(list("colors" = colors,
					"sint" = sneg,
					"splot" = splot,
					"ir" = ir,
					"points3D" = points3D))
	}
}



