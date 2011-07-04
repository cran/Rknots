# Script comments and history
# 2011
# 5:35:25 PM

# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################

msrPlot <-
function (points3D, text = TRUE, showNC = FALSE, ...) 
{
	if (missing(points3D)) 
		stop("msrPlot: Argument 'points3D' missing, with no default\n")
	if (!is.logical(text) | !is.logical(showNC))
		stop("msrPlot: Argument 'text' and 'showNC' must be logical\n")
	lines3d(points3D, ...)
	material3d(color = c("orange","brown"), lit = TRUE, 
			point_antialias = TRUE, line_antialias = TRUE)
	bg3d(sphere = TRUE, back = "filled", color="#887777")
	lines3d(points3D, ...)
	spheres3d(points3D, ...)
	if (text) {
	texts3d(points3D[2 : (nrow(points3D) - 1), ], 
			text = as.character(2 : (nrow(points3D) - 1)),
			cex = 0.8, adj = 1.35, col = "black")
	}
	if (showNC)
	texts3d(points3D[c(1, nrow(points3D)), ], text = c("N", "C"),
			cex = 1.5, adj = 1.35, col = "red")
}
