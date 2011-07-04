# Script comments and history
# 2011
# 5:35:25 PM

# Author: Federico Comoglio @ D-BSSE, ETH Zurich
###############################################################################

fileImport <-
function (pdbID) 
{
	if (missing(pdbID)) 
		stop("fileImport: argument 'filename' missing, with no default\n")
	if (!is.character(pdbID)) 
		stop("fileImport: argument 'filename' must be string\n")    
	pdb <- read.pdb(pdbID)
	tab.selection <- as.matrix(table(pdb$atom[,"chain"], pdb$atom[,"elety"]))
	tab.selection <- as.matrix(tab.selection [,"CA"])
	colnames(tab.selection) <- "#residues"
	chain <- unique(pdb$atom[, "chain"])
	if (length(chain)!=1) {
		cat("Available chains\n")
		print(tab.selection)
		chain.sel <- toupper(readline("Chain to be read "))
		if (!(chain.sel %in% chain))
			stop("fileImport: chain not found\n")
		chain <- chain.sel
	}
	pdb$atom <- pdb$atom[pdb$atom[, "chain"] == chain, ]
	alphatrace <- pdb$atom[pdb$atom[, "elety"] == "CA", ]
	coordinates <- matrix(as.double(alphatrace[, c("x","y","z")]), ncol = 3)
	return(coordinates)
}

