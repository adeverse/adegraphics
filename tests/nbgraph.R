library(adegraphics)
library(spdep)
library(spData)
library(lattice)
library(rgdal)
pdf("nbgraph.pdf")

columbus <- readOGR(system.file("shapes/columbus.shp", package = "spData")[1])
coords <- coordinates(columbus)
col.gal.nb <- read.gal(system.file("weights/columbus.gal", package = "spData")[1])

nbobject <- col.gal.nb
xyplot(coords[, 2] ~ coords[, 1],
  		 panel = function(...) {adeg.panel.nb(col.gal.nb, coords)})

g1 <- s.label(coords, nb = nbobject, porigin.include = F, plabels.cex = 0.7, 
              ppoints.cex = 2, Sp = columbus, pSp.col = "red", pSp.alpha = 0.5)
