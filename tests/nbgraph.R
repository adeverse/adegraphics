library(ade4)
library(adegraphics)
library(sp)
library(spdep)
library(lattice)

pdf("nbgraph.pdf")

data(elec88, package = "ade4")
coords <- sp::coordinates(elec88$Spatial)

xyplot(coords[, 2] ~ coords[, 1],
  		 panel = function(...) {adeg.panel.nb(elec88$nb, coords)})

g1 <- s.label(coords, nb = elec88$nb, porigin.include = F, plabels.cex = 0.7, 
              ppoints.cex = 2, Sp = elec88$Spatial, pSp.col = "red", pSp.alpha = 0.5)
