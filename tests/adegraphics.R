graphics.off()
library(adegraphics)

xy <- cbind.data.frame(runif(7), runif(7))
g1 <- s.label(xy)

data(olympic, package = "ade4")
pca <- ade4::dudi.pca(olympic$tab, scan = FALSE)
g2 <- s.corcircle(pca$co, lab = names(olympic$tab))

g3 <- ADEgS(list(g1, g2), rbind(c(0, 0, 0.5, 1), c(0.5, 0, 1, 1)))
g4 <- ADEgS(list(g1, g2), layout = c(1, 2))
g5 <- s.label(xy, plabels.cex = 0, paxes.draw = TRUE)

g6 <- superpose(g1, g5, plot = TRUE)
g7 <- superpose(s.density(xy), g5)
 
g8 <- superpose(s.label(xy, plot = FALSE), s.label(xy, plabels.cex = 0, paxes.draw = TRUE, plot = FALSE), plot = TRUE)

g9 <- g8[1, drop = TRUE]
class(g9)
g10 <- g8[1, drop = FALSE]
class(g10)

g11 <- ADEgS(list(g8, g3), positions = rbind(c(0, 0, 0.5, 1), c(0.5, 0, 1, 1)))