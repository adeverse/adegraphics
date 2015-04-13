library(adegraphics)
pdf("add.pdf")

data(granulo, package = "ade4")
df <- data.frame(t(apply(granulo$tab, 1, function(x) x / sum(x))))
pca <- ade4::dudi.pca(df, scal = FALSE, scan = FALSE)

g1 <- s.arrow(ade4::dudi.pca(data.frame(df), scan = F, nf = 2)$co)
g2 <- s.label(pca$li, plabels.cex = 0.5, plabels.col = "blue", plot = F)
g3 <- add.ADEg(g2)
g4 <- s.label(pca$c1, plabels.col = "red", add = T)
g5 <- s.arrow(pca$c1, plabels.cex = 1.5)
g6 <- ADEgS(list(g1 = g1, g5 = g5), layout = c(1, 2))
update(g6, pback.col = "lightblue", g1.plabels.cex = 2, g5.plabels.col = "red")