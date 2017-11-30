library(adegraphics)
pdf("s1d.gauss.pdf")

data(meau, package= "ade4")
envpca <- ade4::dudi.pca(meau$env, scannf = FALSE)
dffac <- cbind.data.frame(meau$design$season, meau$design$site)
g11 <- s1d.gauss(envpca$li[, 1], dffac[, 1])
g12 <- s1d.gauss(envpca$li[, 1], dffac[, 1], p1d.rev = TRUE)
g13 <- s1d.gauss(envpca$li[, 1], dffac[, 1], p1d.hori = FALSE)
g14 <- s1d.gauss(envpca$li[, 1], dffac[, 1], p1d.hori = FALSE, p1d.rev = TRUE)

g2 <- s1d.gauss(envpca$li[, 1], dffac[, 1], ppoly.col = 1:4, fill = TRUE, plines.col = 1:4, col = FALSE)

g31 <- s1d.gauss(envpca$li[, 1], dffac[, 2], ppoly.col = 1:4, paxes.draw = TRUE, ylim = c(0, 2), fill = TRUE, p1d.hori = FALSE)
g32 <- s1d.gauss(envpca$li[, 1], dffac[, 2], ppoly.col = 1:4, paxes.draw = TRUE, fill = TRUE, p1d.hori = FALSE)

g4 <- s1d.gauss(envpca$li[, 1], fac = dffac, fill = TRUE, col = 1:5)
g5 <- s1d.gauss(envpca$li[, 1], fac = dffac, fill = TRUE, col = FALSE, ppoly.col = 1:6)
g6 <- s1d.gauss(envpca$li[, 1], fac = dffac[, 1], fill = TRUE, col = 1:6, ppoly.col = 1:6)

g7 <- s1d.gauss(envpca$li[, 1], fac = dffac, fill = TRUE, col = 1:6, ppoly.col = 1:6, steps = 10)
g8 <- s1d.gauss(envpca$li[, 1], dffac[, 2])

update(g11, p1d.reverse = TRUE)
update(g12, p1d.reverse = FALSE)
update(g13, p1d.reverse = TRUE)
update(g14, p1d.reverse = FALSE)
