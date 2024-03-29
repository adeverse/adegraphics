## delete/remove this file when 'scatter' functions will be removed in ade4

library(adegraphics)
pdf("ade4-functions.pdf")

##################### scatter.dudi
data(deug, package = "ade4")
dd1 <- ade4::dudi.pca(deug$tab, scannf = FALSE, nf = 4)
scatter(dd1, posieig = "bottomright")
scatter(dd1, posieig = "bottomright", plot = T, prop = TRUE)
scatter(dd1, posieig = "none", plot = T)
scatter(dd1, posieig = "bottomleft", plot = T)
scatter(dd1, posieig = "topright", plot = T)
scatter(dd1, posieig = "topleft", plot = T, eig.col = c("white", "blue", "red"))

data(rhone, package = "ade4")
dd1 <- ade4::dudi.pca(rhone$tab, nf = 4, scannf = FALSE)
g1 <- scatter(dd1, sub = "Principal component analysis", row = list(plabels.optim = TRUE), col.pla.boxes.alpha = 0.5)
g1[2, drop = TRUE]
scatter(dd1, row = list(sub = "Principal component analysis", plabels.optim = TRUE), col.pla.boxes.alpha = 0.5)
scatter(dd1, prop = TRUE, ppoints.cex = 0.2, density.plot = TRUE, row = list(threshold = 0.01))

scatter(dd1, posieig = "none")
scatter(dd1, posieig = "bottomright")
scatter(dd1, posieig = c(0.5, 0.5))
scatter(dd1, posieig = c(0.5, 0.5, 1, 1))


##################### scatter.coa
data(housetasks, package = "ade4")
par(mfrow = c(2, 2))
dd2 <- ade4::dudi.coa(housetasks, scan = FALSE)
ade4::scatter(dd2, method = 1, sub = "1 / Standard", posieig = "none")
ade4::scatter(dd2, method = 2, sub = "2 / Columns -> averaging -> Rows", posieig = "none")
ade4::scatter(dd2, method = 3, sub = "3 / Rows -> averaging -> Columns ", posieig = "none")
par(mfrow = c(1, 1))

g1 <- scatter(dd2, method = 1, row.sub = "1 / Standard", posieig = "none", plot = FALSE)
g2 <- scatter(dd2, method = 2, col.sub = "2 / Columns -> averaging -> Rows", posieig = "none", plot = FALSE)
g3 <- scatter(dd2, method = 3, row.sub = "3 / Rows -> averaging -> Columns ", posieig = "none", plot = FALSE)
G <- ADEgS(list(g1, g2, g3), layout = c(2, 2), plot = TRUE)

scatter(dd2, posieig = "none")
scatter(dd2, posieig = "bottomright")
scatter(dd2, posieig = c(0.5, 0.5))
scatter(dd2, posieig = c(0.5, 0.5, 1, 1))


##################### scatter.pco
data(yanomama, package = "ade4")
gen <- ade4::quasieuclid(as.dist(yanomama$gen))
gen1 <- ade4::dudi.pco(gen, scann = FALSE, nf = 3)
scatter(gen1, posieig = "none")
scatter(gen1, posieig = "bottomri")
scatter(gen1, posieig = c(0.5, 0.5))
scatter(gen1, posieig = c(0.5, 0.5, 1, 1))


##################### scatter.nipals
data(doubs, package = "ade4")
acp1 <- ade4::dudi.pca(doubs$env, scannf = FALSE, nf = 2)
nip1 <- ade4::nipals(doubs$env)
scatter(nip1, posieig = "none")
scatter(nip1, posieig = "bottomri")
scatter(nip1, posieig = c(0.5, 0.5))
scatter(nip1, posieig = c(0.5, 0.5, 1, 1))


##################### score.inertia - plot.inertia
data(housetasks, package = "ade4")
coa2 <- ade4::dudi.coa(housetasks, scann = FALSE)
res21 <- ade4::inertia(coa2, row = TRUE, col = FALSE)
plot(res21, posieig = "none")
plot(res21, posieig = "bottomri")
plot(res21, posieig = c(0.5,0.5))
plot(res21, posieig = c(0.5, 0.5, 1, 1))
score(res21, posieig = "none")
score(res21, posieig = "bottomri")
score(res21, posieig = c(0.5, 0.5))
score(res21, posieig = c(0.5, 0.5, 1, 1))

res22 <- ade4::inertia(coa2, row = FALSE, col = TRUE)
plot(res22, posieig = "none")
plot(res22, posieig = "bottomri")
plot(res22, posieig = c(0.5, 0.5))
plot(res22, posieig = c(0.5, 0.5, 1, 1))
score(res22, posieig = "none")
score(res22, posieig = "bottomri")
score(res22, posieig = c(0.5, 0.5))
score(res22, posieig = c(0.5, 0.5, 1, 1))

res23 <- ade4::inertia(coa2, row = TRUE, col = TRUE)
plot(res23, posieig = "none")
plot(res23, posieig = "bottomri")
plot(res23, posieig = c(0.5, 0.5))
plot(res23, posieig = c(0.5, 0.5, 1, 1))
score(res23, posieig = "none")
score(res23, posieig = "bottomri")
score(res23, posieig = c(0.5, 0.5))
score(res23, posieig = c(0.5, 0.5, 2, 2))

data(doubs, package = "ade4")
afc <- ade4::dudi.coa(doubs$fish, scannf = FALSE, nf = 5)
ic <- ade4::inertia.dudi(afc, row.inertia=TRUE, col.inertia=TRUE)
plot(ic, contrib = "abs", threshold = 0.1, type = "label")
plot(ic, contrib="abs", threshold = 0.1, type = "label", xax = 3, yax = 4) 


##################### plot.acm
data(lascaux, package = "ade4")

acm1 <- ade4::dudi.acm(lascaux$ornem, sca = FALSE)
p1 <- proc.time()
ade4::scatter(acm1)
Tade4 <- proc.time() - p1

p2 <- proc.time()
plot(acm1, ppoints.cex = 0.3, plot = T)
Tadegraphics <- proc.time() - p2
## faster calculus, longest display than for ade4


##################### plot.fca
data(coleo, package = "ade4")
coleo.fuzzy <- ade4::prep.fuzzy.var(coleo$tab, coleo$col.blocks)

fca1 <- ade4::dudi.fca(coleo.fuzzy, scannf = FALSE, nf = 3)
ade4::scatter(fca1)
plot(fca1)
