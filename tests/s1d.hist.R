library(ade4)
library(adegraphics)
pdf("s1d.hist.pdf")

set.seed(40)
x1 <- rnorm(1000)
g11 <- s1d.hist(x1)
g12 <- s1d.hist(x1, col = 1:10)
g13 <- s1d.hist(x1, col = FALSE, ppoly.col = 1:10)
g14 <- s1d.hist(x1, col = TRUE, ppoly.col = "blue")

set.seed(50)
x1 <- rnorm(1000)
g21 <- s1d.hist(x1) # p1d.hori = TRUE and p1d.reverse = FALSE by default
# g22 <- s1d.hist(x1, p1d.hori = TRUE,  p1d.rev = TRUE)
g23 <- s1d.hist(x1, p1d.hori = FALSE, p1d.rev = FALSE)
# g24 <- s1d.hist(x1, p1d.hori = FALSE, p1d.rev = TRUE)


# randtest.pcaiv
data(rpjdl, package = "ade4")
millog <- log(rpjdl$mil + 1)
coa1 <- dudi.coa(rpjdl$fau, scann = FALSE)
caiv1 <- pcaiv(coa1, millog, scan = FALSE)
set.seed(50)
rd11 <- randtest(caiv1)
plot(rd11)
set.seed(50)
rd12 <- randtest(caiv1, output = "full")
plot(rd12, nclass = 15) # must be the same output as rd11
set.seed(50)
rd13 <- randtest(caiv1, output = "full")
plot(rd13, nclass = 8)
plot(rd13, nclass = 8, plines.col = "red")
plot(rd13, nclass = 8, obs.plines.col = "red")
plot(rd13, nclass = 8, sim.plines.col = "red")

# randtest.dpcoa
data(humDNAm, package = "ade4")
dpcoahum <- dpcoa(data.frame(t(humDNAm$samples)), sqrt(humDNAm$distances), scan = FALSE, nf = 2)
set.seed(50)
rd21 <- randtest(dpcoahum)
plot(rd21)
rd22 <- randtest(dpcoahum, output = "full")
plot(rd22)

# randtest.amova (plot.krandtest)
amovahum <- amova(humDNAm$samples, sqrt(humDNAm$distances), humDNAm$structures)
set.seed(50)
rd31 <- randtest(amovahum, 49)
plot(rd31)
plot(rd31, plines.col = "red")
plot(rd31, g1.plines.col = "red")
set.seed(50)
rd32 <- randtest(amovahum, 49, output = "full")
plot(rd32)
plot(rd32, plines.col = "red")
plot(rd32, g1.plines.col = "red")
plot(rd32, nclass = 30, g2.pback.col = "lightblue")

# randtest.coinertia
data(doubs, package = "ade4")
dudi1 <- dudi.pca(doubs$env, scale = TRUE, scan = FALSE, nf = 3)
dudi2 <- dudi.pca(doubs$fish, scale = FALSE, scan = FALSE, nf = 2)
coin1 <- coinertia(dudi1,dudi2, scan = FALSE, nf = 2)
set.seed(50)
rd4 <- randtest(coin1)
plot(rd4)

# randtest.pcaivortho
data(rpjdl, package = "ade4")
millog <- log(rpjdl$mil + 1)
coa1 <- dudi.coa(rpjdl$fau, scann = FALSE)
caiv1 <- pcaiv(coa1, millog, scan = FALSE)
set.seed(50)
rd5 <- randtest(caiv1)
plot(rd5)

# randtest.rlq (plot.krandtest)
data(aviurba, package = "ade4")
coa1 <- dudi.coa(aviurba$fau, scannf = FALSE, nf = 2)
dudimil <- dudi.hillsmith(aviurba$mil, scannf = FALSE, nf = 2, row.w = coa1$lw)
duditrait <- dudi.hillsmith(aviurba$traits, scannf = FALSE, nf = 2, row.w = coa1$cw)
rlq1 <- rlq(dudimil, coa1, duditrait, scannf = FALSE, nf = 2)
set.seed(50)
rd6 <- randtest(rlq1)
plot(rd6)

# randtest.between
data(meaudret, package = "ade4")
pca1 <- dudi.pca(meaudret$env, scan = FALSE, nf = 3)
set.seed(50)
rd7 <- randtest(bca(pca1, meaudret$design$season, scan = FALSE), 99)
plot(rd7, main = "Monte-Carlo test")

# randtest.discrimin
set.seed(50)
rd8 <- randtest(discrimin(pca1, meaudret$design$season, scan = FALSE), 99)
plot(rd8, main = "Monte-Carlo test")
