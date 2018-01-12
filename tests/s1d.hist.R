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


# randtest

data(rpjdl, package = "ade4")
millog <- log(rpjdl$mil + 1)
coa1 <- dudi.coa(rpjdl$fau, scann = FALSE)
caiv1 <- pcaiv(coa1, millog, scan = FALSE)
rd1 <- randtest(caiv1)
plot(rd1)
