library(ade4)
library(adegraphics)
pdf("s1d.barchart.pdf")

## 1 : reverse and horizontal
set.seed(40)
x1 <- rnorm(10)
g11 <- s1d.barchart(x1)
g12 <- s1d.barchart(x1, ppoly.col = "blue")

g21 <- s1d.barchart(x1, p1d.hori = FALSE, p1d.rev = TRUE)
g23 <- s1d.barchart(x1, p1d.hori = FALSE, p1d.rev = FALSE)
g24 <- s1d.barchart(x1, p1d.hori = TRUE, p1d.rev = TRUE)
g24 <- s1d.barchart(x1, p1d.hori = TRUE, p1d.rev = FALSE)

## 2 : at and sort
data(rpjdl, package = "ade4")
rpjdl.coa <- ade4::dudi.coa(rpjdl$fau, scannf = FALSE, nf = 4)
nam <- rownames(rpjdl.coa$co)
gg1 <- s1d.barchart(rpjdl.coa$co[, 1])
gg2 <- s1d.barchart(rpjdl.coa$co[, 1], labels = nam, at = 51:1)
gg3 <- s1d.barchart(rpjdl.coa$co[, 1], labels = nam, sort = TRUE)
gg4 <- s1d.barchart(rpjdl.coa$co[, 1], labels = nam, sort = TRUE, at = 51:1) # 'at' is ignored
gg5 <- s1d.barchart(rpjdl.coa$co, labels = nam, sort = TRUE)
gg6 <- s1d.barchart(rpjdl.coa$co, labels = nam, sort = FALSE)
stopifnot(gg6[[1]]@data$labels == gg6[[2]]@data$labels)
