graphics.off()
library(adegraphics)

## ex1
data(eurodist)
g1 <- table.value(eurodist)
g2 <- table.value(eurodist, ptable = list(x = list(cstmargin = 17), y = list(cstmargin = 17)))
g3 <- table.value(eurodist, ptable.x = list(cstmargin = 17, pos = "bottom"), ptable.y = list(cstmargin = 17))

## ex2
d <- as.dist(matrix(rep(1:5, 5), 5), upper = TRUE)
attr(d, "Labels") <-c ("A", "B", "C", "D", "E")
g4 <- table.value(d)

## ex3
data(irishdata, package = "ade4")
d.geo <- dist(irishdata$xy.utm)
g5 <- table.value(d.geo)
