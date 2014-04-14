## delete/remove this file when 'scatter' functions will be removed in ade4

rm(list = ls())
graphics.off()
library(adegraphics)

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

#rm(list = ls())
#graphics.off()
#library(adegraphics)
#
#library(MASS)
#byNcov <- function(n, val) {
#	if(length(n) != length(val))
#  	stop("error")
#  SigmaN <- list()
#  for(i in 1:length(n))
#    SigmaN[[i]] <- matrix(val[i], n[i], n[i])
#  
#  covamatrix <- function(ma) {
#    nbsub <- length(ma)
#    compl <- matrix(0, nbsub, 2)
#    for(i in 1:nbsub)
#      compl[i, ] <- dim(ma[[i]])
#    
#    ## a faire remplissage
#    matfull <- list(cbind(ma[[1]], matrix(0, compl[1, 1], sum(compl[2:nbsub, 2]))))
#    for(ips in 2:(nbsub - 1)) {
#      matbefore <- matrix(0, compl[ips, 1], sum(compl[1:(ips - 1), 2]))
#      matafter <- matrix(0, compl[ips, 1], sum(compl[(ips + 1):nbsub, 2]))
#      matfull[[ips]] <- cbind(matbefore, ma[[ips]], matafter)
#    }
#    
#    matfull[[nbsub]] <- cbind(matrix(0, compl[nbsub, 1], sum(compl[1:(nbsub - 1), 2])), ma[[nbsub]])
#    Sigma <- do.call("rbind", matfull)
#    for(i in 1:dim(Sigma)[1])
#      Sigma[i, i] <- 1
#    return(Sigma)
#  }
#  return(covamatrix(SigmaN))
#}
#
#Sigma <- byNcov(c(5, 4, 2, 2, 3), val = c(0.6, 0.4, 0.3, 0.2, 0.5))
#Sigma[5, 1:4] <- Sigma[1:4, 5] <- -0.45
#Sigma[4, 1:3] <- Sigma[4, 1:3] <- -0.3
#Sigma[8, 4] <- Sigma[4, 8] <- 0.25
#Sigma[7, 2] <- Sigma[2, 7] <- -0.1
#Sigma[12, 8] <- Sigma[8, 12] <- -0.2
#Sigma[16, 9] <- Sigma[9, 16] <- -0.4
#
#set.seed(1)
#newdf <- matrix(rnorm(4 * 50000), 50000)
#t <- 1
#for(i in seq(1, 50000, by = 12500)){
#  mux <- c(-1, -2.5, 1.5, 2.5)
#  muy <- c(-2.5, 1.5, -2, 2.5)
#  newdf[i:(i + 12499), 1:2] <- newdf[i:(i + 12499), 1:2] + mux[t]
#  newdf[i:(i + 12499), 3:4] <- newdf[i:(i + 12499), 3:4] + muy[t]
#  t <- t + 1
#}
#
#chol1 <- chol(Sigma)
#x <- matrix(rnorm(50000 * 16), 50000)
#x[, c(1, 8)] <- newdf[, 1:2]
#x[, c(7, 16)] <- newdf[, 3:4]
#dfxy <- x %*% chol1
#
#analyF <- ade4::dudi.pca(dfxy, nf = 4, scannf = FALSE, scale = F)
#scatter(analyF, xax = 1, yax = 2, prop = TRUE)


##################### scatter.coa test
rm(list = ls())
graphics.off()
library(adegraphics)

data(housetasks, package = "ade4")
par(mfrow = c(2, 2))
dd2 <- ade4::dudi.coa(housetasks, scan = FALSE)
ade4::scatter(dd2, method = 1, sub = "1 / Standard", posieig = "none")
ade4::scatter(dd2, method = 2, sub = "2 / Columns -> averaging -> Rows", posieig = "none")
ade4::scatter(dd2, method = 3, sub = "3 / Rows -> averaging -> Columns ", posieig = "none")
g1 <- scatter(dd2, method = 1, row.sub = "1 / Standard", posieig = "none", plot = FALSE)
g2 <- scatter(dd2, method = 2, col.sub = "2 / Columns -> averaging -> Rows", posieig = "none", plot = FALSE)
g3 <- scatter(dd2, method = 3, row.sub = "3 / Rows -> averaging -> Columns ", posieig = "none", plot = FALSE)
G <- ADEgS(list(g1, g2, g3), layout = c(2, 2), plot = TRUE)


##################### plot.acm test
rm(list = ls())
graphics.off()
data(lascaux, package = "ade4")

acm1 <- ade4::dudi.acm(lascaux$ornem, sca = FALSE)
p1 <- proc.time()
ade4::scatter(acm1)
Tade4 <- proc.time() - p1

p2 <- proc.time()
plot(acm1, ppoints.cex = 0.3, plot = T)
Tadegraphics <- proc.time() - p2
## faster caculus, longest display than for ade4


##################### plot.fca text
rm(list = ls())
graphics.off()
data(coleo, package = "ade4")
coleo.fuzzy <- ade4::prep.fuzzy.var(coleo$tab, coleo$col.blocks)

fca1 <- ade4::dudi.fca(coleo.fuzzy, scannf = FALSE, nf = 3)
ade4::scatter(fca1)
plot(fca1)
