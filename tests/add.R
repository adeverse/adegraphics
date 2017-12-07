library(adegraphics)
pdf("add.pdf")
set.seed(40)

########################### add.ADEg ##############################

data(granulo, package = "ade4")
df <- data.frame(t(apply(granulo$tab, 1, function(x) x / sum(x))))
pca <- ade4::dudi.pca(df, scal = FALSE, scan = FALSE)

g1 <- s.arrow(ade4::dudi.pca(data.frame(df), scan = F, nf = 2)$co)
g2 <- s.label(pca$li, plabels.cex = 0.5, plabels.col = "blue", plot = F)
g3 <- add.ADEg(g2)
g4 <- s.label(pca$c1, plabels.col = "red", add = T)

g5 <- s.arrow(pca$c1, plabels.cex = 1.5, plot = FALSE)
g6 <- ADEgS(list(g1 = g1, g5 = g5), layout = c(1, 2))
update(g6, pback.col = "lightblue", g1.plabels.cex = 2, g5.plabels.col = "red")


############################## addhist ##############################
dfxy1 <- matrix(rnorm(200), ncol = 2)
gh1 <- s.label(dfxy1)
gh2 <- addhist(gh1)

dfxy2 <- dfxy1
dfxy2[, 2] <- dfxy2[, 2] + rnorm(100, 2)
gh3 <- s.label(dfxy2)
gh4 <- addhist(gh3, plot.polygon = list(col = "red"))

data(rpjdl, package = "ade4")
coa1 <- ade4::dudi.coa(rpjdl$fau, scannf = FALSE, nf = 4)
gh5 <- s.label(coa1$li)
gh6 <- addhist(gh5)


############################## addtext ##############################
# on a ADEg
addtext(g1, -1, 1, "Data Granulo", plabels.cex = 1.5, plabels.col = "red")
addtext(g1, -1, 1, c("Data", "Granulo"), plabels.cex = 1.5, plabels.col = c("red", "blue")) # the two labels are superposed
addtext(g1, -1, 1, c("Data", "Granulo"), plabels.cex = 1.5, plabels.col = "red") # the two labels are superposed

addtext(g1, c(-1, -0.5), 1, "Data Granulo", plabels.cex = 1.5, plabels.col = c("red", "blue"))
addtext(g1, c(-1, -0.5), 1, c("Data", "Granulo"), plabels.cex = 1.5, plabels.col = c("red", "blue"))

addtext(g1, -1, c(1, 0.9), c("Data", "Granulo"), plabels.cex = 1.5, plabels.col = "red")
addtext(g1, -1, c(1, 0.9), c("Data", "Granulo"), plabels.cex = c(1.5, 2), plabels.col = "red")

data(dunedata, package = "ade4")
afc1 <- ade4::dudi.coa(dunedata$veg, scannf = FALSE)
g7 <- table.value(dunedata$veg, symbol = "circle", ppoints.cex = 0.5, plot = FALSE)
addtext(g7, 1, 20, "A", plabels.srt = 45, plabels.boxes.draw = FALSE)

# on a ADEgS: juxtaposition
addtext(g6, 0.5, 0.5, "Text added", plabels.col = "blue", which = 1)
addtext(g6, 0.5, 0.5, "Text added", plabels.col = c("blue", "green"), which = 2)
addtext(g6, 0.5, 0.5, "Text added", plabels.col = c("green4", "blue"), which = 2)
addtext(g6, 0.5, 0.5, "Text added", plabels.col = "blue", which = 1:2)
addtext(g6, 0.5, 0.5, "Text added", plabels.col = c("green4", "blue"))
addtext(g6, c(0.7, -0.5), c(0.2, -0.4), "Text added", plabels.col = "blue", plabels.cex = 1.2, which = 1:2)
addtext(g6, c(0.7, -0.5), c(0.2, -0.4), "Text added", plabels.cex = c(0.5, 1.5), plabels.col = c("blue", "green4"))

xy2 <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -2, 2), y2 = runif(200, -0.5, 0.5))
fac2 <- factor(xy2$x > 0) 
g9 <- s.class(xy2, fac = fac2, xax = 1, yax = 2:3, plot = FALSE)
addtext(g9, 0, 0, "A", plabels.col = "red", plabels.cex = 2)
addtext(g9, c(-2.1, -1.07), c(2, 1), c("A", "B"), plabels.col ="red", plabels.cex = 2, which = 1:2)
addtext(g9, c(-2.1, -1.07), c(2, 1), c("A", "B"), plabels.col = c("green4", "red"), plabels.cex = c(3, 2), which = 1:2)

# on a ADEgS: facets
xy <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1))
posi <- factor(xy$x > 0) : factor(xy$y > 0)
g10 <- s.class(xy, fac = posi, facets = posi, pellipses.col = 1:4, plabels.cex = 0, plegend.drawKey = FALSE, psub.cex = 0, plot = FALSE)
addtext(g10, c(0.5, 0.5, -0.5, -0.5), c(0.5, -0.5), levels(posi), plabels.cex = 2, plabels.col = 1:4)


############################## addline ##############################
# on a 2D plot
g11 <- s.label(cbind(rnorm(100), rnorm(100)), plot = FALSE)
addline(g11, 0, 1, plines = list(col = "red", lwd = 2, lty = 2))
addline(g11, h = 1, plines.col = "chartreuse4", plines.lwd = 3)
addline(g11, v = c(-1, 1), plines.col = "cadetblue", plines.lwd = 3)

# on a 1D plot
g12 <- s1d.label(rnorm(10), plot = FALSE)
addline(g12, v = 1, plines.col = "chartreuse4", plines.lwd = 3)

# on a ADEgS: juxtaposition
g13 <- ADEgS(c(g11, g11), plot = FALSE)
addline(g13, 0, 1, which = 1, plines.col = "red")
addline(g13, 0, 1, which = 2, plines.col = "red")
addline(g13, 0, 1, which = 1:2, plines.col = "red")
addline(g13, 0, 1, plines.col = "red")
addline(g13, h = 1, plines.col = "red")
addline(g13, h = c(1, -1), plines.col = "red")
addline(g13, v = c(-1, 1), plines.col = "red")
addline(g13, v = c(-1, 1), plines.col = 2:3)
addline(g13, c(0.7, -0.5), c(0.2, -0.4), which = 1:2, plines.col = "red")
addline(g13, c(0.7, -0.5), c(0.2, -0.4), which = 1, plines.col = "red")
addline(g13, 0.7, 0.2, which = 1, plines.col = "red")
addline(g13, 0.7, -0.5, which = 1, plines.col = "red")

xy2 <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -2, 2), y2 = runif(200, -0.5, 0.5))
fac2 <- factor(xy2$x > 0) 
g9 <- s.class(xy2, fac = fac2, xax = 1, yax = 2:3, plot = FALSE)
addline(g9, 1, 0, "A", plines.col = "red", plabels.cex = 2)
addline(g9, c(-2.1, -1.07), c(2, 1), plines.col ="red", which = 1:2)
addline(g9, c(-2.1, -1.07), c(2, 1), plines.col = c("green4", "red"))

# on a ADEgS: facets
xy <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1))
posi <- factor(xy$x > 0) : factor(xy$y > 0)
g10 <- s.class(xy, fac = posi, facets = posi, pellipses.col = 1:4, plabels.cex = 0, plegend.drawKey = FALSE, psub.cex = 0, plot = FALSE)
addline(g10, 0, c(0.5, -0.5), plines.col = 1:4)


############################## addpoint ##############################
# on a 2D plot
g11 <- s.label(cbind(rnorm(100), rnorm(100)), ylab = "y axis label", 
               paxes.draw = TRUE, plot = FALSE)
addpoint(g11, 2, 2, ppoints.col = "coral", ppoints.pch = "*", ppoints.cex = 4)
addpoint(g11, c(1, 2), c(1, 2), ppoints.col = "brown2")
addpoint(g11, 1, c(1, 2), ppoints.col = "cyan3")

# on a 1D plot
g12 <- s1d.density(rnorm(1000), paxes.draw = TRUE, plot = FALSE)
addpoint(g12, 2, 0, ppoints.col = "brown4", ppoints.cex = 3)

g12 <- s1d.density(rnorm(1000), plot = FALSE, ylab = "y axis label")
addpoint(g12, 2, 0, ppoints.col = "brown4", ppoints.cex = 3)

# on a ADEgS: juxtaposition
g13 <- ADEgS(c(g11, g11), plot = FALSE)
addpoint(g13, 2, 2, which = 1, ppoints.col = "cyan3")
addpoint(g13, 2, 2, which = 2, ppoints.col = "cyan3")
addpoint(g13, 2, 2, which = 1:2, ppoints.col = "cyan3")
addpoint(g13, 2, 2, ppoints.col = "cyan3")
addpoint(g13, c(1, 2), 2, ppoints.col = "cyan3")
addpoint(g13, 2, c(1, 2), ppoints.col = "cyan3")

xy2 <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -2, 2), y2 = runif(200, -0.5, 0.5))
fac2 <- factor(xy2$x > 0) 
g9 <- s.class(xy2, fac = fac2, xax = 1, yax = 2:3, plot = FALSE)
addpoint(g9, 1, 0, ppoints.col = "red", ppoints.cex = 2)
addpoint(g9, c(1, -1), 0, ppoints.col = "red", ppoints.cex = 2)
addpoint(g9, 0, c(1, -1), ppoints.col = "red", ppoints.cex = 2)

# on a ADEgS: facets
xy <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1))
posi <- factor(xy$x > 0) : factor(xy$y > 0)
g10 <- s.class(xy, fac = posi, facets = posi, pellipses.col = 1:4, plabels.cex = 0, plegend.drawKey = FALSE, psub.cex = 0, plot = FALSE)
addpoint(g10, 0, c(0.5, -0.5), ppoints.col = 1:4, ppoints.cex = 3)


############################## addsegment ##############################
# on a 2D plot
g11 <- s.label(cbind(rnorm(100), rnorm(100)), paxes.draw = TRUE, plot = FALSE)
addsegment(g11, 0, 2, 0, -2, plines = list(col = "brown2", lwd = 3, lty = 2))
addsegment(g11, c(0, 1), 2, 0, -2, plines = list(col = "brown2", lwd = 3, lty = 2))
addsegment(g11, c(0, 1), 2, c(0, 1), -2, plines = list(col = c("cyan3", "brown2"), lwd = 3, lty = 2))
addsegment(g11, -2, -2, 2, 2, plines = list(col = "brown2", lwd = 3, lty = 2))
addsegment(g11, -2, 2, 2, -2, plines = list(col = "cyan3", lwd = 3, lty = 2))

g12 <- s.label(cbind(rnorm(100), rnorm(100)), ylab = "y axis label", plot = FALSE)
addsegment(g12, 0, 2, 0, -2, plines = list(col = "brown2", lwd = 3, lty = 2))
addsegment(g12, -2, -2, 2, 2, plines = list(col = "brown2", lwd = 3, lty = 2))
addsegment(g12, -2, 2, 2, -2, plines = list(col = "cyan3", lwd = 3, lty = 2))

# on a 1D plot
g13 <- s1d.density(rnorm(1000), paxes.draw = TRUE, ylab = "ylab", plot = FALSE)
addsegment(g13, 2, 0, 2, 0.2, plines.col = 1, plines.lwd = 3)

# on a ADEgS: juxtaposition
g14 <- ADEgS(c(g11, g11), plot = FALSE)
addsegment(g14, 1, 2, 1, -2, which = 1, plines.col = "brown2")
addsegment(g14, 1, 2, 1, -2, which = 2, plines.col = "brown2")
addsegment(g14, c(0, 1), 2, c(0, 1), -2, which = 1:2, plines.col = "brown2")
addsegment(g14, 1, 2, c(0, 1), -2, which = 1:2, plines.col = "brown2")
addsegment(g14, 1, 2, 1, -2, plines.col = "brown2")

xy2 <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -2, 2), y2 = runif(200, -0.5, 0.5))
fac2 <- factor(xy2$x > 0) 
g9 <- s.class(xy2, fac = fac2, xax = 1, yax = 2:3, plot = FALSE)
addsegment(g9, 1, 0, 1, 0.5, plines.col = "red", plines.lwd = 2)
addsegment(g9, 1, -1, 0, 1, plines.col = "red", plines.lwd = 2)
addsegment(g9, 0, 1, -1, 1, plines.col = "red", plines.lwd = 2)

# on a ADEgS: facets
xy <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1))
posi <- factor(xy$x > 0) : factor(xy$y > 0)
g10 <- s.class(xy, fac = posi, facets = posi, pellipses.col = 1:4, plabels.cex = 0, plegend.drawKey = FALSE, psub.cex = 0, plot = FALSE)
addsegment(g10, c(-0.5, 0.5), c(0.5, -0.5), c(-0.5, 0.5), 1, plines.col = 1:4, plines.lwd = 3)
