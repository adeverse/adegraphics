graphics.off()
library(adegraphics)

## ex1
data(olympic, package = "ade4")
tab1 <- data.frame(scale(olympic$tab))
pca <- ade4::dudi.pca(tab1, scann = FALSE)
g1 <- table.value(tab1, maxsi = 0.5, axis.line = list(col = "blue"), axis.text = list(col = "grey"))
g2 <- table.value(tab1, maxsi = 0.5, x = c(1:5, 10:6))
g3 <- table.value(tab1, maxsi = 0.5, x = c(1:5, 10:8))
g4 <- table.value(tab1, y = rank(pca$li[, 1]), x = rank(pca$co[, 1]), method = "color")
g5 <- table.value(tab1, y = pca$li[, 1], x = pca$co[, 1], ptable = list(x = list(srt = 90)))

## ex2
data(eurodist)
g6 <- table.value(eurodist, store = TRUE, symbol = "circle", maxsi = 1)
g7 <- table.value(eurodist, maxsi = 0.5, store = FALSE, psub.text = "eurodist", psub.position = c(0, -0.04))

## ex3
data("doubs", package = "ade4")
tab2 <- as.table(as.matrix(doubs$fish))
g8 <- table.value(tab2)

## ex4
data(chats, package = "ade4")
tab3 <- as.table(as.matrix(data.frame(t(chats))))
coa1 <- ade4::dudi.coa(data.frame(t(chats)), scann = FALSE)
adegpar(ptable = list(x = list(pos = "bottom", srt = 0), y = list(pos = "left")), plegend.draw = FALSE)
g9 <- table.value(tab3, meanX = TRUE, ablineX = TRUE, maxsize = 2)
g10 <- table.value(tab3, meanY = TRUE, ablineY = TRUE, maxsize = 2) 
g11 <- table.value(tab3, x = coa1$c1[, 1], y = coa1$l1[, 1], meanX = TRUE, maxsize = 2, ablineX = TRUE)
g12 <- table.value(tab3, x = coa1$c1[, 1], y = coa1$l1[, 1], meanY = TRUE, maxsize = 2, ablineY = TRUE)
g13 <- ADEgS(list(g9, g10, g11, g12), pos = rbind(c(0, 0.5, 0.5, 1), c(0.5, 0.5, 1, 1), c(0, 0, 0.5, 0.5), c(0.5, 0, 1, 0.5)))

## ex5
data(rpjdl, package = "ade4")
tab4 <- data.frame(t(rpjdl$fau))
coa2 <- ade4::dudi.coa(tab4, scann = FALSE)
g14 <- table.value(tab4, x = coa2$c1[, 1], y = rank(coa2$l1[, 1]), maxsize = 0.2, axis.text = list(cex = 0), labelsy = rpjdl$lalab, plot = F)

## ex6
tab5 <- as.table(matrix(rep(0, 100), 10))
tab5[1:5, 1:5] <- 10
ade4::table.cont(tab5, abmean.x = T, y = 10:1)
g15 <- table.value(tab5, y = 10:1, meanX = T)
g16 <- table.value(tab5, y = 10:1, meanX = T, meanY = TRUE, ablineX = TRUE, ablineY = TRUE)

## ex7
tab6 <- matrix(rep(0, 100), 10)
tab6[1:5, 1:5] <- 20
colnames(tab6) <- LETTERS[1:10]
rownames(tab6) <- LETTERS[1:10]
ade4::table.value(tab6, x = 1:10, y = 10:1)
g17 <- table.value(tab6, x = 1:10, y = 10:1)
g18 <- table.value(tab6, x = 1:10, y = c(1, 2, 5, 6, 8, 9, 10, 3, 4, 7))

