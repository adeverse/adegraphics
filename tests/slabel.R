graphics.off()
library(adegraphics)
library(grid)

x0 <- runif(50, -2, 2)
y0 <- runif(50, -2, 2)
z <- x0 ^ 2 + y0 ^ 2
dfxy <- data.frame(x0, y0)
g1 <- s.label(dfxy, label = as.character(z < 1), paxes.draw = TRUE, axis.text = list(col = "grey"))
g2 <- s.label(dfxy, labels = c("", "MM", "", NA, "ooo"), plabels.optim = TRUE)
g3 <- s.label(dfxy, labels = as.character(z < 1), psub = list(text = "Subtitle", col = "blue", position = "topleft"), plabels.col = 1:5, pgrid.text.pos = c(unit(0.95, "npc"), unit(0.94, "npc")))

dfxy <- cbind(dfxy, runif(50, -5, 5))
g4 <- s.label(dfxy, xax = 1, yax = 2:3, paxes.draw = TRUE, paxes.aspectratio = 1.5, plabels.cex = 0.8)

l <- ls()
x1 <- runif(length(l))
x2 <- runif(100)
y1 <- runif(length(l))
y2 <- runif(100)
g5 <- s.label(cbind(x2, y2), labels = as.character((x2 * x2 + y2 * y2) < 1))
g6 <- s.label(cbind(x1, y1), labels = l, add = TRUE, plabels.col = "blue")

