library(grid)
t <- read.csv("gargsVSclass.csv", sep = ",", header = TRUE, check.names = FALSE)
row.names(t) <- t[, 1]
t <- t[, -1]
t[is.na(t)] <- 0

adegpar("ptable" = list(x = list(tck = unit(5, "mm")), y = list(tck = unit(5, "mm"))), pgrid.draw = TRUE)
table.value(t, plegend.drawKey = FALSE, ppoints.cex = 0.2, symbol = "circle", axis.text = list(cex = 0.7), ptable.y = list(srt = 45, adj = c(0., 0.), margin = c(2, 15)), ptable.x = list(pos = "left", margin = c(15, 5)))



