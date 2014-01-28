###################################################
## Alice Julien-Laferriere                      ###
## definition of generic methods                ###
## TODO: lock binding (cf Genolini S4 )         ###
###################################################

setGeneric("getparameters", function(object, number) {standardGeneric("getparameters")})
setGeneric("getlatticecall", function(object, number) {standardGeneric("getlatticecall")})
setGeneric("gettrellis", function(object) {standardGeneric("gettrellis")})
setGeneric("getcall", function(object) {standardGeneric("getcall")})
setGeneric("getgraphics", function(object) {standardGeneric("getgraphics")})
setGeneric("add.ADEg", function(object) {standardGeneric("add.ADEg")})
setGeneric("panel", function(object, x, y, ...) {standardGeneric("panel")})
setGeneric("panelbase", function(object, x, y) {"panelbase"})

setGeneric("zoom", function(object, zoom, center) {standardGeneric("zoom")}) 
setGeneric("prepare", function(object) {standardGeneric("prepare")})
setGeneric("setlatticecall", function(object) {standardGeneric("setlatticecall")})

setGeneric("addhist", function(object, bandwidth, gridsize = 60, kernel = "normal", cbreaks = 2, storeData = FALSE, plot = TRUE, pos = -1, ...) {standardGeneric("addhist")})

setGeneric("getpositions", function(object) {standardGeneric("getpositions")})
setGeneric("getstats", function(object) {standardGeneric("getstats")})
setGeneric("superpose", function(g1, g2, which, plot = FALSE) {standardGeneric("superpose")})
setGeneric("printSuperpose", function(g1, refg, position) {standardGeneric("printSuperpose")})
setGeneric("insert", function(graphics, oldgraphics, posi = c("bottomleft", "bottomright", "topleft", "topright"), ratio = 0.2, inset = 0.0, plot = TRUE, which, dispatch = FALSE) {standardGeneric("insert")})

