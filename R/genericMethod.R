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
setGeneric("panelbase", function(object, x, y) {"panelbase"})

setGeneric("zoom", function(object, zoom, center) {standardGeneric("zoom")}) 
setGeneric("prepare", function(object) {standardGeneric("prepare")})
setGeneric("setlatticecall", function(object) {standardGeneric("setlatticecall")})

## S2 graph
setGeneric("S2.panelbase", function(object, x, y) {standardGeneric("S2.panelbase")})
setGeneric("S2.panel", function(object, x, y, ...) {standardGeneric("S2.panel")})
setGeneric("addhist", function(object, bandwidth, gridsize = 60, kernel = "normal", cbreaks = 2, storeData = FALSE, plot = TRUE, pos = -1, ...) {standardGeneric("addhist")})

#### ADEgraphics method
setGeneric("getpositions", function(object) {standardGeneric("getpositions")})
setGeneric("getstats", function(object) {standardGeneric("getstats")})
setGeneric("superpose", function(g1, g2, which, plot = FALSE) {standardGeneric("superpose")})
setGeneric("printSuperpose", function(g1, refg, position) {standardGeneric("printSuperpose")})
setGeneric("insert", function(graphics, oldgraphics, posi = c("bottomleft", "bottomright", "topleft", "topright"), ratio = 0.2, inset = 0.0, plot = TRUE, which, dispatch = FALSE) {standardGeneric("insert")})

### T graph
setGeneric("T.panelbase", function(object, x, y) {standardGeneric("T.panelbase")})
setGeneric("T.panel", function(object, x, y,...) {standardGeneric("T.panel")})

### Tr graph
setGeneric("Tr.panelbase", function(object, x, y) {standardGeneric("Tr.panelbase")})
setGeneric("Tr.panel", function(object, x, y, ...) {standardGeneric("Tr.panel")})

## S1 graph
setGeneric("S1.panelbase", function(object, x, y) {standardGeneric("S1.panelbase")})
setGeneric("S1.panel", function(object, x, y,...) {standardGeneric("S1.panel")})

## C1 graph
setGeneric("C1.panelbase", function(object, x, y) {standardGeneric("C1.panelbase")})
setGeneric("C1.panel", function(object, x, y, ...) {standardGeneric("C1.panel")})
