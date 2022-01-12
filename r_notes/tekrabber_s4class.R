setClass("TekCorrSet", slots = c(geneSE="SummarizedExperiment",
                                 teSE="SummarizedExperiment"))

setGeneric("geneSE", function(x) standardGeneric("geneSE"))
setGeneric("teSE", function(x) standardGeneric("teSE"))
setMethod("geneSE", "TekCorrSet", function(x) x@geneSE)
setMethod("teSE", "TekCorrSet", function(x) x@teSE)

setMethod("show", "TekCorrSet", function(tecorrset) {
    spec <- unique(colData(geneSE(tecorrset)))
    cat(sprintf("TekCorrSet instance for %s and %s.\n",
                spec[1,1], spec[2,1]))
})

setValidity("TekCorrSet", function(object){
    tst <- all(colnames(geneSE(object)) == colnames(teSE(object)))
    if (!tst) return("colnames of gene and te data are not identical.")
    gcdn <- names(colData(geneSE(object)))
    tcdn <- names(colData(teSE(object)))
    #if (!("Species" %in% gcdn)) return("no 'Species' variable in colData(geneSE)")
    #if (!("Species" %in% tcdn)) return("no 'Species' variable in colData(teSE)")
    tst
})

TekCorrSet <- function(gse, tse) {
    new("TekCorrSet", geneSE=gse, teSE=tse)
}

speciesCorr <- TekCorrSet(speciesCorr$geneCorr, speciesCorr$teCorr)
ctCorr <- TekCorrSet(ctCorr$geneCorr, ctCorr$teCorr)

use_data(speciesCorr, overwrite = TRUE)
use_data(ctCorr, overwrite = TRUE)
