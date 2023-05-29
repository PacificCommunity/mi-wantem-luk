#mi-wantem-luk - data visualisaton stuff
#Copyright (C) 2018  Rob Scott

#' plot
#'
#' Plot MFCL objects.
#'
#' @param obj An object of class MFCLX.
#' @param \dots Additional argument list that might not ever be used.
#'
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' \dontrun{
#' plot(MFCLFrq())
#' }
#'
#' @aliases mfcl

setMethod("plot", signature(x="MFCLLenFit"), function(x, y="missing", ...){
  #browser()
  args <- list(...)
  
  if(any(is.element(lapply(args, class), "MFCLLikelihood"))){
    ppx <- lenCompFits(x, y, ...)
    plot(ppx)
  }
  if(any(is.element(lapply(args, class), "MFCLALK"))){
    ppx <- condLenAgeFits(x, y, ...)
    plot(ppx)
  }
})

#plot(trim(lfit, length=0:100), pp, tpo, fsh=15, years=2010:2014)

#plot(trim(lfityft, fishery=16, year=2010:2019), trim(alkyft, fishery=16, year=2010:2019))

setMethod("plot", signature(x="MFCLRegion"), function(x, y="missing", ...){
  #browser()
  args <- list(...)
 
  if(args$type=="Sankey"){
    sankeyMovePlot(x, ...)
  }
  if(args$type=="Chord"){
    chordMovePlot(x, ...)
  }
})
  


setMethod("plot", signature(x="MFCLTag"), function(x, y="missing", ...){
  #browser()
  args <- list(...)
  
  chordMovePlot(x, ...)
  
})


