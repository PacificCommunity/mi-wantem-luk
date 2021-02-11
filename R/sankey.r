
#frq  <- read.MFCLFrq("/media/penguin/skj/2019/assessment/Diagnostic/skj.frq")
#tag  <- read.MFCLTag("/media/penguin/skj/2019/assessment/Diagnostic/skj.tag")
#par <- read.MFCLRegion("/media/penguin/skj/2019/assessment/Diagnostic/07.par")



#' sankeyMovePlot
#'
#' Produces a Sankey plot of movement 
#'
#' @param obj:    An object either of class MFCLRegion or of class MFCLTag
#' @param frq:    An object of MFCLFrq. Defaults to NULL but required if obj=MFCLTag
#'
#'
#' @return A plot of movement between assessment regions as determined either by the tag data or the estimated movement from the assessment.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('sankeyMovePlot', function(obj, ...) standardGeneric('sankeyMovePlot')) 


setMethod("sankeyMovePlot", signature(obj="MFCLRegion"), 
          function(obj, ...){
            par <- obj
            
            data_long <- data.frame(source=as.numeric(dimnames(diff_coffs_age_period(par))$from),
                                    target=rep(as.numeric(dimnames(diff_coffs_age_period(par))$to), 
                                               each=length(dimnames(diff_coffs_age_period(par))$from)),
                                    value =c(diff_coffs_age_period(par)[,,15,1]))
            
            data_long$target <- paste(data_long$target, " ", sep="")
            
            nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
            
            data_long$IDsource=match(data_long$source, nodes$name)-1 
            data_long$IDtarget=match(data_long$target, nodes$name)-1
            
            # prepare colour scale
            cols <- paste(colorRampPalette(c("darkslategrey","lightblue"))(dim(diff_coffs_age_period(par))[1]), collapse=",")
            ColourScal <- paste("d3.scaleOrdinal() .range(['", paste(unlist(strsplit(cols, split=",")), collapse="','"), "'])", sep="")
            
            # Make the Network
            sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name", 
                          sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
            
            
            
          })


setMethod("sankeyMovePlot", signature(obj="MFCLTag"), 
          function(obj, frq=NULL, ...){

            tag <- obj
            if(is.null(frq))
              stop("A 'frq' file must also be provided")
            
            fsh.reg.map <- data.frame(recap.fishery=1:n_fisheries(frq), recap.region=c(aperm(region_fish(frq), c(3,1,2,4,5,6))))
            
            data <- merge(recaptures(tag), fsh.reg.map)
            #data <- merge(subset(recaptures(tag), program=="JPTP"), fsh.reg.map)
            
            data <- tapply(data$recap.number, list(data$region, data$recap.region), sum)
            data[is.na(data)] <- 0
            
            data_long <- data.frame(source=rownames(data), target=rep(colnames(data), each=length(rownames(data))), value=c(data))
            
            
            data_long$target <- paste(data_long$target, " ", sep="")
            
            nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
            
            data_long$IDsource=match(data_long$source, nodes$name)-1 
            data_long$IDtarget=match(data_long$target, nodes$name)-1
            
            # prepare colour scale
            cols <- paste(colorRampPalette(c("darkslategrey","lightblue"))(n_regions(frq)), collapse=",")
            ColourScal <- paste("d3.scaleOrdinal() .range(['", paste(unlist(strsplit(cols, split=",")), collapse="','"), "'])", sep="")
            
            # Make the Network
            sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name", 
                          sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
            
            

          })



