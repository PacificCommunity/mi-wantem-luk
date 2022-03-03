
#frq  <- read.MFCLFrq("/media/penguin/skj/2019/assessment/Diagnostic/skj.frq")
#tag  <- read.MFCLTag("/media/penguin/skj/2019/assessment/Diagnostic/skj.tag")
#par <- read.MFCLRegion("/media/penguin/skj/2019/assessment/Diagnostic/07.par")



#' chordMovePlot
#'
#' Produces a Chord plot of movement 
#'
#' @param obj:    An object either of class MFCLRegion or of class MFCLTag
#' @param frq:    An object of MFCLFrq. Defaults to NULL but required if obj=MFCLTag
#' @param age:    By default the first age is plotted
#' @param season: By default the first season is plotted
#'
#'
#' @return A plot of movement between assessment regions as determined either by the tag data or the estimated movement from the assessment.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('chordMovePlot', function(obj, ...) standardGeneric('chordMovePlot')) 


setMethod("chordMovePlot", signature(obj="MFCLRegion"), 
          function(obj, age=1, season=1, ...){

            par <- obj
            args <- list(...)
            
            m       <- diff_coffs_age_period(par)[,,age,season]
            regions <- paste("Region", 1:dim(m)[1])
            
            
            dimnames(m) <- list(from=regions, to=regions)
            #groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223","#000000", "#FFDD89", "#957244", "#F26223")
            rnd <- sample(1:dim(diff_coffs_age_period(par))[1], dim(diff_coffs_age_period(par))[1], replace=F)
            rnd <-        1:dim(diff_coffs_age_period(par))[1]
            groupColors <- colorRampPalette(c("goldenrod","brown","beige"))(dim(diff_coffs_age_period(par))[1])[rnd]
            #groupColors <- viridis(dim(diff_coffs_age_period(par))[1], option="D")
            
            p <- chorddiag(m, groupColors=groupColors, groupnamePadding=40, ...)
            p
          })


setMethod("chordMovePlot", signature(obj="MFCLTag"), 
          function(obj, frq=NULL, program=NULL, ...){

            tag <- obj
            if(is.null(frq))
              stop("A 'frq' file must also be provided")
            
            fsh.reg.map <- data.frame(recap.fishery=1:n_fisheries(frq), recap.region=c(aperm(region_fish(frq), c(3,1,2,4,5,6))))
            
            data <- merge(recaptures(tag), fsh.reg.map)
            
            if(!is.null(program)) 
              data <- merge(recaptures(tag)[recaptures(tag)$program==program,], fsh.reg.map)
            
            
            m <- tapply(data$recap.number, list(data$region, data$recap.region), sum)
            m[is.na(m)] <- 0
            
            regions <- paste("Region", 1:dim(m)[1])
            
            
            dimnames(m) <- list(from=regions, to=regions)
            #groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223","#000000", "#FFDD89", "#957244", "#F26223")
            #rnd <- sample(1:dim(diff_coffs_age_period(par))[1], dim(diff_coffs_age_period(par))[1], replace=F)
            groupColors <- colorRampPalette(c("goldenrod","brown","beige"))(dim(m)[1])
            #groupColors <- viridis(dim(diff_coffs_age_period(par))[1], option="D")
            
            p <- chorddiag(m, groupColors=groupColors, groupnamePadding=40, ...)
            p
          })



