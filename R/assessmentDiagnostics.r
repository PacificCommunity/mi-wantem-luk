

# data for - effort devs plot


#input_dir   <- "/home/rob/MSE/ofp-sam-mixed-fishery-MSE/BET/assessment/2020/1_TagInt_Hi_20_0.65/"
#frq <- read.MFCLFrq(paste0(input_dir, 'bet.frq'))
#par <- read.MFCLPar(paste0(input_dir, '09.par'), first.yr = range(frq)['minyear'])


getEffortDevs <- function(par, frq){

  realz <- realisations(frq)[order(realisations(frq)$fishery, realisations(frq)$year, realisations(frq)$month),]  
  realz <- cbind(realz, yrqtr = realz$year+(realz$month/12), effort_devs=unlist(effort_dev_coffs(par)))
  
  return(realz)
  
}



#' lenCompFits
#'
#' Plot of fits to length composition data
#'
#' @param lenfit: An object of class MFCLLenFit
#' @param par:    An object of class MFCLPar
#' @param tpo:    An object of class MFCLLikelihood
#' @param fsh:    Numeric fishery identifier (default 1)
#' @param years:  Numeric years to be plotted (default last 8 years)
#'
#'
#' @return A plot of obseved vs predicted length for the selected fishery and year range.
#' 
#' 
#' @export
#' @docType methods
#' @rdname lenfit-methods
#'


## length frequency plot function
lenCompFits <- function(lenfit, par=NULL, tpo=NULL, fsh=1, years=NULL, ...){
  
  if(is.null(years))
    years <- range(lenfit)['maxyear']-c(7:0)  # defaults to last 8 years
  
  lenfitx      <- subset(lenfits(lenfit), fishery==fsh & length==min(length))                   # needed to determine which llvals correspond to yr qtr plot data
  lenfitsub    <- subset(lenfits(lenfit), fishery==fsh & year%in%years)                         # data to be plotted
  lenfitsubsub <- subset(lenfits(lenfit), fishery==fsh & year%in%years & length==min(length))   # needed to determine which llvals correspond to yr qtr plot data
  
  years        <- sort(unique(lenfitsubsub$year))   # if some years are missing data altogether - lattice will drop these
  months       <- sort(unique(lenfitsubsub$month))  # if some qtrs consistently not represented - lattice will drop these
  
  if(!is.null(tpo)){
    sampsize1     <- round(lenfitsubsub$sample_size)
    llvals1      <- length_fish(tpo)[[fsh]][is.element(lenfitx$year+(lenfitx$month/12), lenfitsubsub$year+(lenfitsubsub$month/12))]
    noshows      <- is.element(rep(years, each=length(months))+months/12, lenfitsubsub$year+(lenfitsubsub$month/12))  # identify yr qtrs with not data
  
    sampsize     <- rep(0, length(noshows))                                                       # fill in sample size only for yrqtrs with data
    sampsize[noshows] <- sampsize1
    llvals       <- rep(0, length(noshows))
    llvals[noshows] <- llvals1                                                                    # fill in llvals only for yrqtrs with data
  }
  
  pfunlf <- function(x,y,...){
    panel.xyplot(x,y,...)
    if(!is.null(tpo)){
      panel.text(max(lenfitsub$length)*0.9, max(lenfitsub$obs)*0.95, round(sampsize[panel.number()],2), cex=0.8)
      panel.text(max(lenfitsub$length)*0.9, max(lenfitsub$obs)*0.80, round(llvals[panel.number()],2), cex=0.8)
    }
    if(!is.null(par)){
      panel.abline(v=laa(par, ages=seq(1,dimensions(par)['agecls'], by=4)), col="lightgrey")
    }
  }
  xyplot(obs+pred~length|as.factor(month)*as.character(year), data=lenfitsub,
         xlab="Length", ylab="Frequency", type="l", panel=pfunlf, 
         key=list(space='top',columns=2, lines=list(lty=1, col=c("blue","magenta")), text=list(c("observed","predicted"))), ...)
  
}

#pp  <- read.MFCLPar('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/CC_JH/06.par')
#tpo <- read.MFCLLikelihood('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/CC_JH/test_plot_output')
#lfit<- read.MFCLLenFit2('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/CC_JH/length.fit')

#pp  <- read.MFCLPar('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/run17/06.par')
#tpo <- read.MFCLLikelihood('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/run17/test_plot_output')
#lfit<- read.MFCLLenFit2('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/run17/length.fit')

#lenCompFits(lfit, pp, tpo, fsh=15, years=2010:2014)








