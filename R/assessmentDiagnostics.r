

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
#' @return A plot of observed vs predicted length for the selected fishery and year range.
#' 
#' 
#' @export
#' @docType methods
#' @rdname lenfit-methods
#'


## length frequency plot function
lenCompFits <- function(lenfit, par=NULL, tpo=NULL, fsh=NULL, years=NULL, name=NULL, ...){
  #browser()
  if(is.null(years)){
    years <- range(lenfit)['minyear']:range(lenfit)['maxyear']  
    if(length(years)>6){
      print(paste("year range is", length(years), "plotting only the first 6"))
      years <- years[1:6]
    }
  }
  if(is.null(fsh))
    fsh <- lenfits(lenfit)$fishery[1]  #defaults to potting first fishery only
  
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
    if(length(llvals[noshows]) == length(llvals1))
      llvals[noshows] <- llvals1                                                                    # fill in llvals only for yrqtrs with data
    if(length(llvals[noshows]) != length(llvals1)){
      llvals[noshows] <- NA
      warning("year month range of input files not compatible")
    }
      
  }
  
  xyplot(obs+pred~length|as.factor(month)*as.character(year), data=lenfitsub,
         xlab="Length", ylab="Frequency", type="l", 
         panel=panel.superpose, superpose=T, 
         panel.groups=function(..., group.number, col='lightgrey'){
           if(group.number==1){
             #par.settings=list(superpose.fill=list(col="lightgrey"))
             panel.polygon(..., col='lightgrey')
           }
           else 
             panel.xyplot(...)
           
           if(!is.null(par)){
             panel.abline(v=laa(par, ages=seq(1,dimensions(par)['agecls'], by=4)), col="lightgrey")
           }
           if(!is.null(tpo)){
             panel.text(max(lenfitsub$length)*0.9, max(lenfitsub$obs)*0.95, paste("N =", round(sampsize[panel.number()],0)), cex=0.8)
             panel.text(max(lenfitsub$length)*0.9, max(lenfitsub$obs)*0.80, paste("-LL =", round(llvals[panel.number()],0)), cex=0.8)
           }},
         key=list(space='top',columns=2, lines=list(lty=c(1,1), col=c("black","magenta")), text=list(c("observed","predicted"))), ...)
}

#pp  <- read.MFCLPar('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/CC_JH/06.par')
#tpo <- read.MFCLLikelihood('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/CC_JH/test_plot_output')
#lfit<- read.MFCLLenFit2('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/CC_JH/length.fit')

#pp  <- read.MFCLPar('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/run17/06.par')
#tpo <- read.MFCLLikelihood('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/run17/test_plot_output')
#lfit<- read.MFCLLenFit2('/home/rob/MSE/ofp-sam-albacore_MSE/OMs/CC_grid/run29/length.fit', get_lenage=T)

#lenCompFits(trim(lfit, length=0:100), pp, tpo, fsh=15, years=2010:2014)

#wgtfit <- read.MFCLWgtFit('/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/weight.fit', get_wgtage=T)
#wgtCompFits(wgtfit, pp, tpo, fsh=1, years=2010:2014)


## weight frequency plot function
wgtCompFits <- function(wgtfit, par=NULL, tpo=NULL, fsh=NULL, years=NULL, name=NULL, ...){
  #browser()
  if(is.null(years)){
    years <- range(wgtfit)['minyear']:range(wgtfit)['maxyear']  
    if(length(years)>6){
      print(paste("year range is", length(years), "plotting only the first 6"))
      years <- years[1:6]
    }
  }
  if(is.null(fsh))
    fsh <- wgtfits(wgtfit)$fishery[1]  #defaults to plotting first fishery only
  
  wgtfitx      <- subset(wgtfits(wgtfit), fishery==fsh & weight==min(weight))                   # needed to determine which llvals correspond to yr qtr plot data
  wgtfitsub    <- subset(wgtfits(wgtfit), fishery==fsh & year%in%years)                         # data to be plotted
  wgtfitsubsub <- subset(wgtfits(wgtfit), fishery==fsh & year%in%years & weight==min(weight))   # needed to determine which llvals correspond to yr qtr plot data
  
  years        <- sort(unique(wgtfitsubsub$year))   # if some years are missing data altogether - lattice will drop these
  months       <- sort(unique(wgtfitsubsub$month))  # if some qtrs consistently not represented - lattice will drop these
  
  if(!is.null(tpo)){
    sampsize1     <- round(wgtfitsubsub$sample_size)
    llvals1      <- weight_fish(tpo)[[fsh]][is.element(wgtfitx$year+(wgtfitx$month/12), wgtfitsubsub$year+(wgtfitsubsub$month/12))]
    noshows      <- is.element(rep(years, each=length(months))+months/12, wgtfitsubsub$year+(wgtfitsubsub$month/12))  # identify yr qtrs with not data
    
    sampsize     <- rep(0, length(noshows))                                                       # fill in sample size only for yrqtrs with data
    sampsize[noshows] <- sampsize1
    llvals       <- rep(0, length(noshows))
    if(length(llvals[noshows]) == length(llvals1))
      llvals[noshows] <- llvals1                                                                    # fill in llvals only for yrqtrs with data
    if(length(llvals[noshows]) != length(llvals1)){
      llvals[noshows] <- NA
      warning("year month range of input files not compatible")
    }
    
  }
  
  xyplot(obs+pred~weight|as.factor(month)*as.character(year), data=wgtfitsub,
         xlab="Weight", ylab="Frequency", type="l", 
         panel=panel.superpose, superpose=T, 
         panel.groups=function(..., group.number, col='lightgrey'){
           if(group.number==1){
             #par.settings=list(superpose.fill=list(col="lightgrey"))
             panel.polygon(..., col='lightgrey')
           }
           else 
             panel.xyplot(...)
           
           if(!is.null(par)){
             panel.abline(v=laa(par, ages=seq(1,dimensions(par)['agecls'], by=4)), col="lightgrey")
           }
           if(!is.null(tpo)){
             panel.text(max(wgtfitsub$weight)*0.9, max(wgtfitsub$obs)*0.95, paste("N =", round(sampsize[panel.number()],0)), cex=0.8)
             panel.text(max(wgtfitsub$weight)*0.9, max(wgtfitsub$obs)*0.80, paste("-LL =", round(llvals[panel.number()],0)), cex=0.8)
           }},
         key=list(space='top',columns=2, lines=list(lty=c(1,1), col=c("black","magenta")), text=list(c("observed","predicted"))), ...)
}



condLenAgeFits <- function(lfitx, alk=NULL, ...){
  #browser()
  #merge the lenfit data and the ALK
  ddlfit <- cbind(subset(lenagefits(lfitx), pred>0), dtype='pred')
  colnames(ddlfit)[6] <- 'data'
  ddalk  <- cbind(subset(ALK(alk), obs>0), dtype='obs')
  colnames(ddalk)[7] <- 'data'
  # rescale the ALK to proportions - consistent with lenfit
  tempdatsum <- sum(ddalk$data)  #tapply(ddalk$data, list(ddalk$year, ddalk$month, ddalk$fishery), sum)
  tempdatn   <- nrow(ddalk)      #tapply(ddalk$data, list(ddalk$year, ddalk$month, ddalk$fishery), length)
  ddalk$data <- ddalk$data/tempdatsum
  
  dd     <<- rbind(ddlfit[,c('year','month','fishery','age','length','data','dtype')],
                  ddalk[,c('year','month','fishery','age','length','data','dtype')])
  
  ll <- sort(unique(dd$length))
  aa <- seq(range(unique(dd$age))[1], range(unique(dd$age))[2], length=length(ll))
  
  par(mfrow=c(length(unique(dd$year)), length(unique(dd$month))), mar=c(0,0,0,0), omi=c(0.8,0.8,0.5,0.5))
  
  for(yy in sort(unique(dd$year))){
    for(mm in sort(unique(dd$month))){
      if(mm==min(sort(unique(dd$month))) & yy<max(sort(unique(dd$year))))  # graphics settings for left column plots
        plot(ll,aa, type='n', xaxt='n', xlab='', ylab='')
      if(mm>min(sort(unique(dd$month))) & yy==max(sort(unique(dd$year))))  # graphics settings for bottom row plots
        plot(ll,aa, type='n', yaxt='n', xlab='', ylab='')
      if(mm==min(sort(unique(dd$month))) & yy==max(sort(unique(dd$year))))  # graphics settings for bottom left corner
        plot(ll,aa, type='n', xlab='', ylab='')
      if(mm>min(sort(unique(dd$month))) & yy<max(sort(unique(dd$year))))  # graphics settings for all others
        plot(ll,aa, type='n', xaxt='n', yaxt='n', xlab='', ylab='')
      
      text(max(ll)*0.25, max(aa)*0.9, paste('Year =', yy))
      text(max(ll)*0.25, max(aa)*0.75, paste('Month =', mm))
        if(nrow(subset(dd, year==yy & month==mm & dtype=='pred'))>0)  
          symbols(subset(dd, year==yy & month==mm & dtype=='pred')$length, subset(dd, year==yy & month==mm & dtype=='pred')$age, 
                circles=subset(dd, year==yy & month==mm & dtype=='pred')$data, inches=0.1, fg='cyan', add=T)
        if(nrow(subset(dd, year==yy & month==mm & dtype=='obs'))>0)
          symbols(subset(dd, year==yy & month==mm & dtype=='obs')$length, subset(dd, year==yy & month==mm & dtype=='obs')$age, 
                circles=subset(dd, year==yy & month==mm & dtype=='obs')$data, inches=0.1, fg='blue', add=T)
        lines(c(laa(lfitx)), c(as.numeric(dimnames(laa(lfitx))$age))/4)
      }
  }
  #mtext('Length', side=1)
  #mtext('Age',    side=2) 
  
}


#condLenAgeFits(trim(lfityft, fishery=16, year=2010:2019), trim(alkyft, fishery=16, year=2010:2019))






## Not really working - switching to a non-lattice approach

#condLenAgeFitsX <- function(lfit, alk=NULL, ...){
  #browser()
  #merge the lenfit data and the ALK
#  ddlfit <- cbind(subset(lenagefits(lfit), pred>0), dtype='pred', col='red')
#  colnames(ddlfit)[6] <- 'data'
#  ddalk  <- cbind(subset(ALK(alk), obs>0), dtype='obs', col='blue')
#  colnames(ddalk)[7] <- 'data'
  # rescale the ALK to proportions - consistent with lenfit
#  tempdatsum <- tapply(ddalk$data, list(ddalk$year, ddalk$month, ddalk$fishery), sum)
#  tempdatn   <- tapply(ddalk$data, list(ddalk$year, ddalk$month, ddalk$fishery), length)
#  ddalk$data <- ddalk$data/rep(tempdatsum, tempdatn)/10
  
#  dd     <- rbind(ddlfit[,c('year','month','fishery','age','length','data','dtype','col')],
#                  ddalk[,c('year','month','fishery','age','length','data','dtype','col')])
  
#  cc <- lapply(unique(dd$month), function(x){subset(dd, month==x)$data})
  
#  pxfun <- function(x,y, pancex=pancex, ...){      
    #browser()
#    panel.xyplot(x,y, cex=pancex[[panel.number()]], ...)
#    panel.lines(c(laa(lfit)), as.numeric(dimnames(laa(lfit))$age)/4, col="black")
#  }
  
#  xyplot(age~length|as.factor(month)*as.character(year), group=dtype, data=dd, pch=1, col=c('red','blue'), 
#         pancex=list(c(4,1),c(c(8,2,3,4,5,6,7,8),2),c(1,4),c(2,5)),
#         panel=pxfun)
#         }


#lfityft <- read.MFCLLenFit2('/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/length.fit', get_lenage=T)
#alkyft  <- read.MFCLALK('/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/yft.age_length', lenfit=lfityft)
#condLenAgeFits(trim(lfityft, fishery=16, year=2010), trim(alkyft, fishery=16, year=2010))





