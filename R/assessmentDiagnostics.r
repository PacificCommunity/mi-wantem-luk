

# data for - effort devs plot


#input_dir   <- "/home/rob/MSE/ofp-sam-mixed-fishery-MSE/BET/assessment/2020/1_TagInt_Hi_20_0.65/"
#frq <- read.MFCLFrq(paste0(input_dir, 'bet.frq'))
#par <- read.MFCLPar(paste0(input_dir, '09.par'), first.yr = range(frq)['minyear'])


getEffortDevs <- function(par, frq){

  realz <- realisations(frq)[order(realisations(frq)$fishery, realisations(frq)$year, realisations(frq)$month),]  
  realz <- cbind(realz, yrqtr = realz$year+(realz$month/12), effort_devs=unlist(effort_dev_coffs(par)))
  
  return(realz)
  
}





