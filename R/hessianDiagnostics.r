

## Hessian diagnostic functions
## December 2021



negEigenSummary <- function(eigen_report="new_cor_report", varnames="xinit.rpt"){
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  # read new_cor_report
  ncr <- readLines(eigen_report)
  
  # read in the varnames - with fix to sort out the missing '_" in grouped catch_dev_coffs
  vars <- readLines(varnames)
  rows <- grep('grouped', vars)
  for(rr in rows){
    temp <- unlist(strsplit(vars[rr], split="[[:blank:]]+"))
    vars[rr] <- paste(temp[1], " ", temp[2], "_", temp[3], sep="")
  }
  
  vars <- matrix(unlist(strsplit(vars, split="[[:blank:]]+")), ncol=2, byrow=T)
  vars.df <- data.frame(varnum=as.numeric(vars[,1]), varname=vars[,2])
  
  
  eigen.val  <- as.numeric(splitter(ncr, "Smallest", 1, 1)[1])
  eigen.vars <- regmatches(ncr[2], gregexpr("(?<=\\().*?(?=\\))", ncr[2], perl=T))[[1]]
  
  eigen.vars    <- matrix(as.numeric(unlist(strsplit(eigen.vars, split="[[:blank:]]+"))), ncol=2, byrow=T)
  eigen.vars.df <- data.frame(varnum=eigen.vars[,1], contrib=eigen.vars[,2])
  
  eigen.vars.df <- merge(eigen.vars.df, vars.df)
  eigen.vars.df <- eigen.vars.df[rev(order(abs(eigen.vars.df$contrib))),]
  
  eigen.cols.df <- data.frame(varname=unique(eigen.vars.df$varname), varcol=viridis(length(unique(eigen.vars.df$varname))))
  eigen.vars.df <- merge(eigen.vars.df, eigen.cols.df)
  eigen.vars.df <- eigen.vars.df[rev(order(abs(eigen.vars.df$contrib))),]
  
  return(eigen.vars.df)
}


## testing
#path <- '/home/rob/MSE/ofp-sam-skipjack_MSE/condor/hessian_split/pd_hessian'
#eigencontribs <- negEigenSummary(eigen_report = paste(path, 'new_cor_report', sep="/"),
#                                 varnames     = paste(path, 'xinit.rpt', sep="/"))

#barplot(eigencontribs$contrib, names.arg = eigencontribs$varname, las=2, col=eigencontribs$varcol)

#barplot(eigencontribs$contrib, col=eigencontribs$varcol)
#legend(300, 0.3, legend=unique(eigencontribs$varname), fill=viridis(3), bty='n')
