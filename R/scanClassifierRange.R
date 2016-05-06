#'Scan a Range of Feature Set Sizes for a Specific Classifier
#'
#'\code{scanClassifierRange} returns a list of 3 elements: 1) list of classifier
#'probeset ids; 2) numeric vector of classifier accuracies: 3) list of classifier 
#'probesets training data. 
#'
#'This function is called by \code{\link{createClassifier}} when the option 
#''typeK' is equal to "range". It is a wrapper for calling 
#'\code{\link{selectFeaturesMRMR}} for each value in the specified range and then 
#'calling \code{\link{plotRangeQC}}. 
#'@param data_subset data frame of RMA values where samples are columns and 
#'probesets as rows for the subset of training set samples in the specified comparison. 
#'@param plotQC logical to generate a plot of the accuracies for the specified range of
#'feature set sizes (default: TRUE). 
#'@inheritParams createClassifier 
#'@return list
#'@examples
#'\dontrun{
#'func_output_1 <- scanClassifierRange(data_subset=data, comp_type=c("AR","TX"),
#'                                      range_pars=c("full", 90, 110, 2))
#'} 

scanClassifierRange <- function(data_subset, range_pars, comp_type, type=type, plotQC=TRUE, ...) {
  
  dots <- list(...)
  # range_pars: range parameter vector - 1: # of iterations for the LOOCV
  #                              2: lowest feature set size to test
  #                              3: highest feature set size to test
  #                              4: increment (optional)
  
  if(length(range_pars) < 5) {
    if (as.numeric(range_pars[2]) <= as.numeric(range_pars[3])) {
      if (is.na(range_pars[4]) | !(as.numeric(range_pars[4]) > 0) ) {
        kRange <- seq(as.numeric(range_pars[2]), as.numeric(range_pars[3]), 1)    
      } else {
        kRange <- seq(as.numeric(range_pars[2]), as.numeric(range_pars[3]), as.numeric(range_pars[4]))
      }
    } else {
      stop("Feature set size range is negative!")
    }
  } else {
    stop("Need to provide a range of feature set sizes: numKstart & numKend")
  } 
  
  message("Feature Sizes to Test:")
  if (is.na(range_pars[4])) {
    message(paste(range_pars[2],range_pars[3],sep=" to "))    
  } else {
    message(sprintf("%s to %s, incremented by %s", range_pars[2],range_pars[3], range_pars[4]))
  }
    
  predFeatData <- selectFeaturesMRMR(data_subset, 
                                     feat_count=max(kRange))
  predGenesData <- data_subset[,predFeatData@filters[[1]][,1]]
  svmargs <- c("svm_cost", "svm_gamma")
  
  if (sum(unique(predGenesData[,1]) %in% unique(data_subset$pheno)) == 0) {
    
    cv <- unlist(lapply(kRange, function(k) { 
      do.call(crossValidateClassifier, c(list(df=cbind(pheno=data_subset$pheno, 
                                                       predGenesData[,(max(kRange) - (k - 1)):max(kRange)]),
                                              num_iter=range_pars[1]),  dots[names(dots) %in% svmargs]))
    }))
    
    predFeatDataList <- lapply(kRange, function(k) { 
      do.call(txClassifier, c(list(classes=comp_type, type=type, 
                                   data_set=cbind(pheno=data_subset$pheno, 
                                                  predGenesData[,(max(kRange) - (k - 1)):max(kRange)])), 
                              dots[names(dots) %in% svmargs]))
    })    
    
    predFeatList <- lapply(kRange, function(k) { 
      colnames(predGenesData)[(max(kRange) - (k - 1)):max(kRange)]
    })  
    names(cv) <- names(predFeatDataList) <- names(predFeatList) <- as.character(kRange)
    
  } else {
    predFeatDataList <- list()
    predFeatList <- list()
    cv <- rep("NA", length(kRange))
  }
  
  out <- list(predFeatList, cv, predFeatDataList)
  cvacc <- out[[2]]
  
  plotRangeQC(data.frame(fss=kRange, acc=cvacc), 
              "pts", 
              comp_type, 
              dim(data_subset)[1], 
              dim(data_subset)[2])
  
  return(out)
}
