#'Subset Training Set Data by Classifier Type
#'
#'\code{subsetComparison} returns a data frame containing only rows with phenotypes
#'corresponding to the specified classifier.
#'
#'This function takes a training set data frame and removes all rows (samples) that 
#'do not have a phenotype associated with the classifier in the specified comparison.
#'@param data_set data frame where rows are samples, column 1 is the
#'sample phenotype (i.e. C, A, or B), and the remaining columns are discretized
#'values for the classifier probesets
#'@param comp vector of 2 elements that specifies the groups of samples to keep in the 
#'frame.
#'@return data frame
#'@examples 
#'\dontrun{
#'datasubset <- subsetComparison(df, c("A", "C"))
#'}

subsetComparison <- function(data_set, comp) {
 
  pheno <- data_set[,1]
  ## subset only the data that falls into the two pheno groups
  grp.numPhenos <- match(pheno, comp)
  grp.pheno <- factor(as.character(pheno[!(is.na(grp.numPhenos))]), ordered=TRUE)
  grp.data <- data_set[!(is.na(grp.numPhenos)),]
  grp.data$pheno <- grp.pheno
  if(length(unique(grp.data$pheno)) < 2) stop(paste("Data does not contain the 2 groups specified for comparison:", 
                                                    comp[1], comp[2],sep=" "))
  return(grp.data)
}

#'Select Features using MRMR
#'
#'\code{selectFeaturesMRMR} returns an object from the MRMR.ensemble function based on
#'the inputs provided by the function \code{\link{scanClassifierRange}}. 
#'
#'This function is called by \code{\link{createClassifier}} or 
#'\code{\link{scanClassifierRange}}. It is a wrapper for calling 
#'\code{\link[mRMRe]{mRMR.data}} and \code{\link[mRMRe]{mRMR.ensemble}}  
#'@param data_subset data frame of RMA values where samples are columns and 
#'probesets as rows for the subset of training set samples in the specified comparison.
#'@param feat_count integer that specifies the feature set size to be tested. 
#'@inheritParams createClassifier 
#'@return mrmr.ensemble object
#'@examples
#'\dontrun{
#'func_output_1 <- selectFeaturesMRMR(data, feat_count=100)
#'}  
selectFeaturesMRMR <- function(data_subset, feat_count) {
  
  mrmr_input <- mRMRe::mRMR.data(data=data_subset)
  predFeatSet <- mRMRe::mRMR.ensemble(data=mrmr_input,
                                      target_indices=c(1), 
                                      feature_count=feat_count,, 
                                      solution_count=1,
                                      method="bootstrap")
  
  pgd <- data_subset[,predFeatSet@filters[[1]][,1]]
  if (sum(unique(pgd[,1]) %in% unique(data_subset$pheno)) != 0) {
    message(paste("MRMR could not identify feature set of size:", feat_count, sep=" "))
  }
  return(predFeatSet)
}
