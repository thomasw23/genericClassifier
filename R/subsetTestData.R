#'Subset classifier probe RMA values from Test Samples 
#'
#'\code{subsetTestData} returns a data frame with rows equal to the number of 
#'samples and columns equal to the number of classifier probes
#'
#'This function loads the list of classifier probes takes only that subset of 
#' RMA values in the test samples. 
#'@param d data frame of normalized RMA values for test samples
#'@param feat character vector specifying classifier probesets 
#'@return data.frame
#'@examples 
#'\dontrun{
#'t <- c("PAX")
#'data_file <- paste0("txClassifier" , t)
#'subsetTestData(data, colnames(data_file[[1]][,-1]), t)
#'} 

subsetTestData <- function(d, feat) {
  d2 <- d[, colnames(d) %in% feat]
  return(d2)
}	 
