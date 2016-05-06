# Search in the package's data directory for the previous version of the the data files
# Calls to this function during training will create a new data structure if no classifier exists
# Calls to this function during testing will return "FALSE" if no classifier exists 
#'Test for Existing Classifier
#'
#'\code{testForExistingClassifier} returns a vector of boolean values
#'
#'This function checks for the existence of a loaded list of classifier objects. If no object is
#' loaded, then the 'txClassifierPAX' dataset from the diagnoseTx package 
#'is loaded. Next, the presence of classifier probesets in each of the slots 
#'in the list is tested. To test new samples, all classifiers must be present.
#'@param classifier_name_base character specifying beginning of the name of the data file 
#'containing the feature set for each classifier. This value will be appended with the type
#' to make the complete filename of the classifier data object to be loaded (default: txClassifier).  
#'@param classifier_name character specifying the name of the data file containing the feature 
#'set for each classifier. If this value is NULL, then the classifier_name_base and type will
#'be used to create a filename on which to search and load the classifier object (default: NULL).
#'@inheritParams loadClassifier 
#'@return 3-length vector of class types
#'@rdname testForExistingClassifier
#'@export
#'@examples 
#'type <- c("PAX")
#'testForExistingClassifier(type)

testForExistingClassifier <- function(type="PAX", classifier_name_base="txClassifier", classifier_name=NULL) {
  
  if(is.null(classifier_name)) {
      classifier_name <- paste0(classifier_name_base, type)
  }
  
  if(exists(classifier_name, inherits=FALSE, envir=globalenv())) { 
    message(sprintf("Classifier probesets already loaded into %s", classifier_name))
  } else {
    data(list=c(classifier_name))
  }
  
  if (length(get(classifier_name)) == 0) {
    message("Classifier object is empty!")
    classStatus <- NA
  } else {
    classStatus <- sapply(1:length(get(classifier_name)), function(x) {
      if(is.null(get(classifier_name)[[x]])) {
        NA
      } else {
        message(paste0("Classifier ", x, " includes: ", names(get(classifier_name))[x]))
        if (class(get(classifier_name)[[x]]) == "data.frame") {
          "data.frame"
        } else if (class(get(classifier_name)[[x]]) == "txClassifier") {
          "txClassifier"
        } else {
          class(get(classifier_name)[[x]])
        }
      }  
    })
  }
  names(classStatus)[1] <- classifier_name
  
  return(classStatus)  
}
