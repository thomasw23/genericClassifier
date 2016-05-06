#'txClassifier Constructor Function
#'
#'\code{\link{txClassifier}} returns an object of class txClassifier. 
#'
#'This function is a constructor for building a txClassifier object. The txClassifier 
#'object is a list of parameters for making and using a trained svm model based on 
#'gene expression data from microarrays.
#'
#'@param classes character vector of 2 elements that specifies the groups of samples to
#' keep in the data frame
#'@param data_set data frame of RMA values for training set with samples as columns and
#' probesets as rows (default: NULL).
#'@param svm_cost numeric value representing the cost parameter when training an svm model 
#'(default: 1)
#'@param svm_gamma numermic value (or "def") representingthe gamma parameter when training 
#'an svm model (default: "def") 
#'@param type character of value "PAX" or "BX" specifying the type of samples to be
#'tested (default: PAX).
#'@return object of class txClassifier
#'@examples 
#'\dontrun{
#'txClassifier <- txClassifier(classes=c("notTX", "TX"), data_set="", sample_labels="", 
#'                              svm_cost=1, svm_gamma=0.05, type="PAX")
#'}


txClassifier <- function(classes, data_set, svm_cost=1, svm_gamma="def", type="PAX")
{
  if(length(classes)!= 2) stop("Exactly 2 classes needed for classifier!")
  if(colnames(data_set)[1] != "pheno" | sum(length(unique(data_set[,1]))) != 2) {
    stop("The training data set is formatted incorrectly")
  }
  
  feat <- length(colnames(data_set)[-1])
  if(svm_gamma == "def") {
    svm_gamma <- 1/feat
  } else if (!is.numeric(svm_gamma)) {
    stop("Incorrect'gamma' value: must be numeric!")
  } else {
    message(sprintf("SVM paramters: cost - %s, gamma - %s", svm_cost, svm_gamma))
  }
  model <- e1071::svm(pheno ~ ., data=data_set[order(data_set$pheno, decreasing=TRUE),], probability=TRUE, 
                      cost=svm_cost, gamma=svm_gamma)
  
  traincalls <- e1071:::predict.svm(model, data_set[order(data_set$pheno, decreasing=TRUE),], 
                                    decision.values=TRUE, probabilities=TRUE)
  trainscores <- attr(traincalls, "decision.values")[,1]
  min <- min(trainscores) 
  max <- max(trainscores)
  
  acc <- crossValidateClassifier(df=data_set, num_iter="full", 
                                 svm_cost=svm_cost, svm_gamma=svm_gamma)
  
  me <- list(
    type=type,
    classes=classes,
    data=data_set,
    probes=colnames(data_set)[-1], 
    svm.model=model,
    cost=svm_cost,
    gamma=svm_gamma,
    acc=acc,
    max_score=max,
    min_score=min
  )
  
  ## Set the name for the class
  attr(me, "class") <- "txClassifier"
  return(me)
}
