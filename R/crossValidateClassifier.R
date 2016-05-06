#'Measure SVM Performance with Leave-One-Out Cross-Validation  
#'
#'\code{crossValidateClassifier} returns a number representing the accuracy of the
#'input classifier after performing leave-one-out cross-validation on an svm model
#' 
#'This function takes a data frame containing classifier probe values for
#'all training set samples and performs a leave-one-out cross-validation ("full" or
#'with replacement and a specified number of iterations) on an svm model to determine
#'the accuracy of the classifier.
#'@param df data frame where rows are samples, column 1 is the sample phenotype
#'(i.e. C, A, or B), and the remaining columns are values for the classifier probesets.
#'@param num_iter integer specifying the number of times to sample (w/ replacement)
#'from the available row numbers. If numIter equals "full", then the function 
#'\code{\link{loadClassifier}} (with a object of class "overall" will be called 
#'instead (default: "full").
#'@param ... additional arguments include: None
#'@inheritParams txClassifier
#'@return numeric
#'@rdname crossValidateClassifier
#'@export
#'@examples 
#'\dontrun{
#'crossValidateClassifier(df, 100)
#'}
crossValidateClassifier <- function(df, num_iter, ..., svm_cost=1, svm_gamma="def") {
  UseMethod("crossValidateClassifier", num_iter)
}

#'@rdname crossValidateClassifier

crossValidateClassifier.default <- function(df, num_iter, ..., svm_cost=1, svm_gamma="def") {
  # df - data frame of values to train on 
  # numIter - number of times to samples (with replacement) from the training data
  if(svm_gamma == "def") {svm_gamma <- 1/ncol(df[,-1])}
  r <- nrow(df)
  s <- sample(1:r, num_iter, replace=TRUE)
  sm <- sum(sapply(s, function(x) {
    pfit <- e1071:::predict.svm(e1071::svm(pheno ~ ., data=df[-x,], 
                                cost=svm_cost, gamma=svm_gamma),newdata=df[x,])
    ifelse(df$pheno[x] == as.character(pfit), 1,0)
  }	))
  acc <- sm/num_iter
  return(acc)
}

### Leave-One-Out Cross-Validation w/o replacement to get simple accuracy
#'Measure SVM Accuracy with Leave-One-Out Cross-Validation (w/o Replacement)
#'
#'\code{crossValidateClassifier} returns a numeric vector of accuracies determined by full leave-one-out
#'cross-validation on an svm model.
#'
#'This function takes a list of data frames containing classifier probe values
#'for all training set samples and performs a full leave-one-out cross-validation (w/o 
#'replacement) of the svm model to determine the accuracy of the classifier.
#'@return numeric vector
#'@rdname crossValidateClassifier
#'@examples 
#'data(txClassifierPAX)
#'lapply(txClassifierPAX, function(x) {
#'  crossValidateClassifier(x, "full", svm_cost=1, svm_gamma="def")
#'}) 
#'

crossValidateClassifier.character <- function(df, num_iter, ..., svm_cost=1, svm_gamma="def") {
  
  if(!tolower(num_iter) == "full") stop("Cannot determine method of cross validation. Specify an integer or 'full'.")
  if(svm_gamma == "def") {svm_gamma <- 1/ncol(df[,-1])}
  out <- lapply(list(df), function(x) {
    s <- sample(1:nrow(x), nrow(x), replace=FALSE)
    sm <- sum(sapply(s, function(y) {
      pfit <- e1071:::predict.svm(e1071::svm(pheno ~ ., data=x[-y,], 
                                  cost=svm_cost, gamma=svm_gamma), newdata=x[y,]) 
      ifelse(x$pheno[y] == as.character(pfit), 1, 0)
    } ))
    sm/nrow(x)
  })
  unlist(out)  
}
