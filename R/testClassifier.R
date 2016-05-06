#'Test Classifier on Sample Data 
#'
#'\code{testClassifier} returns a data frame with 2 columns: 1) diagnosis; 2) TruGRAF score.
#'
#'This function is called by \code{\link{loadClassifier}} to run the classifiers on sample
#'data and return a result (call and score) for each.
#'@param obj data.frame or txClassifier containing the original training set data. 
#'@param train_data_name character value representing the name of the classifier.
#'@param data data data frame of discretized test sample values.
#'@param sub_name character value representing the training set sample to remove if performing
#' a leave-one-out cross-validation on the training set (default: empty) 
#'@param train_data data frame of discretized training sample values.
#'@param a numeric representing the lower bound for the score transformation (default: 0).
#'@param b numeric representing the upper bound for the score transformation (default: 100).
#'@inheritParams loadClassifier
#'@return data.frame
#'@rdname testClassifier
#'@export
#'@examples 
#'\dontrun{
#'testClassifier()
#'}

testClassifier <- function(obj, train_data_name, data, type, sub_name="",
                           svm_cost, svm_gamma, ..., train_data, a=0, b=100) {
  UseMethod("testClassifier")
}

#'@rdname testClassifier

testClassifier.default <- function(obj, train_data_name, data, type, sub_name="", 
                                   svm_cost, svm_gamma, ..., train_data, a=0, b=100) {
  fc <- unlist(strsplit(train_data_name, split="v"))[1]
  dots <- list(...)
  disc_data <- do.call(subsetTestData, list(d=data, feat=colnames(train_data)[-1]))
  if(svm_gamma == "def") {svm_gamma <- 1/ncol(train_data[,-1])}  
  model <- e1071::svm(pheno ~ .,data=train_data[order(train_data$pheno, decreasing=TRUE),], 
                      probability=TRUE, cost=svm_cost, gamma=svm_gamma)
  traincalls <- e1071:::predict.svm(model, train_data[order(train_data$pheno, decreasing=TRUE),], 
                                    decision.values=TRUE, probabilities=TRUE)
  trainscores <- attr(traincalls, "decision.values")[,1]
  mn <- min(trainscores); mx <- max(trainscores)
  svmcalls <- e1071:::predict.svm(model, new=disc_data, decision.values=TRUE)
  call_score <- attr(svmcalls, "decision.values")[,1]
  new_call_score <- sapply(call_score, function(x) {          
    if(x <= 0) {
      ifelse(x > mn, b/2 + (b-(b/2))*(-x/(a - mn)), b)
    } else {
      ifelse(x < mx, b/2 - (b-(b/2))*(-x/(a - mx)), a)
    }
    
  })
  data.frame(diag=svmcalls,score=new_call_score) 
} 

#'@rdname testClassifier

testClassifier.txClassifier <- function(obj, train_data_name, data, type, sub_name="", 
                                        svm_cost, svm_gamma, ..., train_data, a=0, b=100) {
  
  if(sub_name == "") {
    dots <- list(...)
    disc_data <- do.call(subsetTestData, list(d=data, feat=obj$probes))
    svmcalls <- e1071:::predict.svm(obj$svm.model, new=disc_data, decision.values=TRUE)  
    call_score <- attr(svmcalls, "decision.values")[,1]
    new_call_score <- sapply(call_score, function(x) {          
      if(x <= 0) {
        ifelse(x > obj$min_score, b/2 + (b-(b/2))*(-x/(a - obj$min_score)), b)
      } else {
        ifelse(x < obj$max_score, b/2 - (b-(b/2))*(-x/(a - obj$max_score)), a)
      }
    })
    data.frame(diag=svmcalls,score=new_call_score)
  } else {
    train_data <- obj$data[!rownames(obj$data) %in% sub_name,]
    NextMethod("testClassifier", train_data=train_data, svm_cost=obj$cost, svm_gamma=obj$gamma )
  }
  
}

#'@rdname testClassifier

testClassifier.data.frame <- function(obj, train_data_name, data, type, sub_name="", 
                                      svm_cost, svm_gamma, ..., train_data, a=0, b=100) {
  train_data <- train_data <- obj[!rownames(obj) %in% sub_name,]
  NextMethod("testClassifier", train_data=train_data)
} 
