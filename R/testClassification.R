#'Run Classification on Test Samples with Known Phenotype 
#'
#'#'\code{testClassification} returns a list of vectors of 4 elements: 1) sample IDs; 
#'2) classification; 3) score; 4) actual classification. 
#'
#'This function is a wrapper for calling \code{\link{loadClassifier}} and outputs 
#'an extra column to calculate accuracy.
#'@param data_prefix character specificying the name of the file (with or without 
#'".csv") to be loaded for classification. Can also be a preloaded data frame that 
#'is correctly formatted (i.e. phenotypes/labels = column 1, rows = samples, 
#'probe data = columns)
#'@param type character of value "PAX" or "BX" specifying the type of samples to be
#'tested (default: PAX). 
#'@param id_data character specifying a file that contains additional identifying
#' information about the test samples. Can also be a preloaded vector(w/names) that 
#' be concatenated to the data (default: NULL). 
#'@param pheno_col numeric specifying the column in 'id_file' with known sample 
#'phenotypes (default: 1).
#'@param id_col numeric specifying the column in 'id_file' with sample ids (default: 2).
#'@param output_file_prefix character specifying the beginning of the output filename
#'which will be added to the time and date stamp and ".csv" (default: NULL).
#'@param calc_ts_overall_accuracy logical specifying whether to run leave-one-out cross-
#'validation on the samples being tested (default: FALSE).
#'@param ... additional arguments include: "col", "head", "id", "data_start_col", 
#'"probeID_col", "classifier_name", "classifier_name_base", "svm_cost", "svm_gamma" 
#'@return list
#'@examples
#'\dontrun{ 
#'testClassification(data_prefix="train_data", calc_ts_overall_accuracy=TRUE)
#'}

testClassification <- function(data_prefix, type="PAX", id_data=NULL, pheno_col=1, id_col=2,  
                                output_file_prefix=NULL, calc_ts_overall_accuracy = FALSE, ... ) {
  dots <- list(...)
  if (!is.null(id_data)) {
    
    if (class(id_data) == "character" & !(is.null(names(id_data)))) {
      labels <- id_data 
    } else {
      message(paste("id filename: ", id_data, sep=""))
      labels <- do.call(getLabels, list(id_data, col=pheno_col, id=id_col))
    }
    func1args <- c("")
    core_data_list <- do.call(loadTestClassificationData, c(list(data_prefix=data_prefix),
                                                            dots[names(dots) %in% c("probeID_col")]))
    if (length(labels) != length(core_data_list[[2]])) {
      stop("There is a discrepancy between labels/phenotypes in data_prefix and id_data")
    }
    core_data <- core_data_list[[1]]
    
  } else if (calc_ts_overall_accuracy) {
    stop("Need to specify 'id_file' to use this QC function!")
  } else {
    
    core_data_list <- do.call(loadTestClassificationData, c(list(data_prefix=data_prefix),
                                                            dots[names(dots) %in% c("probeID_col")]))
    core_data <- core_data_list[[1]]
    labels <- core_data_list[[2]]
  }  

  svmargs <- c("svm_cost", "svm_gamma")
  func3args <- c("classifier_name", "classifier_name_base")
  if(calc_ts_overall_accuracy) {
  
    message("Performing Leave-one-out Cross Validation on Samples!")
    output <- do.call(loadClassifier, c(list(obj=structure(list(), class="yesCV"), data=core_data,
                                             type=type), dots[names(dots) %in% c(func3args, svmargs)]))
    output2 <- cbind(output[[1]], labels[output[[2]]]) 
  
  } else {
  
    classOutput <- do.call(loadClassifier, c(list(obj=structure(list(), class="noCV"), data=core_data, 
                                                  type=type), dots[names(dots) %in% c(func3args, svmargs)]))
    output <- reportClassifierCalls(classOutput)
    output2 <- cbind(names(labels), output, labels)
  
  }

  colnames(output2) <- c("sample", "classification", 
                          colnames(output2)[grep("score", colnames(output2))],
                          "actual_classification")
  train_acc <- 100*(sum(output2$classification == output2$actual_classification)/dim(output2)[1])
  message(sprintf("After locking classifier, new overall accuracy: %s percent", train_acc))
  
  if(!is.null(output_file_prefix)) {
    timestamp <- paste(unlist(strsplit(as.character(Sys.time()), split = " "))[1],
                       paste(unlist(strsplit(unlist(strsplit(as.character(Sys.time()), split = " "))[2],
                                             split=":")),collapse="-"), sep="_")
    outfile <- sprintf("%s_%s.txt", output_file_prefix, timestamp)
    write.table(output2, file=outfile, sep="\t", quote=FALSE, row.names=FALSE)
  }
    
  return(output2) 
}
