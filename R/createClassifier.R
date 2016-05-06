#'Manage training and creation of individual classifiers
#'
#'\code{createClassifier} returns a list of 3 elements: 1) list of classifier
#'probeset ids; 2) numeric vector of classifier accuracies: 3) list of classifier 
#'probesets discretized training data. 
#'
#'This function is a wrapper for calling \code{\link{subsetComparison}} prior to a
#''typeK' based decision. If 'typeK' equals "range", then 
#'\code{\link{scanClassifierRange}} is called and produces classifier
#'accuracies for a specified range. Otherwise \code{\link{testForExistingClassifier}}
#'and \code{\link{selectFeaturesMRMR}} are called to create the classifier based on a
#'specified feature set size.
#'@param data data frame of discretized RMA values for training set with samples as
#'columns and probesets as rows
#'@param comp_type character vector of 2 elements that specifies the groups of samples
#'to keep in the data frame (default: 4).
#'@param typeK character that specifies whether to test a range of feature set parameters
#'("range") or lock the classifier based on a specified feature set size ("set").
#'@param range_pars vector specifying feature set size parameters specific to type of 
#'training specified by 'typeK'. 
#'@param remove_overlaps logical specifying whether to remove overlapping probesets 
#'if the 'txClassifier' list has more than one classifier (default = TRUE).
#'@param overlap_class_1 character specifying the name of the first classifier to be 
#'used when looking for overlapping probesets when remove_overlaps is TRUE.
#'@param overlap_class_2 character specifying the name of the second classifier to be 
#'used when looking for overlapping probesets when remove_overlaps is TRUE.
#'#'@param ... additional arguments include: "svm_cost", "svm_gamma", 
#'"classifier_name_base", "classifier_name".
#'@inheritParams trainClassifier
#'@return list
#'@examples
#'\dontrun{
#'func_output_1 <- passDataToCreateClassifier(data, c("C","A"), "range", c(250, 90, 110))
#'func_output_2 <- passDataToCreateClassifier(data, c("C","A"), "set", c("full", 100))
#'} 

createClassifier <- function(data, comp_type, typeK="range", range_pars, remove_overlaps=TRUE, 
                                       type="PAX", overlap_class_1="", overlap_class_2="", ...) {
  dots <- list(...)
  comp_type_label <- paste0(comp_type[1], "v", comp_type[2])
  data_subset <- subsetComparison(data, comp_type)
  message(paste("samples:", dim(data_subset)[1], "; genes:", dim(data_subset)[2], sep=" "))
  
  if (missing(typeK)) stop("Must define a single feature set size or a range!")
  svmargs <- c("svm_cost", "svm_gamma")
  classifierargs <- c("classifier_name_base", "classifier_name")
  
  if (typeK == "range") {
    createClassOut <- do.call(scanClassifierRange, c(list(data_subset=data_subset, range_pars=range_pars, type=type, 
                                                           comp_type=comp_type), dots[names(dots) %in% svmargs]))
  } else {
    class_created <- do.call(testForExistingClassifier, c(list(type=type), 
                                                          dots[names(dots) %in% classifierargs]))
    classOutFeat <- selectFeaturesMRMR(data_subset, feat_count=as.numeric(range_pars[2]))
    classOutFeatData <- data_subset[,classOutFeat@filters[[1]][,1]]

    if (sum(unique(classOutFeatData[,1]) %in% unique(data_subset$pheno)) == 0) {
      cv <- do.call(crossValidateClassifier, c(list(df=cbind(pheno=data_subset$pheno, classOutFeatData), 
                                                    num_iter=range_pars[1]),dots[names(dots) %in% svmargs]))
    } else {
      cv <- c("NA")
    }
    
    temp_class <- get(names(class_created)[1])
    temp_class[[comp_type_label]] <- do.call(txClassifier, c(list(classes=comp_type, type=type,
                                                                  data_set=cbind(pheno=data_subset$pheno, classOutFeatData)),  
                                                             dots[names(dots) %in% svmargs]))
    
    if(length(temp_class) > 1) {

      if(remove_overlaps) {
        if(length(temp_class) == 2) {
          overlap_class_1 <- 1
          overlap_class_2 <- 2
        }
        overlaps <- intersect(colnames(temp_class[[overlap_class_1]])[-1],
                              colnames(temp_class[[overlap_class_2]])[-1])
        temp_class[[overlap_class_1]] <- temp_class[[overlap_class_1]][, 
                                   !colnames(temp_class[[overlap_class_1]]) %in% overlaps]
        temp_class[[overlap_class_2]] <- temp_class[[overlap_class_2]][, 
                                   !colnames(temp_class[[overlap_class_2]]) %in% overlaps]
      }
    }
    
    assign(names(class_created), temp_class, envir = globalenv())
    save(list=c(names(class_created)[1]), file=paste0(names(class_created)[1], ".rda"))   
    
    # Report the median LOOCV accuracy 
    cvacc <-  cv
    message(paste("Leave One Out Cross Validation Accuracy for ", range_pars[2], 
                  " features: ", round(cvacc, 3), sep=""))
    createClassOut <- list(colnames(classOutFeatData), cv, classOutFeatData)
  }
  
  return(createClassOut)
}
