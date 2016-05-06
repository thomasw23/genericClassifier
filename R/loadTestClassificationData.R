

loadTestClassificationData <- function(data_prefix, ...) {
  dots <- list(...)
  if (class(data_prefix) == "data.frame") {
    
    core_data <- data_prefix
    if (colnames(core_data)[1] == "pheno") {
      labels <- core_data[,1]
      names(labels) <- rownames(core_data)
    } else {
      stop("Data object must have labels or phenotypes in the first column!!")
    }
    
  } else {
    
    if(!grepl(".csv", data_prefix)) {
      data_prefix <- paste0(data_prefix, ".csv")
    }
    
    if(file.exists(data_prefix)) {
      if("probeID_col" %in% names(dots)){
        pidcol <- dots[["probeID_col"]]
      } else {
        pidcol <- c(1)
      }
      filedata <- read.csv(data_prefix, header=TRUE, row.names=pidcol, nrows=5)
      names(labels) <- labels <- gsub(".CEL|.cel", "", colnames(filedata))
      core_data <- cbind(pheno=labels, 
                         as.data.frame(t(read.csv(data_prefix, header=TRUE, row.names=pidcol))))  
    
    } else {
      stop(paste("Cannot find RMA csv output file: ", data_prefix, ". Check 'file_prefix' option.", sep=""))
    }
  }
  core_data_list <- list(core_data, labels)
  return(core_data_list)
}