#'Plot a Range of Classifier Accuracies
#'
#'Produces either a boxplot or a scatter plot of classifier accuracies 
#'determined by leave one out cross validation of a trained SVM model.
#'
#'@param df data.frame specifying plot values
#'@param plot_type character which can be either "pts" or "box" to specify
#'the type of plot. If more than one y value exists for a given x value, 
#'then "box" shall be value.
#'@param comp_type character vector of 2 elements. This value is used to
#' create the main title of the plot. 
#'@param samples integer specifying the number of samples used to generate
#'accuracy values. 
#'@param genes integer specifying the number of genes (probesets) used to
#'search for classifiers.
#'@param ybounds optional numeric vector to specify the plot range on the
#'y-axis (default: c(0.6, 1.0).
#'@param save_dir optional character specifying a location for saving the
#'generated plots (default: NULL).
#'@return NULL
#'@examples
#'kRange <- seq(51,100) 
#'accur <- rnorm(50, 0.8, 0.1)
#'ptsData <- data.frame(fss=kRange,acc=accur)
#'
#'# Scatter Plot
#'plotRangeQC(ptsData, "pts", comp_type=3, samples=200, genes=15000)
#'
#'boxAccur <- lapply(accur, function(x) rnorm(25, x, 0.05))
#'plotdf <- do.call(rbind, lapply(1:length(kRange), function(x) {
#'data.frame(fss=rep(kRange[x],length(boxAccur[[x]])), 
#'           acc=boxAccur[[x]])}))
#'# Boxplot
#'plotRangeQC(plotdf, "box", comp_type=3, samples=200, genes=15000)

plotRangeQC <- function(df, plot_type, comp_type, samples, genes, 
                        ybounds=c(0.6, 1.0), save_dir=NULL) {
  
  # samples: # of samples used in comparison (determined by calling function)
  # genes: # of genes used in comparison (determined by calling function
  
  if(!(missing(samples)) && !(missing(genes))) {
    details <- paste(paste0(comp_type[1], "v", comp_type[2]), "samples",samples, "genes", genes, sep="_")
  } else {
    details <- NULL
  }
  
  # savedir: location for saving plot other than working directory, default is NULL

  plotname <- paste("LOOCVaccuracy_RangeOfFeatureSetSizes_", details, "_", plot_type,".pdf", sep="")

  if (!is.null(save_dir)) {
    plotname <- paste(save_dir, plotname, sep="/")
  } else {
    
    # test/create a sub-directory for figures in the output folder
    
    figDir <- c("Figures")
    if (!file.exists(figDir)) dir.create(file.path(figDir), recursive = T)
    plotname <- paste(figDir, plotname, sep="/")
  }

  head_name <- paste("MRMR Feature Set Size Accuracy - ", paste(comp_type[1], "vs.", comp_type[2], sep= " "), sep="") 
  
  pdf(file=plotname, width=5, height=5)

  if(plot_type == "pts") {
    plot(df$fss, 
         df$acc, 
         cex=0.5, 
         xlab="Feature Set Size", 
         ylab="Leave One Out CV Accuracy (%)",
         main=head_name, 
         ylim=ybounds)
    
  } else if (plot_type == "box") {
    boxplot(acc~fss, 
            data=df,
            xlab="Feature Set Size",
            ylab="Leave One Out - CV Accuracy (%)", 
            main=head_name, 
            ylim=ybounds)
    
  } else {
    stop("Invalid plot type chosen")
  }

  dev.off()
}
