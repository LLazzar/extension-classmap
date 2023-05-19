#adding new visualizations

#what needed:
#cmdsale from

#baseline ispiration is taken from silplot function so it is written in that style

mdsColorscale <- function (vcrout, diss, classLabels = NULL) {
  
  nlab <- length(vcrout$levels)
  
  #handling class labels, mainly for legend and readibility of the graph
  if (is.null(classLabels)) { 
    lvls <- vcrout$levels
  }
  else {
    if (!is.vector(classLabels)) {
      stop("\n classLabels should be a vector")
    }
    lvls <- classLabels
    if (length(lvls) != nlab) {
      stop(paste0("\n The number of classLabels should equal",
                  " length(vcrout$levels) = ", nlab, "."))
    }
  }
  
  #produce main colors
  if (is.null(classCols)) {
    classCols <- rainbow(nlab) #like in silplot son that color would hopefully match in the two visualizaitons
  }
  else {
    if (!is.vector(classCols)) {
      stop("\n classCols should be a vector")
    }
    if (length(classCols) < nlab) {
      stop(paste0("\n The number of classCols should be at",
                  " least length(vcrout$levels) = ", nlab,
                  "."))
    }
    classCols <- classCols[seq_len(nlab)]
  }
  
  #getting the given classes and checks for training and test
  if (is.null(vcrout$yintnew)) {
    if (is.null(vcrout$yint)) {
      stop("there is no vcrout$yint or vcrout$yintnew")
    }
    yintv <- vcrout$yint  
    training <- TRUE    #identify in variable that discrimanates between training and test set
  }
  else {
    yintv <- vcrout$yintnew #yintv represents true labels regardless training or not 
    training <- FALSE
  }
  
  #produce strings for training and test
  whichdata <- if (training) {
    "[training]"
  }
  else {
    "[newdata]"
  }
  whichyint <- if (training) {
    "yint"
  }
  else {
    "yintnew"
  }
  
  #getting valid labels (non missing, others are disgarded in visualization) #CAN PUT THEM BUT LEFT THEM IN BLACK
  indsv <- which(!is.na(yintv))
  if (length(indsv) == 0) {
    stop(paste0(whichyint, " has no available values, ",
                " so no silhouette plot can be made."))
  }
  if (length(indsv) < 2) {
    stop(paste0("At least 2 cases with non-missing ",
                whichyint, " are required."))
  }
  yintv <- yintv[indsv]
  ayint <- sort(unique(yintv))
  if (sum(!(yintv %in% seq_len(nlab))) > 0) {
    stop(paste0("Not all ", whichyint, "[indsv] lie in 1:",
                nlab))
  }
  
  #getting pac for non missing values
  PAC <- vcrout$PAC[indsv]
  
  if (sum(is.na(PAC)) > 0)
    stop("PAC[indsv] has missing values.")
  
  #actually convenient using silohutte value sense of intepretation
  #high sil (near 1), max intensity color which is max confidence right class
  #low sil (near 1), low intensity color which is min confidence right class
  # <0, change also shape meaning it's missclassified
  si <- 1 - 2 * PAC 
  
  #setting default title of the plot
  if (is.null(main)) { 
    main <- paste0(whichdata, " MDS color-scaled plot ")
  }
  
  cmdscale(diss)
  
  y=c("A","A","A", "C", "B", "B", "C")
  y=as.factor(y)
  levels=levels(y)
  nlab=length(levels)
  classCols #defines class colors
  
  #matching each observation with its class color
  col_vec_true = colors[match(data$y, c("  1", "  2", "  3", "  4"))]
  
  
  
}