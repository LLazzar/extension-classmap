#adding new visualizations

#what needed:
#cmdscale from ?
#ggplot2

#baseline ispiration is taken from silplot function so it is written in that style

library(plotly)


mdsColorscale <- function (vcrout, diss, classCols=NULL, classLabels = NULL, main=NULL, size=3) {
  
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
    classCols <- rainbow(nlab) #like in silplot son that color would hopefully match in the two visualizations
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
    yv <- vcrout$y
    training <- TRUE    #identify in variable that discrimanates between training and test set
  }
  else {
    yintv <- vcrout$yintnew #yintv represents true labels regardless training or not 
    yv <- vcrout$ynew
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
  
  mds=cmdscale(diss)
  
  #QUANTITIES DEFINED FOR TES, REMOVE LATER
  #y=c("A","A","A", "C", "B", "B", "C")
  #y=as.factor(y)
  #yintv=as.numeric(y)
  #levels=levels(y)
  #nlab=length(levels)
  #classCols #defines class colors
  
  #matching each observation with its class color
  ycolor=classCols[yintv]
  
  #creating a vector with faded color 
  yshade=rep(NA,length(yintv))
  
  #setting the shade according to PAC/sil value (using PAC because range for scale pal is [0,1], if we would use sil value we would have to to stretch again only in [0,1] domain)
  for (i in 1:length(yshade)){
    pal <- colorRamp(c(ycolor[i], "white")) #create the palette containing gradient between the color and white
    pal = pal(0.9*PAC[i]) #set color according to PAC information
    yshade[i] = rgb(pal, maxColorValue = 255) #color back to code
  }
  
  #setting shape
  yshape=ifelse(PAC > 0.5, 22, 21) #if PAC > 0.5 point is misclassified, shape pch=22 square
  #else  point is correctly classified, shape pch=21 circle
  
  #plot(mds, col=ycolor, pch=yshape, bg=yshade, cex=1)
  
  #trying instead using ggplot2
  #in ggplot we need dataframe with all related quantities, that's how it works
  
  #creating dataframe with coordinates
  plot_data <- data.frame(
    dim1 = mds[,1],
    dim2 = mds[,2],
    PAC = PAC,
    ycolor = ycolor,
    yv=yv,
    yshape = ifelse(PAC > 0.5, "Misclassified", "Correctly Classified"),
    yshade = yshade
  )
  
  class_colors = setNames(classCols, lvls ) #match 
  
  library(ggplot2)
  
  fig <- plot_ly(data = plot_data, x = ~dim1, y = ~dim2, type = 'scatter',
                 mode = 'markers', symbol = ~yshape, symbols = c('100','101'),
                 color = I("black"),  marker = list(size = 10))
  
  #https://plotly.com/r/line-and-scatter/#mapping-data-to-symbols
  
  
  
  
  return(fig)
  
}