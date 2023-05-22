#adding new visualizations

#what needed:
#cmdscale from ?
#plotly
#col2hex from gplots

#baseline ispiration is taken from silplot function so it is written in that style

library(plotly)
library(gplots) #only to use


mdsColorscale <- function (vcrout, diss, classLabels = NULL, classCols=NULL, main=NULL, size=8, bordersize=1.2) {
  
  # 
  #
  #
  # Arguments:
  #
  # vcrout        :the output of a vcr.*.train or vcr.*.newdata
  # diss          :a distance structure such as that returned by dist or 
  #                a full symmetric matrix containing pairwise dissimilarities about the set of
  #                observations evaluated in vcrout. Distance matrix should be coherent with
  #                view of data points that classifiers has.
  # classLabels   :the labels (levels) of the classes. If NULL, they are taken from vcrout.
  # classCols     :a list of colors for the classes. There should be at least as many as there 
  #                are levels. If NULL a default palette is used.
  # main          :title for the plot. If NULL, a default title is used.
  # size          :sets the size of the plotted points. 
  # bordersize    :sets the thickness of the border around each data point. The color of the border is used to 
  #                discriminate among the different true classes of the points.
                   
  
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
  
  #produce main colors used to discrinate among different classes
  if (is.null(classCols)) {
    classCols <- rainbow(nlab) #like in silplot so that color would hopefully match in the two visualizations
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
    classCols = col2hex(classCols) #from library gplot, to convert like "blue", "green" to hex code, if that kind of colors are provided, if already in hex code nothing happens
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
  
  #dimensionality check on dimension of dissmatrix
  dims=dim(diss)
  n=length(yv)
  if (dims[1] != n || dims[2] != n) {
    stop(paste("ERROR: The dissimilarity matrix or dist object is not a ",n, "x", n, " as expected. It should be about the same data used to produce the vcrout object feeded in this function"))
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
    yshape = ifelse(PAC > 0.5, "x", "circle"),
    yshade = yshade
  )
  
  class_colors = setNames(classCols, lvls ) #match 
  
  library(ggplot2)
  
  fig = plot_ly(type = 'scatter', mode = 'markers')
  
  #fig <- fig %>% add_markers(x = mds[,1], y = mds[,2], name="corect class", 
                             #marker=list(size=7, color=I('black'), symbol=c("circle-open","square")), visible = 'legendonly', showlegend=TRUE)
  
  for (i in 1:nrow(plot_data)) {
  fig <- fig %>% 
    add_trace(x = plot_data$dim1[i], y = plot_data$dim2[i], 
              marker=list(size=size, color=plot_data$yshade[i],
                          line=list(color=plot_data$ycolor[i], width=bordersize), symbol=plot_data$yshape[i]),  showlegend = F)
  }
  
  #add dummy traces for legend
  
  #legend for the shape correct/incorrect prediction
  fig <- fig %>% add_trace(x = Inf, y = Inf, name="Correctly classified",
             marker=list(size=12, color='black', symbol="circle-open"))
  
  fig <- fig %>% add_trace(x = Inf, y = Inf, name="Misclassified",
                           marker=list(size=12, color='black', symbol="x-open"))
  
  
  
  #class color
  
  for (g in 1:length(lvls)) {
    
  fig <- fig %>% add_trace(plot_data, x = Inf, y = Inf, legendgroup="Classes",
                           name=lvls[g], mode="markers", marker=list(size=8, opacity=1, color=classCols[g], symbol="circle-open",line=list(color=classCols[g], width=2)),
                           hoverinfo="none")
  }
  
  fig <- fig %>% layout(title= list(text = paste0(main)))
  
  #https://plotly.com/r/line-and-scatter/#mapping-data-to-symbols
  fig
  
  
  
  return(fig)
  
}