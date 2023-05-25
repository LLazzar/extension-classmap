######################################

#dependencies needed 
library(stats) #for cmdsclae, to produce mds from dissimilarity/dist matrix
library(plotly) #to plot
library(gplots) #only to use col2hex()
library(grDevices) #for col2rgb() etc, managing colors
library(colorspace) #to use darken() default palette created by rainbow()

#in case of making package should import plotly (whole) and col2hex from gplots and cmdscale from stats

###############################################

#rough baseline inspiration is taken from silplot function so it is written trying to follow that style

mdscolorscale <- function (vcrout, diss, classLabels = NULL, classCols=NULL, main=NULL, size=8, bordersize=1.2, showLegend=TRUE) {
  
  # 
  #
  #
  # Arguments:
  #
  # vcrout        :the output of a vcr.*.train or vcr.*.newdata
  # diss          :a distance structure such as that returned by dist or 
  #                a full symmetric matrix containing pairwise dissimilarities about the set of
  #                observations evaluated in vcrout. The order of the observation should be the same 
  #                in which the observations are presented in producing the vcrout object.
  #                Distance matrix should be coherent with
  #                view of data points that classifiers has.
  # classLabels   :the labels (levels) of the classes. If NULL, they are taken from vcrout.
  # classCols     :a list of colors for the classes. There should be at least as many as there 
  #                are levels. If NULL a default palette is used. Suggested to use darker colors
  # main          :title for the plot. If NULL, a default title is used.
  # size          :sets the size of the plotted points. 
  # bordersize    :sets the thickness of the border around each data point. The color of the border is used to 
  #                discriminate among the different true classes of the points.
  # showLegend    :if TRUE, a legend is shown to the right of the plot.
                   
  #check vcrout object if valid and not null
  if (is.null(vcrout$levels)) { #check on random quantity levels that always exists
    stop("vcrout object is invalid (empty?, do you created it?)")
  }
  
  nlab <- length(vcrout$levels) #number of labels the data problem has
  
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
    classCols <- darken(classCols, 0.6) #darkening color beacuse with dark class color visualization is better, just darken default palette rainbow so just as sil colours but more darker
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
  
  #getting valid labels (non missing, others are discarded and not visualization) #CAN PUT THEM BUT LEFT THEM IN BLACK
  indsv <- which(!is.na(yintv))
  if (length(indsv) == 0) {
    stop(paste0(whichyint, " has no available values, ",
                " so no silhouette plot can be made."))
  }
  if (length(indsv) < 2) {
    stop(paste0("At least 2 cases with non-missing ",
                whichyint, " are required."))
  }
  yintv <- yintv[indsv] #set yintv to only available labels
  ayint <- sort(unique(yintv))
  if (sum(!(yintv %in% seq_len(nlab))) > 0) {
    stop(paste0("Not all ", whichyint, "[indsv] lie in 1:",
                nlab))
  }
  
  #getting PAC for non missing values
  #curcial quanitty that will allow to shade the filling of the points
  PAC <- vcrout$PAC[indsv] 
  
  if (sum(is.na(PAC)) > 0)
    stop("PAC[indsv] has missing values.")
  
  #computing also silhoutte value
  si <- 1 - 2 * PAC 
  
  #setting a default title for the plot
  if (is.null(main)) { 
    main <- paste0(whichdata, " MDS color-scaled plot ")
  }
  
  #dimensionality check on dimension of dissmatrix
  if (is.null(diss)) {
    stop("diss is NULL")
  }
  
  n=length(yv)
  if (is.matrix(diss)) {
    dims=dim(diss)
    if (dims[1] != n || dims[2] != n) {
      stop(paste("ERROR: The dissimilarity matrix or dist object is not a ",n, "x", n, " as expected. It should be about the same data used to produce the vcrout object feeded in this function"))
    }
  }
  else { #it's diss/dist object'
    if (attr(diss, "Size") != n ) {
      stop(paste("ERROR: The dissimilarity matrix or dist object is not aabout",n, "elements as expected. It should be about the same data used to produce the vcrout object feeded in this function"))
    }
  }
  
  #MDS scaling of the dissimilarity matrix/dist object
  mds=cmdscale(diss)
  
  #matching each observation with its class color
  ycolor=classCols[yintv]
  
  #creating a vector for faded/shaded color 
  yshade=rep(NA,length(yintv))
  
  #setting the shade according to PAC/sil value (using PAC because range for scale pal is [0,1], if we would use sil value we would have to to stretch again only in [0,1] domain)
  for (i in 1:length(yshade)){
    pal <- colorRamp(c(ycolor[i], "white")) #create the palette containing gradient between the color and white, solid white is HIGH pac (1) and low confidence of true class
    pal = pal(PAC[i]) #set color according to PAC information, could use a scaling factor to multiply PAC, such as 0.9, so that total solid white would never be reached in the shade
    yshade[i] = rgb(pal, maxColorValue = 255) #color back to hex code
  }
  
  #creating dataframe that cointains info for plotting:
  plot_data <- data.frame(
    dim1 = mds[,1], #coordinates in 2d
    dim2 = mds[,2],
    PAC = PAC,
    sil=si, #silhoutte width
    ycolor = ycolor, #class color
    yv=yv, #the label 
    yshape = ifelse(PAC > 0.5, "x", "circle"), #shape over pac 0.5 is misclassified and set to a cross
    yshade = yshade #color shade for the fill
  )
  
  
  #creating plotly basic object
  pp = plot_ly(type = 'scatter', mode = 'markers')
  
  #plotting points separately one by one with the loop
  for (i in 1:length(yintv)) {
  pp <- pp %>% 
    add_trace(x = plot_data$dim1[i], y = plot_data$dim2[i], name=paste0("Ids:",i),
              marker=list(size=size, color=plot_data$yshade[i],
                          line=list(color=plot_data$ycolor[i], width=bordersize), symbol=plot_data$yshape[i]),  
              text=paste0("PAC:", round(plot_data$PAC[i],2),"\n","Sil:", round(plot_data$sil[i],2)),
              hoverinfo="text+name", showlegend = F)
  }
  
  if (showLegend) {
    
    #add legend for shape, though dummy empty trace setting value to Inf
    pp <- pp %>% add_trace(x = Inf, y = Inf, name="Correctly classified",
                 marker=list(size=10, color='black', symbol="circle-open"), legendgroup="shape")
    pp <- pp %>% add_trace(x = Inf, y = Inf, name="Misclassified",
                           marker=list(size=10, color='black', symbol="x-open"), legendgroup="shape")
  
    #add title for legend of classes
    pp <- pp %>% add_trace(x = Inf, y = Inf, name="True Classes: (Border Color)", legendgroup="Classes", opacity=0,
                         marker=list(size=15, color='black', symbol="x-open"))
  
    #add legend for class color, as above adding dummy traces
    for (g in 1:length(lvls)) { #loop over all posible classes
    
      pp <- pp %>% add_trace(plot_data, x = Inf, y = Inf, legendgroup="Classes",
                           name=lvls[g], mode="markers", marker=list(size=10, opacity=1, color=classCols[g], symbol="circle-x-open",line=list(color=classCols[g], width=2)),
                           hoverinfo="none")
    }
  
  }
  
  #adding title to graph
  pp <- pp %>% layout(title= list(text = paste0(main)))
  
  
  return(pp)
  
}