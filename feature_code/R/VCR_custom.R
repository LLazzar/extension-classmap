######################### (code that will not be needed in case of classmap merge)

#to import functions needed for VCR_custom (especially for checkLabels, computeFarness..)
library(cellWise) #for transfo function used in Comp fareness in VCR_auxillaryFunctions.R #this would go in imports in (NAMESCPACE) of the package
source("R_classmap_package_full/R/VCR_auxiliaryFunctions.R") #importing auxillary functions needed
                                                             #this script is available in classmap package
                                                             #so in case of integration of VCR_custom to classmap
                                                             #this import would be useless

#when building classmapExt cellwise should go on imports and VCR_auxiliaryFunctions.R put in the package (as it can't be imported from classmap sinc eit is invisible from user)
#in github repo package add about merge to classmpa section and what should be disgarded or added to classmap

#########################

# TRYING TO DEVISE THIS FUNCTION LIKE vcr.neural ALREADY IN THE PACKAGE THAT IS RATHER FLEXIBLE FOR NEURAL NETWORKS


vcr.custom.train <- function(y, probs, distToClasses=NULL) {
  #
  # Using the outputs of a general classification algorithm
  # applied to the training data, this function prepares for
  # graphical displays.
  #
  #
  #
  # 
  # Arguments:
  #
  #   y             : factor with the given class labels of the objects.
  #                   Make sure that the levels are in the same order as
  #                   used by the training algorithm.
  #   probs         : set of posteriors probabilities that the trained model produce
  #                   on the training set considered, either naturally or derived in some way by the user for its algorithm
  #                   The columns of probs must be in the same order as the levels of y.
  #                   This posteriors should be consisent with the fact that classifier
  #                   object to class with higher posterior (NBNB).
  #   distToClasses : set of distances of each training observation to all possible class in
  #                   the eye of the trained algorithm, defined by user, it allows computation 
  #                   of farness for each observaiton and thus of the classmap plot.
  #                   The columns of distToclass, that represents the classes, must be in the same order as the levels of y.
  # 
  # Returns:
  #   yint      : given labels as integer 1, 2, 3, ...
  #   y         : given labels
  #   levels    : levels of y
  #   predint   : predicted class number. For each case this
  #               is the class with the highest probability,
  #               that is, which.max(probs[i,]).
  #   pred      : predicted label of each object.
  #   altint    : alternative class as integer, i.e. the non-given
  #               class number with the highest mixture density.
  #   altlab    : alternative class of each object.
  #   PAC       : probability of alternative class for each object
  #   figparams : parameters for computing fig, can be used for
  #               new data.
  #   fig       : farness of each object i from each class g.
  #   farness   : farness of each object to its given class.
  #   ofarness  : For each object i, its lowest farness to any
  #               class, including its own. Always exists.
  #
  #
  #
  #

  n <- length(y)
  if (n < 2) stop("The training data should have more than one case.") 

  # Check whether y and its levels are of the right form:
  checked <- checkLabels(y, n, training = TRUE) 
  
  # If it did not stop: yint has length n, and its values are in
  # 1, ..., nlab without gaps. It is NA where y is: outside indsv.
  lab2int <- checked$lab2int #retrieves function that form labels levels assign integers
  indsv   <- checked$indsv #vector contains the indices of the entries of the response variable y that can be visualized, i.e., are not NA.
  levels  <- checked$levels #unique labels (or levels) of the response variable y that actually occur in the data
  nlab    <- length(levels)
  yint    <- lab2int(y) #given label (true) as integer
  yintv   <- yint[indsv] #subsetting yint variable to non NAs
  
  #
  #
  # Check matrix of posterior probabilities:
  #
  
  probs <- as.matrix(probs)
  if (length(dim(probs)) != 2) stop("probs should be a matrix.")
  if (nrow(probs) != n) stop(paste0(
    "The matrix probs should have ", n, " rows"))
  if (ncol(probs) != nlab) stop(paste0(
    "The matrix probs should have ", nlab, " columns"))
  if (any(is.na(probs))) stop("probs should not have any NA's.")
  #
  # Compute prediction for all objects in the training data:
  #
  predint <- apply(probs[, , drop = FALSE], 1, which.max) #label that should be predicted by algorithm
                                                          #for that NB that user provide posterior that are consistent
                                                          #in the sense that classifier predicts label with higher posterior
  
  #
  #
  # 
  # Computing all the quantities for the PAC
  ptrue <- palt <- altint <- PAC <- rep(NA, n)
  for (g in seq_len(nlab)) { # g=1
    clinds <- indsv[which(yintv == g)] # indices in 1, ..., n
    others <- (seq_len(nlab))[-g] # alternative classes
    ptrue[clinds]  <- probs[clinds, g]
    palt[clinds]   <- apply(probs[clinds, others, drop = FALSE], 1, max)
    altint[clinds] <- others[apply(probs[clinds, others, drop = FALSE],
                                   1, which.max)]
  }
  #
  # Compute PAC:
  #
  PAC[indsv] <- palt[indsv] / (ptrue[indsv] + palt[indsv])
  # (PAC and altint stay NA outside indsv)
  #
  #
  #
  if (!is.null(distToClasses)) { #if feeded then D(x,g) is estimated to get farness and paramters for estimation are saved for eventual test data
    #
    # Compute Farness
    #
    #
    ##########checks of correct input
    if (length(dim(distToClasses)) != 2) stop("distToclasses should be a matrix.")
    if (nrow(probs) != n) stop(paste0(
      "The matrix distToclasses should have ", n, " rows"))
    if (ncol(probs) != nlab) stop(paste0(
      "The matrix distToclasses should have ", nlab, " columns"))
    if (any(is.na(distToClasses))) stop("distToclasses should not have any NA's.")
    ####################

    #using compFarness with affine option that takes input the matrix distToClasses and estimate cumulative 
    #distribution D(x,ki)
    farout <- compFarness(type = "affine", testdata = FALSE, yint = yint,
                        nlab = nlab, X = NULL, fig = distToClasses,
                        d = NULL, figparams = NULL) 

    figparams <- farout$figparams
  }
  else {
    figparams=NULL
    farout=NULL
  }


  return(list(yint = yint, #integer labels
              y = levels[yint], #getting back y (for example if they were a string)
              levels = levels,
              predint = predint, #predicted integer value
              pred = levels[predint], #predicted value (for example if they were a string)
              altint = altint, #integer of best alternative class
              altlab = levels[altint], #best alternative clas
              PAC = PAC, #PAC
              figparams = figparams, #EXIST ONLY IF DISTTOCLLASS FEEDEDparamters estimated to get to estimate cdf D(x,g) that is used to compute farnees
              fig = farout$fig, #EXIST ONLY .. estimated farness of each observation to each class
              farness = farout$farness, #EXIST ONLY... #estimated farness of each observation to each given class
              ofarness = farout$ofarness #EXIST ONLY
              ))
}

vcr.custom.newdata <- function(ynew, probs, vcr.custom.train.out, newDistToClasses=NULL){
  #
  # Prepares graphical display of new data (test) fitted by a custom classifier
  # that was modeled on the training data, using the output
  # of vcr.custom.train() on the training data.
  #
  # Arguments:
  #   
  #   ynew                 : factor with class membership of each new
  #                          case. Can be NA for some or all cases.
  #                          If NULL, is assumed to be NA everywhere.
  #   probs                : posterior probabilities obtained by
  #                          running the classifier on the new data.
  #   vcr.custom.train.out : output of vcr.custom.train() on the
  #                          training data.
  #
  # Returns:
  #   yintnew   : given labels as integers 1, 2, 3, ..., nlab.
  #               Can have NA's.
  #   ynew      : labels if yintnew is available, else NA.
  #   levels    : levels of the response, from vcr.custom.train.out
  #   predint   : predicted label as integer, always exists.
  #   pred      : predicted label of each object
  #   altint    : alternative label as integer if yintnew was given,
  #               else NA.
  #   altlab    : alternative label if yintnew was given, else NA.
  #   PAC       : probability of alternative class for each object
  #               with non-NA yintnew.
  #   figparams : (from training) parameters used to compute fig
  #   fig       : farness of each object i from each class g.  #             
  #   farness   : farness of each object to its given class, for
  #               objects with non-NA yintnew.
  #   ofarness  : For each object i, its lowest farness to any
  #               class, including its own. Always exists.
  #
  n <- length(ynew)
  
  levels <- vcr.custom.train.out$levels # as in training data #for this we require vcr.custom.train out
  nlab   <- length(levels) # number of classes
  #
  if (is.null(ynew)) ynew <- factor(rep(NA, n), levels = levels)
  checked <- checkLabels(ynew, n, training = FALSE, levels)
  lab2int <- checked$lab2int # is a function
  indsv   <- checked$indsv   # INDiceS of cases we can Visualize,
  #                         # can even be empty, is OK.
  nvis    <- length(indsv)   # can be zero.
  yintnew <- rep(NA, n)       # needed to overwrite "new" levels.
  yintnew[indsv] <- lab2int(ynew[indsv])
  # Any "new" levels are now set to NA.
  #
  probs <- as.matrix(probs)
  if (length(dim(probs)) != 2) stop("probs should be a matrix.")
  if (ncol(probs) == 1) probs <- t(probs) # if new data is 1 object
  if (ncol(probs) != nlab) stop(paste0(
    "The matrix probs should have ", nlab, " columns"))
  if (any(is.na(probs))) stop("probs should not have any NA's.")
  #
  # Compute prediction for all objects in the new data:
  #
  predint <- apply(probs[, , drop = FALSE], 1, which.max)
  #
  # Compute PAC for all objects with available y:
  #
  ptrue <- palt <- altint <- PAC <- rep(NA, n)
  if (nvis > 0) { # if there are new data with labels
    yintv  <- yintnew[indsv] # yint of cases we will Visualize
    ayintv <- sort(unique(yintv)) # available yintv values
    # ayintv can be a proper subset of 1, ..., nlab for new data.
    for (g in ayintv) {
      clinds <- indsv[which(yintv == g)] # indices in 1, ..., n
      others <- (seq_len(nlab))[-g] # non-self classes
      ptrue[clinds]  <- probs[clinds, g]
      palt[clinds]   <- apply(probs[clinds, others, drop  = FALSE], 1, max)
      altint[clinds] <- others[apply(probs[clinds, others, drop = FALSE],
                                     1, which.max)]
    }
    PAC[indsv] <- palt[indsv] / (ptrue[indsv] + palt[indsv])
    # (PAC and altint stay NA outside indsv)
  }
  
  if (!is.null(newDistToClasses)) {
    # Compute farness:
    #
    #
    ##########checks of correct input
    if (length(dim(newDistToClasses)) != 2) stop("newDistToclasses should be a matrix.")
    if (nrow(newDistToClasses) != n) stop(paste0(
      "The matrix newDistToclasses should have ", n, " rows"))
    if (ncol(newDistToClasses) != nlab) stop(paste0(
      "The matrix newDistToclasses should have ", nlab, " columns"))
    if (any(is.na(newDistToClasses))) stop("newDistToclasses should not have any NA's.")
    ####################
    #
    #
    #
    
    farout <- compFarness(type = "affine", testdata = TRUE, #again affine is good for our purpose as it does estimation based on train paramters
                          yint = yintnew, nlab = nlab, X = NULL,
                          fig = newDistToClasses, figparams = vcr.custom.train.out$figparams) #removed d= check if it is ok!
    }
  else {
    figparams=NULL
    farout=NULL
  }
    
  
  return(list(yintnew = yintnew,
              ynew = levels[yintnew],
              levels = levels,
              predint = predint,
              pred = levels[predint],
              altint = altint,
              altlab = levels[altint],
              PAC = PAC,
              fig = farout$fig,
              farness = farout$farness,
              ofarness = farout$ofarness))
}
