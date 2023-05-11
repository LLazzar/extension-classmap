######################### (code that will not be needed in case of classmap merge)
#to import functions needed for VCR_pamr (especially for checkLabels, computeFarness..)
library(cellWise) #for transfo function used in Comp fareness in VCR_auxillaryFunctions.R
source("R_classmap_package_full/R/VCR_auxiliaryFunctions.R") #importing auxillary functions needed
                                                             # this script is available in classmap package
                                                             # so in case of integration of VCR_pamr this import would be useless
#########################
source("feature_code/R/VCR_auxiliaryFunctions_alt.R") # an alternative version of the file used for some experiments
                                                      # for example i tried to removed the division by omedian in compFareness

# TRYING TO DEVISE THIS FUNCTION LIKE vcr.neural ALREADY IN THE PACKAGE THAT IS RATHER FLEXIBLE FOR NEURAL NETWORKS


vcr.custom.train <- function(X, y, probs, distToClasses=NULL) {
  #
  # Using the outputs of a general classification algorithm
  # applied to the training data, this function prepares for
  # graphical displays.
  #
  ##########
  # TO DO LIST:
  # MISSING vcr.custom.newdata
  #########
  #
  # 
  # Arguments:
  #   X             : the coordinates of the n objects of the training
  #                   data. Missing values are not allowed.
  #   y             : factor with the given class labels of the objects.
  #                   Make sure that the levels are in the same order as
  #                   used by the training algorithm.
  #   posteriors    : set of posteriors probabilities that the trained model produce
  #                   on the training set, either naturally or derived in some way by the user for its algorithm
  #                   The columns of probs must be in the same order as the levels of y.
  #                   This posteriors should be consisent with the fact that classifier
  #                   obsject to class with higher posterior.
  #   distToClasses : set of distances of each training observation to all possible class in
  #                 : the eye of the trained algorithm, defined by user, it allows computation 
  #                 : of farness for each observaiton and thus of the classmap plot
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
  #   classMS   : list with center and covariance matrix of each class
  #   PAC       : probability of alternative class for each object
  #   PCAfits   : if not NULL, PCA fits to each class, estimated from
  #               the training data but also useful for new data.
  #   figparams : parameters for computing fig, can be used for
  #               new data.
  #   fig       : distance of each object i from each class g.
  #   farness   : farness of each object to its given class.
  #   ofarness  : For each object i, its lowest farness to any
  #               class, including its own. Always exists.
  #
  keepPCA <- TRUE; prec <- 1e-10  ####PROBABLY TO BE REMOVED
  #
  # Subsetting to the same subset (of variables and observation) on which pamr fit works on.

  X <- as.matrix(X) # in case it is a data frame
  
  #NAs check
  if (nrow(X) == 1) X <- t(X) #one observation feeded
  if (sum(is.na(as.vector(X))) > 0) {
    stop("The coordinate matrix X has NA's.")
  }
  
  n <- nrow(X)
  d <- ncol(X)
  if (n < 2) stop("The training data should have more than one case.") #WTF YOU TRASNPOSED IT EARLIER

  y=as.factor(data$y) #factorize the given classes

  # Check whether y and its levels are of the right form:
  checked <- checkLabels(y, n, training = TRUE) #PROBABLY SHOULD RE DIG DEEP TO UNDERSTAND THIS FUNCTION
  
  # If it did not stop: yint has length n, and its values are in
  # 1, ..., nlab without gaps. It is NA where y is: outside indsv.
  lab2int <- checked$lab2int #retrieves function that form labels levels assign integers
  indsv   <- checked$indsv #vector contains the indices of the entries of the response variable y that can be visualized, i.e., are not NA.
  levels  <- checked$levels #unique labels (or levels) of the response variable y that actually occur in the data
  nlab    <- length(levels)
  yint    <- lab2int(y) #given label (true) as integer
  yintv   <- yint[indsv] #subsetting yint variable to non NAs
  classSizes <- rep(0, nlab)  ###PROBABLY TO BE REMOVED
  for (g in seq_len(nlab)) {classSizes[g] <- sum(yintv == g)} ### PROBABLY TO BE REMOVED
  # classSizes
  #
  # Check matrix of posterior probabilities:

  if (length(dim(probs)) != 2) stop("probs should be a matrix.")
  if (nrow(probs) != n) stop(paste0(
    "The matrix probs should have ", n, " rows"))
  if (ncol(probs) != nlab) stop(paste0(
    "The matrix probs should have ", nlab, " columns"))
  if (any(is.na(probs))) stop("probs should not have any NA's.")
  #
  # Compute prediction for all objects in the training data:
  #
  predint <- apply(probs[, , drop = FALSE], 1, which.max) 
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
  # Compute farness:
  #

  if (any(is.na(distToClasses))) { ##CHECK PUT FOR DEVELOPING REASONS PROBABLY COULD REMOVE
    stop("distToclasses has NAs")
  }

  initfig<-(distToClasses) #minus because wrt to paper is with opposite sign here

  rd=pamrfit$nonzero[ii] #getting reducted dimension
  farout <- compFarness(type = "affine", testdata = FALSE, yint = yint,
                        nlab = nlab, X = NULL, fig = initfig,
                        d = NULL, figparams = NULL) #DON'T REALLYU KNOW IF TO FEED OR NOT D

  figparams <- farout$figparams
  figparams$ncolX <- d
  #figparams$computeMD <- computeMD check what is that, probabbly related only to neuralnet
  #figparams$classMS <- classMS check what is that, probbably related to neuralnet
  #figparams$PCAfits <- farout$PCAfits #only for neuralnet??


  ####################################################
  # calculating pairwise distances, for additional visualization feature

  pw_mdS2 <-function(x, sd, prior, weight) { #pairwise mahalobis squared
    if(! missing(weight)) {
      posid <- (weight > 0)
      if(any(posid)) {
        weight <- sqrt(weight[posid])
        x <- x[posid,  , drop = FALSE] * weight #get only positions non zero positions
      }
      else {
        mat <- outer(rep(1, ncol(x)), log(prior), "*")
        dimnames(mat) <- list(NULL, dimnames(centroids)[[2]])
        return(mat)
      }
    }
    p=ncol(t(x))
    n=nrow(t(x))
    pwd=matrix(NA, nrow=n, ncol=n)
    sd=sd[posid]
    for (i in 1:n){
      pwd[,i]=mahalanobis(t(x),t(x)[i,],cov=diag(sd^2))
    }
    pwd
  }

  #pwd=pw_mdS2(xtest, sd, weight=posid)
  ###############################################

  return(list(X = X,
              yint = yint,
              y = levels[yint],
              levels = levels,
              predint = predint,
              pred = levels[predint],
              altint = altint,
              altlab = levels[altint],
              PAC = PAC,
              figparams = figparams,
              fig = farout$fig,
              farness = farout$farness,
              ofarness = farout$ofarnes,
              initfig=initfig, #INITFIG ADDED FOR TEST
              posid=posid, #added for test
              sd=sd#pwd=pwd #pairwise matrix distance for mds
              ))
}
