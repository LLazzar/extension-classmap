\name{vcr.custom.newdata}
\alias{vcr.custom.newdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare for visualization of a classification made by a ML classifier on new test data.
}
\description{
  Produces outputs for graphical displays of new data fitted by a trained a classifier algorithm of choice, using   the output of \code{\link{vcr.custom.train}} on the training data.
}
\usage{
vcr.custom.newdata(ynew = NULL, probs, vcr.custom.train.out,
                   newDistToClass=NULL)
}

\arguments{
\item{ynew}{factor with given/true class labels of each new case. It's advisable to factorize \code{ynew} in tandem with training data during initial preprocessing to avoid possible inconsistencies in the attribuition of the levels. It may may include fewer unique labels than the training set. Can be \code{NA} for some or all cases. If \code{NULL}, is assumed to be \code{NA} everywhere.}
\item{probs}{posterior probabilities predicted on the new data by the chosen classifier fitted on the training data.}
\item{vcr.custom.train.out}{output of \code{\link{vcr.custom.train}} on the training data on which the classifier has been fitted.}
\item{newDistToClass}{Optional. Matrix containing the distances of each new case to each possible class. The user should produce this matrix using a custom-devised metric that is coherent with how the once-trained classifier works and sees the data, i.e according to its formed decision space. The ordering for rows and columns is the same as in \code{probs}. It permits '\code{farness}' computation for each observation and enables the \code{classmap} visual.}
}

\value{
A list with components: \cr
\item{yintnew}{number of the given class of each case. Can contain \code{NA}'s.}
\item{ynew}{given class label of each case. Can contain \code{NA}'s.}
\item{levels}{levels of the response, from \code{vcr.svm.train.out}.}
\item{predint}{predicted class number of each case. Always exists.}
\item{pred}{predicted label of each case.}
\item{altint}{number of the alternative class. Among the classes different from the given class, it is the one with the highest posterior probability. Is \code{NA} for cases whose \code{ynew} is missing.}
\item{altlab}{alternative label if yintnew was given, else \code{NA}.}
\item{PAC}{probability of the alternative class. Is \code{NA} for cases whose \code{ynew} is missing.}
\item{fig}{distance of each case \eqn{i} from each class \eqn{g}. Always exists.}
\item{farness}{farness of each case from its given class. Is \code{NA} for cases whose \code{ynew} is missing.}
\item{ofarness}{for each case \eqn{i}, its lowest \eqn{fig[i,g]} to any class \eqn{g}. Always exists.}
}

\references{--add paper at release}


\author{
Lazzari. L.
}

\seealso{
\code{\link{vcr.custom.train}}, \code{\link{classmap::silplot}}, \code{\link{classmap:classmap}}, \code{\link{mdscolorscale}}
}

\examples{
# For examples, we refer to the vignette:
\dontrun{
vignette("make vignettes")
}
}
