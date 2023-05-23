\name{mdscolorscale}
\alias{mdscolorscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a 2-d MDS color-scaled scatterplot for a classification
}
\description{
Draw a 2D scatterplot using classified observations, employing MDS for dimensional reduction. Utilizes outputs from \code{vcr.*.*} functions in the classmap package or supplementary functions \code{vcr} in this extension package. Input is a dist or matrix object providing pairwise distances aligned with trained classifier's view on data. Point border color discrimnates among true classes. Fill color intensity, mapped to \code{PAC}/\code{Sil} values, conveys \code{Sil} value information (representing classifier conviction in predicting the correct class): a less intense fill (approaching white) indicates lower \code{Sil} values (Higher \code{PAC}) and lower.
}

\usage{
colorscale(vcrout, diss, classLabels = NULL, classCols=NULL, main=NULL, size=8, bordersize=1.2, showLegen =TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{vcrout}{output of a vcr.*.train or vcr.*.newdata. Required. Defines set of data considered}
\item{diss}{a distance structure akin to the output from \code{dist}, or a full symmetric matrix housing pairwise dissimilarities related to the observations evaluated in vcrout. Observation order must mirror that used in creating the vcrout object. Distance matrix should align with the trained classifier's data point perception.}
\item{classLabels}{the labels (levels) of the classes. If \code{NULL}, they are taken from \code{vcrout}.}
\item{classCols}{a list of colors for the classes. There should be at least as many as there are levels. If NULL a default dark tone palette is used. Darker tones are recommended for better plot interpretation.}
\item{main}{title for the plot. If \code{NULL}, a default title is used.}
\item{size}{sets the size of the plotted points.}
\item{bordersize=1.2}{sets the thickness of the border around each data point. The color of the border is used to discriminate among the different true classes of the points.}
\item{main}{if TRUE, a legend is shown to the right of the plot.}


}


\value{
A plotly plot containing the descripted visualization
}
\references{--add paper at release}
}

\author{
 Lazzari. L, Raymaekers J., Rousseeuw P.J.
}

\seealso{
\code{\link{vcr.custom.newdata}}, \code{\link{classmap}}, \code{\link{silplot}}}
}

\examples{
# For examples, we refer to the vignette:
\dontrun{
vignette("MAKE EXAMPLE VIGNETTE")
}
}
