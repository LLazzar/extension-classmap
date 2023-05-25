\name{mdscolorscale}
\alias{mdscolorscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a 2-d MDS color-scaled scatterplot for a classification
}
\description{
Draw a 2D scatterplot using classified observations, employing MDS for dimensional reduction. Utilizes output from a \code{vcr.*.*} function in the classmap package or supplementary \code{vcr.*.*} functions in this complementary package. Input is a dist or matrix object providing pairwise distances aligned with the trained classifier's view on data. Point border color discriminates among true classes. Point fill color intensity, mapped to \code{PAC}/\code{Sil} values, conveys \code{Sil} value information (representing classifier conviction in predicting the correct class): a less intense fill (approaching white) indicates lower \code{Sil} values (higher \code{PAC}) and lower convinction in classyfing the point to the right class.
}

\usage{
colorscale(vcrout, diss, classLabels = NULL, classCols=NULL,
          main=NULL, size=8, bordersize=1.2, showLegend =TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{vcrout}{output of a \code{vcr.*.train} or \code{vcr.*.newdata}. Required. Defines set of labeled data to evaluate and provides needed quantities for visualization (especially \code{PAC}).}
\item{diss}{a distance structure akin to the output from \code{dist}, or a full symmetric matrix housing pairwise dissimilarities related to the observations evaluated in vcrout. Observation order must mirror that used in creating the \code{vcrout} object. Distance matrix should align with the once-trained classifier's data point space perception.}
\item{classLabels}{the labels (levels) of the classes. If \code{NULL}, they are taken from \code{vcrout}.}
\item{classCols}{a list of colors for the classes. There should be at least as many as there are levels. If \code{NULL} a default dark tone palette is used. Darker tones are recommended for better plot interpretation.}
\item{main}{title for the plot. If \code{NULL}, a default title is used.}
\item{size}{sets the size of the plotted points.}
\item{bordersize}{sets the thickness of the border around each data point. The color of the border is used to discriminate among the different true classes of the points.}
\item{showLegend}{if TRUE, a legend is shown to the right of the plot.}


}


\value{
A plotly plot containing the descripted visualization.
}

\references{--add paper at release}


\author{
 Lazzari. L.
}

\seealso{
\code{\link{vcr.custom.train}}, \code{\link{vcr.custom.newdata}}, \code{vcr.*.*} from classmap package.}


\examples{
# For examples, we refer to the vignette:
\dontrun{
vignette("MAKE EXAMPLE VIGNETTE")
}
}
