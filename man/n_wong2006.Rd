% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_wong2006.R
\name{n_wong2006}
\alias{n_wong2006}
\title{Compute grain roughness following Wong and Parker (2006)}
\usage{
n_wong2006(grain, restrict = TRUE)
}
\arguments{
\item{grain}{grain size (d90) in millimeters. The original model was calibrated
for 0.38 mm < d90 < 28.65 mm}

\item{restrict}{allows for function parameters to restrict certain values. Type bool. Default TRUE.}
}
\value{
Manning's n
}
\description{
\code{n_wong2006} calculate Manning's n using the Wong and Parker (2006) method
for estimating grain roughness
}
\examples{
# Result: Manning's n of 0.022
n_wong2006(20)

# Result: Manning's n of 0.013
n_wong2006(0.82)

# Result: Manning's n of 0.021
n_wong2006(12)

# Result: Grain must be within 0.38 and 28.65 mm.
n_wong2006(30)

}
\references{
Wong, M., and G. Parker. 2006. Reanalysis and Correction of Bed-load Relation
of Meyer-Peter and Muller Using Their Own Database. Journal of Hydraulic
Engineering. American Society of Civil Engineers, Vol. 132 (11), pp. 1159-1168.
}
