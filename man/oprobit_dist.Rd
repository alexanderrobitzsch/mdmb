%% File Name: oprobit_dist.Rd
%% File Version: 0.07

\name{oprobit_dist}
\alias{oprobit_dist}
\alias{doprobit}
\alias{fit_oprobit}

\alias{coef.fit_oprobit}
\alias{logLik.fit_oprobit}
\alias{summary.fit_oprobit}
\alias{vcov.fit_oprobit}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ordinal Probit Models
}

\description{
Fits and evaluates the ordinal probit model.
}

\usage{
#---- ordinal probit model
doprobit(x, thresh, max_val=99)

fit_oprobit(x, par_init=NULL, weights=NULL)
\method{coef}{fit_oprobit}(object, ...)
\method{logLik}{fit_oprobit}(object, ...)
\method{summary}{fit_oprobit}(object, digits=4, file=NULL, ...)
\method{vcov}{fit_oprobit}(object, ...)

}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector
}
  \item{thresh}{
Vector of thresholds
}
\item{max_val}{Maximum value for computing thresholds}
\item{par_init}{Optional vector of initial parameters}
\item{weights}{Optional vector of sampling weights}
\item{object}{Object of class \code{fit_yjt_scaled} or \code{fit_t_scaled}}
\item{digits}{Number of digits used for rounding in \code{summary}}
\item{file}{File name for the \code{summary} to be sunk into}
\item{\dots}{Further arguments to be passed}
}


%\details{
%}


\value{
Vector or an object of fitted distribution depending on the called function
}

%\references{
%xxxx
%}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{oprobit_regression}} for fitting a regression model in which
the response variable follows an ordinal probit model.
}

%% \code{\link[car:yjPower]{car::yjPower}}


\examples{
#############################################################################
# EXAMPLE 1: Fit an ordinal probit distribution
#############################################################################

#-- simulate data
set.seed(987)
N <- 1500
# define thresholds
thresh <- c(0,.3, .7, 1.6)
# latent continuous data
yast <- stats::rnorm(N)
# discretized ordinal data
x <- as.numeric( cut( yast, c(-Inf,thresh,Inf) ) ) - 1

#-- fit ordinal probit distribution
mod <- mdmb::fit_oprobit(x=x)
summary(mod)
logLik(mod)
vcov(mod)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
