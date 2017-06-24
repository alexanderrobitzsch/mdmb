\name{data.mb}
\alias{data.mb}

\alias{data.mb01}
\alias{data.mb02}

\docType{data}

\title{
Example Datasets for \pkg{mdmb} Package
}

\description{
Example datasets for \pkg{mdmb} package.
}


\usage{
data(data.mb01)
data(data.mb02)
}


\format{
\itemize{

\item Dataset \code{data.mb01}. Simulated dataset with missing values.
Variables \code{Y}, \code{X} and \code{Z} are continuous.

\code{List of 2} \cr
\code{ $ complete:'data.frame':       4000 obs. of  3 variables:} \cr
\code{  ..$ X: num [1:4000] -1.08 0.57 -0.32 0.34 1.21 -0.44 -1.07 -0.29 0.76 -1.75 ...} \cr
\code{  ..$ Z: num [1:4000] -0.02 0.26 -1.45 1.24 0.98 -2.36 0.84 -1.08 -0.15 -1.36 ...} \cr
\code{  ..$ Y: num [1:4000] 0.88 1.75 -0.82 -1.81 -1.58 -3.34 -3.35 -0.29 1.47 0.23 ...} \cr
\code{ $ missing :'data.frame':       4000 obs. of  3 variables:} \cr
\code{  ..$ X: num [1:4000] -1.08 0.57 NA NA 1.21 NA -1.07 -0.29 0.76 NA ...} \cr
\code{  ..$ Z: num [1:4000] -0.02 0.26 -1.45 1.24 0.98 -2.36 0.84 -1.08 -0.15 -1.36 ...} \cr
\code{  ..$ Y: num [1:4000] 0.88 1.75 -0.82 -1.81 -1.58 -3.34 -3.35 -0.29 1.47 0.23 ...} \cr



\item Dataset \code{data.mb02}. Simulated dataset with missing values. The
variables \code{Z} and \code{Y} are dichotomous.

\code{List of 2} \cr
\code{ $ complete:'data.frame':       2000 obs. of  3 variables:} \cr
\code{  ..$ X: num [1:2000] -0.93 0.3 -0.93 0.7 0.52 -1.38 -0.14 0.09 0.23 -1.64 ...} \cr
\code{  ..$ Z: num [1:2000] 1 0 1 0 0 1 1 1 1 1 ...} \cr
\code{  ..$ Y: num [1:2000] 1 1 0 1 1 0 1 0 0 1 ...} \cr
\code{ $ missing :'data.frame':       2000 obs. of  3 variables:} \cr
\code{  ..$ X: num [1:2000] -0.93 0.3 -0.93 0.7 0.52 NA -0.14 0.09 0.23 -1.64 ...} \cr
\code{  ..$ Z: num [1:2000] 1 0 1 NA NA 1 NA 1 1 1 ...} \cr
\code{  ..$ Y: num [1:2000] 1 1 0 1 1 0 1 0 0 1 ...} \cr


}
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
%\examples{
%data(data.ma01)
%## maybe str(data.ma01) ; plot(data.ma01) ...
%}
\keyword{datasets}
