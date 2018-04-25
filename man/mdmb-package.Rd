%% File Name: mdmb-package.Rd
%% File Version: 0.22

\name{mdmb-package}
\alias{mdmb-package}
\alias{mdmb}
\docType{package}

\title{
Model Based Treatment of Missing Data  
}

\description{
    Contains model-based treatment of missing data for regression models 
    with missing values in covariates or the dependent variable 
    using maximum likelihood or Bayesian estimation (Ibrahim et al., 2005;
    <doi:10.1198/016214504000001844>). The regression model can be nonlinear
    (e.g., including interaction or quadratic effects). 
    Multiple imputation can be also conducted.
}

\details{

\itemize{

\item The maximum likelihood estimation of regression models with
missing values in covariates is implemented in \code{\link{frm_em}}.

\item Bayesian estimation and multiple imputation of regression models with
missing values in covariates is implemented in \code{\link{frm_fb}}. 

}



}

\author{
Alexander Robitzsch & Oliver Luedtke \cr 
IPN - Leibniz Institute for Science and Mathematics Education
at Kiel University \cr \cr
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de> \cr \cr
URL: \url{https://sites.google.com/site/alexanderrobitzsch/}
}


\references{
Ibrahim, J. G., Chen, M. H., Lipsitz, S. R., & Herring, A. H. (2005). 
Missing-data methods for generalized linear models: A comparative review. 
\emph{Journal of the American Statistical Association, 100}, 332-346.
}



\seealso{
The EM algorithm for the multivariate normal model is implemented in 
\code{norm2::emNorm} in the \pkg{norm2} package. 
A corresponding MCMC algorithm can be
found in the \code{norm2::mcmcNorm} function.

See the \pkg{lavaan}, \pkg{OpenMx} or \pkg{sem} packages for full information 
maximum likelihood approaches for handling missing data for multivariate
normal distributions, linear regression models, and, more generally, 
structural equation modeling with missing data.

Structural equation models with missing data can be also estimated with a
two-stage procedure. In a first stage, a mean vector and a covariance matrix
is estimated (possibly with auxiliary variables) and in the second stage,
the structural equation model is estimated on the previously obtained
mean vector and covariance matrix. The procedure is implemented in the  
\code{semTools::twostage} function in the
\pkg{semTools} package.
}

% \code{\link[semTools:twostage]{semTools::twostage}}
% \code{\link[norm2:emNorm]{norm2::emNorm}}
% \code{\link[norm2:mcmcNorm]{norm2::mcmcNorm}}

\examples{
  ##  
  ##  |\  /||~~\ |\  /||~~\
  ##  | \/ ||   || \/ ||--<
  ##  |    ||__/ |    ||__/
  ##                       

  ##  
  ##  > library(mdmb)
  ##  * mdmb 0.0-13 (2017-01-15)
  ##      
}

\keyword{package}
