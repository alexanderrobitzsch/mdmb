%% File Name: mdmb-package.Rd
%% File Version: 0.401

\name{mdmb-package}
\alias{mdmb-package}
\alias{mdmb}
\docType{package}


\title{
\packageTitle{mdmb} 
}

\description{
\packageDescription{mdmb}
}

\author{
\packageAuthor{mdmb}

Maintainer: \packageMaintainer{mdmb}
}


\details{

\itemize{

\item The maximum likelihood estimation of regression models with
missing values in covariates is implemented in \code{\link{frm_em}}.
Available regression models are linear regression, logistic regression,
ordinal probit regression and models with Box-Cox or Yeo-Johnson transformed 
normally distributed outcomes. The factorization based regression model
also allow the inclusion of latent variables and measurement error prone
covariates.  

\item Bayesian estimation and multiple imputation of regression models with
missing values in covariates is implemented in \code{\link{frm_fb}}. The same
regression models like in \code{\link{frm_em}} can be specified. Moreover,
multilevel models can also be specified with Bayesian estimation. 
The function \code{\link{frm_fb}} allows substantive model compatible multiple imputation.
}



}



\references{
Ibrahim, J. G., Chen, M. H., Lipsitz, S. R., & Herring, A. H. (2005). 
Missing-data methods for generalized linear models: A comparative review. 
\emph{Journal of the American Statistical Association, 100}, 332-346.

Luedtke, O., Robitzsch, A., & West, S. (XXXX). Regression models involving nonlinear 
effects with missing data: A sequential modeling approach using Bayesian estimation. 
\emph{Psychological Methods, xx}(xx), xxx-xxx. 
\doi{10.1037/met0000233}

Luedtke, O., Robitzsch, A., & West, S. (XXXX). Analysis of interactions and nonlinear 
effects with missing data: A factored regression modeling approach using maximum 
likelihood estimation. \emph{Multivariate Behavioral Research, xx}(xx), xxx-xxx. 
\doi{10.1080/00273171.2019.1640104}
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
\code{semTools::twostage} function in the \pkg{semTools} package.
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
