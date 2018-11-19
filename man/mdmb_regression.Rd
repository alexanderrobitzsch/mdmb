%% File Name: mdmb_regression.Rd
%% File Version: 0.565

\name{mdmb_regression}

\alias{logistic_regression}
\alias{coef.logistic_regression}
\alias{vcov.logistic_regression}
\alias{logLik.logistic_regression}
\alias{summary.logistic_regression}
\alias{predict.logistic_regression}

\alias{yjt_regression}
\alias{coef.yjt_regression}
\alias{vcov.yjt_regression}
\alias{logLik.yjt_regression}
\alias{summary.yjt_regression}
\alias{predict.yjt_regression}

\alias{bct_regression}
\alias{coef.bct_regression}
\alias{vcov.bct_regression}
\alias{logLik.bct_regression}
\alias{summary.bct_regression}
\alias{predict.bct_regression}

\alias{oprobit_regression}
\alias{coef.oprobit_regression}
\alias{vcov.oprobit_regression}
\alias{logLik.oprobit_regression}
\alias{summary.oprobit_regression}
\alias{predict.oprobit_regression}

%- Also NEED an '\alias' for EACH other topic documented here.


\title{
Several Regression Models with Prior Distributions and Sampling Weights
}

\description{
Several regression functions which allow for sampling weights and
prior distributions.

The function \code{yjt_regression} performs a linear regression in which the
response variable is transformed according to the Yeo-Johnson transformation
(Yeo & Johnson, 2000; see \code{\link{yjt_dist}}) and the residuals are
distributed following the scaled \eqn{t} distribution. The degrees of freedom
of the \eqn{t} distribution can be fixed or estimated (\code{est_df=TRUE}).
The function \code{bct_regression} has same functionality like the
Yeo-Johnson transformation but employs a Box-Cox transformation
of the outcome variable.

The Yeo-Johnson transformation can be extended by a probit transformation
(\code{probit=TRUE}) to cover the case of bounded variables on \eqn{[0,1]}.

The function \code{logistic_regression} performs logistic regression
for dichotomous data.

The function \code{oprobit_regression} performs ordinal probit regression
for ordinal polytomous data.
}

\usage{
#---- linear regression with Yeo-Johnson transformed scaled t distribution
yjt_regression(formula, data, weights=NULL, beta_init=NULL, beta_prior=NULL,
        df=Inf, lambda_fixed=NULL, probit=FALSE, est_df=FALSE,
        use_grad=2, h=1e-5, control=NULL)

\method{coef}{yjt_regression}(object, ...)
\method{logLik}{yjt_regression}(object, ...)
\method{predict}{yjt_regression}(object, newdata=NULL, trafo=TRUE,  ...)
\method{summary}{yjt_regression}(object, digits=4, file=NULL, ...)
\method{vcov}{yjt_regression}(object, ...)

#---- linear regression with Box-Cox transformed scaled t distribution
bct_regression(formula, data, weights=NULL, beta_init=NULL, beta_prior=NULL,
        df=Inf, lambda_fixed=NULL, est_df=FALSE, use_grad=2, h=1e-5, control=NULL)

\method{coef}{bct_regression}(object, ...)
\method{logLik}{bct_regression}(object, ...)
\method{predict}{bct_regression}(object, newdata=NULL, trafo=TRUE, ...)
\method{summary}{bct_regression}(object, digits=4, file=NULL, ...)
\method{vcov}{bct_regression}(object, ...)

#---- logistic regression
logistic_regression(formula, data, weights=NULL, beta_init=NULL,
         beta_prior=NULL, use_grad=2, h=1e-5, control=NULL)

\method{coef}{logistic_regression}(object, ...)
\method{logLik}{logistic_regression}(object, ...)
\method{predict}{logistic_regression}(object, newdata=NULL, ...)
\method{summary}{logistic_regression}(object, digits=4, file=NULL, ...)
\method{vcov}{logistic_regression}(object, ...)

#---- ordinal probit regression
oprobit_regression(formula, data, weights=NULL, beta_init=NULL,
        use_grad=2, h=1e-5, control=NULL, control_optim_fct=NULL)

\method{coef}{oprobit_regression}(object, ...)
\method{logLik}{oprobit_regression}(object, ...)
\method{predict}{oprobit_regression}(object, newdata=NULL, ...)
\method{summary}{oprobit_regression}(object, digits=4, file=NULL, ...)
\method{vcov}{oprobit_regression}(object, ...)

}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
Formula
}
  \item{data}{
Data frame. The dependent variable must be coded as 0 and 1.
}
  \item{weights}{
Optional vector of sampling weights
}
  \item{beta_init}{
Optional vector of initial regression coefficients
}
\item{beta_prior}{Optional list containing priors of all parameters
(see Examples for definition of this list).}
\item{df}{Fixed degrees of freedom for scaled \eqn{t} distribution}
\item{lambda_fixed}{Optional fixed value for \eqn{\lambda} for scaled
  \eqn{t} distribution with Yeo-Johnson transformation}
\item{probit}{Logical whether probit transformation should be employed for
bounded outcome in \code{yjt_regression}}
\item{est_df}{Logical indicating whether degrees of freedom in
\eqn{t} distribution should be estimated.}
 \item{use_grad}{Computation method for gradients in \code{stats::optim}.
      The value \code{0} is the internal approximation of
      \code{stats::optim} and applies the settings in \pkg{mdmb} (\eqn{\le}0.3).
      The specification \code{use_grad=1} uses the calculation of
 the gradient in \code{CDM::numerical_Hessian}. The value \code{2} is
 usually the most efficient calculation of the gradient.}
\item{h}{Numerical differentiation parameter.}
  \item{control}{
Optional arguments to be passed to optimization function
\code{\link[stats:optim]{stats::optim}}
}
\item{control_optim_fct}{Optional control argument for gradient in
optimization}
\item{object}{Object of class \code{logistic_regression}}
\item{newdata}{Design matrix for \code{predict} function}
\item{trafo}{Logical indicating whether fitted values should be on the
    transformed metric (\code{trafo=TRUE}) or the original metric
(\code{trafo=FALSE})}
\item{digits}{Number of digits for rounding}
\item{file}{File name if the \code{summary} output should be sunk into a file.}
\item{\dots}{Further arguments to be passed.}
}


%%\details{
%%  ~~ If necessary, more details than the description above ~~
%% }


\value{
List containing values

\item{coefficients}{Estimated regression coefficients}
\item{vcov}{Estimated covariance matrix}
\item{partable}{Parameter table}
\item{y}{Vector of values of dependent variable}
\item{Xdes}{Design matrix}
\item{weights}{Sampling weights}
\item{fitted.values}{Fitted values in metric of probabilities}
\item{linear.predictor}{Fitted values in metric of logits}
\item{loglike}{Log likelihood value}
\item{logprior}{Log prior value}
\item{logpost}{Log posterior value}
\item{deviance}{Deviance}
\item{loglike_case}{Case-wise likelihood}
\item{ic}{Information criteria}
\item{R2}{Pseudo R-square value according to McKelvey and Zavoina}
}

\references{
McKelvey, R., & Zavoina, W. (1975). A statistical model for the analysis of
ordinal level dependent variables.
\emph{Journal of Mathematical Sociology, 4}, 103-120.

Yeo, I.-K., & Johnson, R. (2000). A new family of power transformations to
improve normality or symmetry. \emph{Biometrika, 87}, 954-959.
}

\author{
Alexander Robitzsch
}


%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{yjt_dist}} or \code{car::yjPower}
for functions for the Yeo-Johnson transformation.

See \code{\link[stats:lm]{stats::lm}} and
\code{\link[stats:glm]{stats::glm}} for linear and logistic
regression models.
}
%% \code{\link[car:yjPower]{car::yjPower}}


\examples{
#############################################################################
# EXAMPLE 1: Simulated example logistic regression
#############################################################################

#--- simulate dataset
set.seed(986)
N <- 500
x <- stats::rnorm(N)
y <- 1*( stats::runif(N) < stats::plogis( -0.8 + 1.2 * x ) )
data <- data.frame( x=x, y=y )

#--- estimate logistic regression with mdmb::logistic_regression
mod1 <- mdmb::logistic_regression( y ~ x, data=data )
summary(mod1)

\dontrun{
#--- estimate logistic regression with stats::glm
mod1b <- stats::glm( y ~ x, data=data, family="binomial")
summary(mod1b)

#--- estimate logistic regression with prior distributions
b0 <- list( "dnorm", list(mean=0, sd=100) )  # first parameter
b1 <- list( "dcauchy", list(location=0, scale=2.5) )   # second parameter
beta_priors <- list( b0, b1 )  # order in list defines priors for parameters
#* estimation
mod2 <- mdmb::logistic_regression( y ~ x, data=data, beta_prior=beta_priors )
summary(mod2)

#############################################################################
# EXAMPLE 2: Yeo-Johnson transformed scaled t regression
#############################################################################

#*** create simulated data
set.seed(9865)
n <- 1000
x <- stats::rnorm(n)
y <- .5 + 1*x + .7*stats::rt(n, df=8 )
y <- mdmb::yj_antitrafo( y, lambda=.5 )
dat <- data.frame( y=y, x=x )
# display data
graphics::hist(y)

#--- Model 1: fit regression model with transformed normal distribution (df=Inf)
mod1 <- mdmb::yjt_regression( y ~ x, data=dat )
summary(mod1)

#--- Model 2: fit regression model with transformed scaled t distribution (df=10)
mod2 <- mdmb::yjt_regression( y ~ x, data=dat, df=10)
summary(mod2)

#--- Model 3: fit regression model with transformed normal distribution (df=Inf)
#             and fixed transformation parameter lambda of .5
mod3 <- mdmb::yjt_regression( y ~ x, data=dat, lambda_fixed=.5)
summary(mod3)

#--- Model 4: fit regression model with transformed normal distribution (df=Inf)
#             and fixed transformation parameter lambda of 1
#             -> This model corresponds to least squares regression
mod4 <- mdmb::yjt_regression( y ~ x, data=dat, lambda_fixed=1)
summary(mod4)

# fit with lm function
mod4b <- stats::lm( y ~ x, data=dat )
summary(mod4b)

#--- Model 5: fit regression model with estimated degrees of freedom
mod5 <- mdmb::yjt_regression( y ~ x, data=dat, est_df=TRUE)
summary(mod5)

#** compare log-likelihood values
logLik(mod1)
logLik(mod2)
logLik(mod3)
logLik(mod4)
logLik(mod4b)
logLik(mod5)

#############################################################################
# EXAMPLE 3: Regression with Box-Cox and Yeo-Johnson transformations
#############################################################################

#*** simulate data
set.seed(985)
n <- 1000
x <- stats::rnorm(n)
y <- .5 + 1*x + stats::rnorm(n, sd=.7 )
y <- mdmb::bc_antitrafo( y, lambda=.5 )
dat <- data.frame( y=y, x=x )

#--- Model 1: fit regression model with Box-Cox transformation
mod1 <- mdmb::bct_regression( y ~ x, data=dat )
summary(mod1)
#--- Model 2: fit regression model with Yeo-Johnson transformation
mod2 <- mdmb::yjt_regression( y ~ x, data=dat )
summary(mod2)
#--- compare fit
logLik(mod1)
logLik(mod2)

#############################################################################
# EXAMPLE 4: Ordinal probit regression
#############################################################################

#--- simulate data
set.seed(987)
N <- 1500
x <- stats::rnorm(N)
z <- stats::rnorm(N)
# regression coefficients
b0 <- -.5 ; b1 <- .6 ; b2 <- .1
# vector of thresholds
thresh <- c(-1, -.3, 1)
yast <- b0 + b1 * x + b2*z + stats::rnorm(N)
y <- as.numeric( cut( yast, c(-Inf,thresh,Inf) ) ) - 1
dat <- data.frame( x=x, y=y, z=z )

#--- probit regression
mod <- mdmb::oprobit_regression( formula=y ~ x + z + I(x*z), data=dat)
summary(mod)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
