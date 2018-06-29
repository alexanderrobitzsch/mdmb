%% File Name: yjt_dist.Rd
%% File Version: 0.36

\name{yjt_dist}
\alias{yjt_dist}
\alias{yj_trafo}
\alias{yj_antitrafo}
\alias{dt_scaled}
\alias{rt_scaled}
\alias{fit_t_scaled}
\alias{coef.fit_t_scaled}
\alias{logLik.fit_t_scaled}
\alias{summary.fit_t_scaled}
\alias{vcov.fit_t_scaled}
\alias{dyjt_scaled}
\alias{ryjt_scaled}
\alias{fit_yjt_scaled}
\alias{coef.fit_yjt_scaled}
\alias{logLik.fit_yjt_scaled}
\alias{summary.fit_yjt_scaled}
\alias{vcov.fit_yjt_scaled}
\alias{bc_trafo}
\alias{bc_antitrafo}
\alias{dbct_scaled}
\alias{rbct_scaled}
\alias{fit_bct_scaled}
\alias{coef.fit_bct_scaled}
\alias{logLik.fit_bct_scaled}
\alias{summary.fit_bct_scaled}
\alias{vcov.fit_bct_scaled}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scaled \eqn{t} Distribution with Yeo-Johnson and Box-Cox Transformations
}

\description{
Collection of functions for the Yeo-Johnson transformation
(Yeo & Johnson, 2000) and the corresponding distribution family of scaled
\eqn{t} distribution with and without Yeo-Johnson transformation
(see Details). The Box-Cox transformation (\code{bc}; Sakia, 1992)
is also included in this collection of functions.
}

\usage{
# Yeo-Johnson transformation and its inverse transformation
yj_trafo(y, lambda)
yj_antitrafo(y, lambda)

#---- scaled t distribution with Yeo-Johnson transformation
dyjt_scaled(x, location=0, shape=1, lambda=1, df=Inf, log=FALSE)
ryjt_scaled(n, location=0, shape=1, lambda=1, df=Inf)

fit_yjt_scaled(x, df=Inf, par_init=NULL, lambda_fixed=NULL, weights=NULL)
\method{coef}{fit_yjt_scaled}(object, ...)
\method{logLik}{fit_yjt_scaled}(object, ...)
\method{summary}{fit_yjt_scaled}(object, digits=4, file=NULL, ...)
\method{vcov}{fit_yjt_scaled}(object, ...)

# Box-Cox transformation and its inverse transformation
bc_trafo(y, lambda)
bc_antitrafo(y, lambda)

#---- scaled t distribution with Box-Cox transformation
dbct_scaled(x, location=0, shape=1, lambda=1, df=Inf, log=FALSE)
rbct_scaled(n, location=0, shape=1, lambda=1, df=Inf)

fit_bct_scaled(x, df=Inf, par_init=NULL, lambda_fixed=NULL, weights=NULL)
\method{coef}{fit_bct_scaled}(object, ...)
\method{logLik}{fit_bct_scaled}(object, ...)
\method{summary}{fit_bct_scaled}(object, digits=4, file=NULL, ...)
\method{vcov}{fit_bct_scaled}(object, ...)

#---- scaled t distribution
dt_scaled(x, location=0, shape=1, df=Inf, log=FALSE)
rt_scaled(n, location=0, shape=1, df=Inf)

fit_t_scaled(x, df=Inf, par_init=NULL, weights=NULL)
\method{coef}{fit_t_scaled}(object, ...)
\method{logLik}{fit_t_scaled}(object, ...)
\method{summary}{fit_t_scaled}(object, digits=4, file=NULL, ...)
\method{vcov}{fit_t_scaled}(object, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Numeric vector
}
  \item{lambda}{
Transformation parameter \eqn{\lambda} for Yeo-Johnson transformation
}
\item{x}{Numeric vector}
\item{location}{Location parameter of (transformed) scaled \eqn{t} distribution}
\item{shape}{Shape parameter of (transformed) scaled \eqn{t} distribution}
\item{df}{Degrees of freedom of (transformed) scaled \eqn{t} distribution}
\item{log}{Logical indicating whether logarithm of the density should be
computed}
\item{n}{Number of observations to be simulated}
\item{par_init}{Optional vector of initial parameters}
\item{lambda_fixed}{Optional value for fixed \eqn{\lambda} parameter}
\item{weights}{Optional vector of sampling weights}
\item{object}{Object of class \code{fit_yjt_scaled} or \code{fit_t_scaled}}
\item{digits}{Number of digits used for rounding in \code{summary}}
\item{file}{File name for the \code{summary} to be sunk into}
\item{\dots}{Further arguments to be passed}
}

\details{
Let \eqn{g_\lambda} be the Yeo-Johnson transformation. A random variable \eqn{X}
is distribution as Scaled \eqn{t} with Yeo-Johnson transformation with location
\eqn{\mu}, scale \eqn{\sigma} and transformation parameter \eqn{\lambda}
iff \eqn{X=g_\lambda ( \mu + \sigma Z ) } and \eqn{Z} is \eqn{t} distributed
with \code{df} degrees of freedom.
}


\value{
Vector or an object of fitted distribution depending on the called function
}

\references{
Sakia, S. M. (1992). The Box-Cox transformation technique: A review.
\emph{The Statistician, 41}, 169-178.

Yeo, I.-K., & Johnson, R. (2000). A new family of power transformations to
improve normality or symmetry. \emph{Biometrika, 87}, 954-959.
}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{yjt_regression}} for fitting a regression model in which
the response variable is distributed according to the scaled \eqn{t}
distribution with Yeo-Johnson transformation.

See \code{car::yjPower} for fitting the Yeo-Johnson
transformation in the \pkg{car} package. See \code{car::bcPower} for the
Box-Cox transformation.

The scaled \eqn{t} distribution can be also found in
\code{metRology::dt.scaled} (\pkg{metRology} package).

See \code{\link[stats:dt]{stats::dt}} for the \eqn{t} distribution.

See the \pkg{fitdistrplus} package or the general
\code{\link[stats4:mle]{stats4::mle}} function
for fitting several distributions in \R.
}

%% \code{\link[car:yjPower]{car::yjPower}}


\examples{
#############################################################################
# EXAMPLE 1: Transforming values according to Yeo-Johnson transformation
#############################################################################

# vector of y values
y <- seq(-3,3, len=100)

# non-negative lambda values
plot( y, mdmb::yj_trafo( y, lambda=1 ), type="l", ylim=8*c(-1,1),
           ylab=expression( g[lambda] (y) ) )
lines( y, mdmb::yj_trafo( y, lambda=2 ), lty=2 )
lines( y, mdmb::yj_trafo( y, lambda=.5 ), lty=3 )
lines( y, mdmb::yj_trafo( y, lambda=0 ), lty=4 )

# non-positive lambda values
plot( y, mdmb::yj_trafo( y, lambda=-1 ), type="l", ylim=8*c(-1,1),
           ylab=expression(g[lambda] (y) ) )
lines( y, mdmb::yj_trafo( y, lambda=-2 ), lty=2 )
lines( y, mdmb::yj_trafo( y, lambda=-.5 ), lty=3 )
lines( y, mdmb::yj_trafo( y, lambda=0 ), lty=4 )

\dontrun{
#############################################################################
# EXAMPLE 2: Density of scaled t distribution
#############################################################################

# define location and scale parameter
m0 <- 0.3
sig <- 1.5
#-- compare density of scaled t distribution with large degrees of freedom
#   with normal distribution
y1 <- mdmb::dt_scaled( y, location=m0, shape=sig, df=100 )
y2 <- stats::dnorm( y, mean=m0, sd=sig )
max(abs(y1-y2))

#############################################################################
# EXAMPLE 3: Simulating and fitting the scaled t distribution
#############################################################################

#-- simulate data with 10 degrees of freedom
set.seed(987)
df0 <- 10    # define degrees of freedom
x <- mdmb::rt_scaled( n=1E4, location=m0, shape=sig, df=df0 )
#** fit data with df=10 degrees of freedom
fit1 <- mdmb::fit_t_scaled(x=x, df=df0 )
#** compare with fit from normal distribution
fit2 <- mdmb::fit_t_scaled(x=x, df=Inf )  # df=Inf is the default

#-- some comparisons
coef(fit1)
summary(fit1)
logLik(fit1)
AIC(fit1)
AIC(fit2)

#############################################################################
# EXAMPLE 4: Simulation and fitting of scaled t distribution with
#            Yeo-Johnson transformation
#############################################################################

# define parameters of transformed scaled t distribution
m0 <- .5
sig <- 1.5
lam <- .5

# evaluate density
x <- seq( -5, 5, len=100 )
y <- mdmb::dyjt_scaled( x, location=m0, shape=sig, lambda=lam )
graphics::plot( x, y, type="l")

# transform original values
mdmb::yj_trafo( y=x, lambda=lam )

#** simulate data
set.seed(987)
x <- mdmb::ryjt_scaled(n=3000, location=m0, shape=sig, lambda=lam )
graphics::hist(x, breaks=30)

#*** Model 1: Fit data with lambda to be estimated
fit1 <- mdmb::fit_yjt_scaled(x=x)
summary(fit1)
coef(fit1)

#*** Model 2: Fit data with lambda fixed to simulated lambda
fit2 <- mdmb::fit_yjt_scaled(x=x, lambda_fixed=lam)
summary(fit2)
coef(fit2)

#*** Model 3: Fit data with lambda fixed to 1
fit3 <- mdmb::fit_yjt_scaled(x=x, lambda_fixed=1)

#-- compare log-likelihood values
logLik(fit1)
logLik(fit2)
logLik(fit3)

#############################################################################
# EXAMPLE 5: Approximating the chi square distribution
#            with yjt and bct distribution
#############################################################################

#-- simulate data
set.seed(987)
n <- 3000
df0 <- 5
x <- stats::rchisq( n=n, df=df0 )

#-- plot data
graphics::hist(x, breaks=30)

#-- fit data with yjt distribution
fit1 <- mdmb::fit_yjt_scaled(x)
summary(fit1)
c1 <- coef(fit1)

#-- fit data with bct distribution
fit2 <- mdmb::fit_bct_scaled(x)
summary(fit2)
c2 <- coef(fit2)
# compare log-likelihood values
logLik(fit1)
logLik(fit2)

#-- plot chi square distribution and approximating yjt distribution
y <- seq( .01, 3*df0, len=100 )
dy <- stats::dchisq( y, df=df0 )
graphics::plot( y, dy, type="l", ylim=c(0, max(dy) )*1.1 )
# approximation with scaled t distribution and Yeo-Johnson transformation
graphics::lines( y, mdmb::dyjt_scaled(y, location=c1[1], shape=c1[2], lambda=c1[3]),
                     lty=2)
# approximation with scaled t distribution and Box-Cox transformation
graphocs::lines( y, mdmb::dbct_scaled(y, location=c2[1], shape=c2[2], lambda=c2[3]),
                     lty=3)
# appoximating normal distribution
graphics::lines( y, stats::dnorm( y, mean=df0, sd=sqrt(2*df0) ), lty=4)
graphics::legend( .6*max(y), .9*max(dy), c("chi square", "yjt", "bct", "norm"),
                     lty=1:4)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
