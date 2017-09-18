%% File Name: frm.Rd
%% File Version: 0.62
%% File Last Change: 2017-08-21 10:44:19

\name{frm}
\alias{frm_em}
\alias{coef.frm_em}
\alias{vcov.frm_em}
\alias{logLik.frm_em}
\alias{summary.frm_em}
\alias{frm_fb}
\alias{coef.frm_fb}
\alias{plot.frm_fb}
\alias{vcov.frm_fb}
\alias{summary.frm_fb}
\alias{frm2datlist}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Factored Regression Model: Generalized Linear Regression Model with Missing Covariates
}


\description{
The factored regression model (FRM) allows the estimation of the linear regression model
(with normally distributed residuals) and the generalized logistic regression model
(logistic regression for dichotomous outcomes). Missing values in covariates are
handled by posing a conditional univariate distribution for each covariate. The
approach follows Ibrahim (1990), Ibrahim, Chen, Lipsitz and Herring (2005)
and Lee and Mitra (2016). Latent variables
and covariates with measurement error or multiple indicators can also be handled 
within this framework (see Examples 3, 4 and 5).

Missing values are handled by numerical integration in \code{frm_em} 
(see also Allison, 2012). The user has to specify an integration grid for each 
variable (defined in argument \code{nodes} for each model). 

Standard error estimates in \code{frm_em} are obtained by a numerical differentiation of 
the Fisher score function (see Jamshidian & Jennrich, 2000). 

The function \code{frm_fb} employs a fully Bayesian approach with noninformative
prior distribution. This function imputes missing values in the models from
the posterior distributions. Imputed datasets can be extracted by the
function \code{frm2datlist}.

The current functionality only support missing values on continuous covariates
(accomodating skewness and only positive values) and dichotomous covariates.
The handling of ordinal and nominal covariates will be included in future
\pkg{mdmb} package versions.
}


\usage{
# Factored regression model: Numerical integration with EM algorithm
frm_em(dat, dep, ind, weights = NULL, verbose = TRUE, maxiter = 500, conv_dev = 1e-08, 
         conv_parm = 1e-05, nodes_control = c(11,5), h = 1e-04, use_grad = 2)

\method{coef}{frm_em}(object, ...)
\method{logLik}{frm_em}(object, ...)
\method{summary}{frm_em}(object, digits=4, file=NULL, ...)
\method{vcov}{frm_em}(object, ...)

# Factored regression model: Fully Bayesian estimation
frm_fb(dat, dep, ind, weights=NULL, verbose=TRUE, data_init = NULL, iter=500, 
        burnin=100, Nimp=10, Nsave = 3000, refresh = 25, acc_bounds = c(.45,.50), 
        print_iter = 10)
	
\method{coef}{frm_fb}(object, ...)
\method{plot}{frm_fb}(x, idparm=NULL, ask=TRUE, ... )
\method{summary}{frm_fb}(object, digits=4, file=NULL, ...)
\method{vcov}{frm_fb}(object, ...)

frm2datlist(object)  # create list of imputed datasets
}
%- maybe also 'usage' for other objects documented here.


\arguments{
 \item{dat}{
Data frame
}
  \item{dep}{
List containing model specification for dependent variable. The list 
has arguments (see Examples)

\code{model}: String indicating the model type. Options are
\code{"linreg"} (wrapper to \code{\link[stats:lm]{stats::lm}}
or \code{\link[stats:lm.wfit]{stats::lm.wfit}}),
\code{"logistic"} (wrapper to \code{\link{logistic_regression}}), 
\code{"yjtreg"} (wrapper to \code{\link{yjt_regression}}),
\code{"bctreg"} (wrapper to \code{\link{bct_regression}})
 \cr
\code{formula}: An \R formula object. \cr
\code{nodes} (for \code{frm_em}): Vector containing the integration nodes \cr
\code{nodes_weights} (for \code{frm_em}): 
Optional vector containing initial probabilities for each node \cr
\code{coef_inits}: Optional vector containing initial coefficient for the model \cr
\code{sigma_fixed}: Fixed standard deviations in case of \code{model="linreg"}. 
Heterogeneous standard deviations are allowed. \cr
}
  \item{ind}{
List containing a list of univariate conditional models for covariates.
See \code{dep} for more details on specification.
}
  \item{weights}{
Optional vector of sampling weights
}
  \item{verbose}{
Logical indicating whether convergence progress should be displayed.
}
  \item{maxiter}{
Maximum number of iterations
}
  \item{conv_dev}{
Convergence criterion for deviance
}
  \item{conv_parm}{
Convergence criterion for regression coefficients
}
\item{nodes_control}{Control arguments if nodes are not provided
by the user. The first value denote the number of nodes, while
the second value denotes the spread of the node distribution defined
as the factor of the standard deviation of the observed data.}
\item{h}{Step width for numerical differentiation for calculating the
covariance matrix}
\item{use_grad}{Computation method for gradient in \code{\link{yjt_regression}},
	\code{\link{bct_regression}} or \code{\link{logistic_regression}}.
	It can be \code{0} (compatible with \pkg{mdmb} \eqn{\le}0.3), 
\code{1} or \code{2} (most efficient one).}
\item{data_init}{Initial values for dataset}
\item{iter}{Number of iterations}
\item{burnin}{Number of burnin iterations}
\item{Nimp}{Number of imputed datasets}
\item{Nsave}{(Maximum) Number of values to be saved for MCMC chain}
\item{refresh}{Number of imputations after which proposal distribution 
should be updated in Metropolis-Hastings step}
\item{acc_bounds}{Bounds for acceptance rates for parameters}
\item{print_iter}{Number of imputation after which iteration progress
should be displayed}
\item{object}{Object of corresponding class}
\item{x}{Object of corresponding class}
\item{digits}{Number of digits in \code{summary}}
\item{file}{File to which the \code{summary} should be linked}
\item{idparm}{Indices for parameters to be plotted}
\item{ask}{Logical indicating whether the user is asked before new plot}
\item{\dots}{Further arguments to be passed}
}




\details{
The function allows for fitting a factored regression model. Consider the case
of three variables \eqn{Y}, \eqn{X} and \eqn{Z}. A factored regression model
consists of a sequence of univariate conditional models \eqn{P(Y|X,Z)},
\eqn{P(X|Z)} and \eqn{P(Z)} such that the joint distribution can be factorized as
\deqn{ P(Y,X,Z) = P( Y|X,Z) P(X|Z) P(Z) }
Each of the three variables can contain missing values. Missing values are 
integrated out by posing a distributional assumption for each variable with 
missing values.
}

\value{
For \code{frm_em} it is a list containing the following values

\item{coefs}{Estimated coefficients}
\item{vcov}{Covariance matrix}
\item{partable}{Summary parameter table}
\item{all_coefs}{List with all estimated coefficients}
\item{ll}{Log likelihood value}
\item{like}{Individual likelihood}
\item{dat}{Data frame with included latent values for each variable with missing
 values}
\item{se}{Standard errors for coefficients}
\item{info}{Information matrix}
\item{conv}{Convergence indicator}
\item{iter}{Number of iterations}
\item{ic}{Information criteria}
\item{ind0}{List with model specifications including \code{dep} and \code{ind}}
\item{predictorMatrix}{Predictor matrix}
\item{variablesMatrix}{Matrix containing all variables appearing in statistical 
models}
\item{desc_vars}{Descriptive statistics of variables}
\item{model_results}{Results from fitted models}

The output for \code{frm_fb} contains particular additional values
\item{tech_summary}{Summary table with informations about MCMC algorithm}
\item{values_coda}{Sampled parameter values saved as class \code{mcmc} for
analysis in \pkg{coda} package}
\item{parms_mcmc}{Object containing informations of sampled parameters}
\item{imputations_mcmc}{Object containing informations of imputed datasets}
}


\references{
Allison, P. D. (2012). \emph{Handling missing data by maximum likelihood}.
SAS Global Forum 2012.

Ibrahim, J. G. (1990). Incomplete data in generalized linear models. 
\emph{Journal of the American Statistical Association}, \bold{85}, 765-769.

Ibrahim, J. G., Chen, M. H., Lipsitz, S. R., & Herring, A. H. (2005). 
Missing-data methods for generalized linear models: A comparative review. 
\emph{Journal of the American Statistical Association}, \bold{100}, 332-346.

Jamshidian, M., & Jennrich, R. I. (2000). Standard errors for EM estimation. 
\emph{Journal of the Royal Statistical Society (Series B), 62}, 257-270.

Lee, M. C., & Mitra, R. (2016). Multiply imputing missing values in data sets 
with mixed measurement scales using a sequence of generalised linear models. 
\emph{Computational Statistics & Data Analysis}, \bold{95}, 24-38.
}

\author{
Alexander Robitzsch
}

\note{
The \code{coef} and \code{vcov} can be used to extract coefficients and
the corresponding covariance matrix, respectively. Standard errors for a fitted
object \code{mod} can be extracted by making use of the \pkg{survey}
package and the statement \code{survey::SE(mod)}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \pkg{icdGLM} package for estimation of generalized
linear models with incomplete discrete covariates.

The imputation of substantive models with interactions or nonlinear
terms can be conducted with the \pkg{smcfcs} package or the
\code{jomo::jomo.lm} function in the \pkg{jomo}
package.
}

% \code{\link[jomo:jomo.lm]{jomo::jomo.lm}}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Simulated example linear regression with interaction effects
#############################################################################

# The interaction model stats::lm( Y ~ X + Z + X:Z) is of substantive interest.
# There can be missing values in all variables.

data(data.mb01)
dat <- data.mb01$missing

#******************************************
# Model 1: ML approach

#--- specify models

# define integration nodes
xnodes <- seq(-4,4,len=11)		# nodes for X
ynodes <- seq(-10,10,len=13)           
  # nodes for Y. These ynodes are not really necessary for this dataset because
  # Y has no missing values.

# define model for dependent variable Y
dep <- list("model" = "linreg" , "formula" = Y ~  X*Z , "nodes" = ynodes )

# model P(X|Z)
ind_X <- list( "model" = "linreg" , "formula" = X ~ Z , "nodes" = xnodes )
# all models for covariates
ind <- list( "X" = ind_X )

#--- estimate model with numerical integration
mod1 <- mdmb::frm_em(dat=dat, dep=dep, ind=ind )
summary(mod1)

# extract some informations
coef(mod1)
vcov(mod1)
logLik(mod1)

#******************************************
# Model 2: Fully Bayesian approach / Multiple Imputation

#--- define models
dep <- list("model" = "linreg", "formula" = Y ~  X*Z )
ind_X <- list( "model" = "linreg", "formula" = X ~ Z  )
ind_Z <- list( "model" = "linreg", "formula" = Z ~ 1  )
ind <- list( "X" = ind_X, Z = ind_Z)

#--- estimate model
mod2 <- mdmb::frm_fb(dat, dep, ind , burnin=200, iter=1000)
summary(mod2)
#* plot parameters
plot(mod2)

#--- create list of multiply imputed datasets 
datlist <- mdmb::frm2datlist(mod2)
# convert into object of class mids
imp2 <- miceadds::datlist2mids(datlist)
# estimate linear model on multiply imputed datasets
mod2c <- with(imp2, stats::lm( Y ~ X*Z ) )
summary( mice::pool(mod2c) )

#******************************************
# Model 3: Multiple imputation in jomo package

library(jomo)

# impute with substantive model containing interaction effects
formula <- Y ~ X*Z
imp <- jomo::jomo.lm( formula=formula, data=dat, nburn=100, nbetween=100)

# convert to object of class mids
datlist <- miceadds::jomo2mids( imp )
# estimate linear model
mod3 <- with(datlist, lm( Y ~ X*Z ) )
summary( mice::pool(mod3) )

#############################################################################
# EXAMPLE 2: Simulated example logistic regression with interaction effects
#############################################################################

# Interaction model within a logistic regression Y ~ X + Z + X:Z
# Y and Z are dichotomous variables.

# attach data
data(data.mb02)
dat <- data.mb02$missing

#******************************************
# Model 1: ML approach

#--- specify model

# define nodes
xnodes <- seq(-5,5,len=15)  # X - normally distributed variable
ynodes <- c(0,1)	            # Y and Z dichotomous variable

# model P(Y|X,Z) for dependent variable
dep <- list("model" = "logistic" , "formula" = Y ~  X*Z , "nodes" = ynodes )
# model P(X|Z)
ind_X <- list( "model" = "linreg" , "formula" = X ~ Z , "nodes" = xnodes )
# model P(Z)
ind_Z <- list( "model" = "logistic" , "formula" = Z ~ 1 , "nodes" = ynodes )
ind <- list(  "Z" = ind_Z , "X" = ind_X )

#--- estimate model with numerical integration
mod1 <- mdmb::frm_em(dat=dat, dep=dep, ind=ind )
summary(mod1)

#******************************************
# Model 2: Fully Bayesian approach

#--- specify model
dep <- list("model" = "logistic" , "formula" = Y ~  X*Z  )
ind_X <- list( "model" = "linreg" , "formula" = X ~ Z )
ind_Z <- list( "model" = "logistic" , "formula" = Z ~ 1 )
ind <- list(  "Z" = ind_Z , "X" = ind_X )

#--- Bayesian estimation
mod2 <- mdmb::frm_fb(dat=dat, dep=dep, ind=ind, burnin=500, iter=1000 )
summary(mod2)

#############################################################################
# EXAMPLE 3: Confirmatory factor analysis
#############################################################################

# A latent variable can be considered as missing data and the 'frm_em' function
# is used to estimate the latent variable model.

#--- simulate data
N <- 1000
set.seed(91834)
# latent variable
theta <- stats::rnorm(N)
# simulate items
y1 <- 1.5 + 1*theta + stats::rnorm(N , sd = .7 )
y2 <- 1.9 + .7*theta + stats::rnorm(N , sd = 1 )
y3 <- .9 + .7*theta + stats::rnorm(N , sd = .2 )
dat <- data.frame(y1,y2,y3)
dat$theta <- NA

#******************************************
# Model 1: ML approach

#--- define model
nodes <- seq(-4,4,len=21)
ind_y1 <- list("model" = "linreg" , "formula" = y1 ~  offset(1*theta) , 
                "nodes" = nodes )
ind_y2 <- list( "model" = "linreg", "formula" = y2 ~ theta, "nodes" = nodes, 
                "coef_inits" = c(NA,1) )
ind_y3 <- list( "model" = "linreg", "formula" = y3 ~ theta, "nodes" = nodes, 
                 "coef_inits" = c(1,1) )
dep <- list( "model" = "linreg", "formula" = theta ~ 0 , "nodes" = nodes )
ind <- list( "y1" = ind_y1 ,  "y2" = ind_y2 , "y3" = ind_y3)

#*** estimate model with mdmb::frm_em
mod1 <- mdmb::frm_em(dat, dep, ind)
summary(mod1)

#*** estimate model in lavaan
library(lavaan)
lavmodel <- "
     theta =~ 1*y1 + y2 + y3
     theta ~~ theta
     "
mod1b <- lavaan::cfa( model=lavmodel , data = dat )     
summary(mod1b)

# compare likelihood
logLik(mod1)
logLik(mod1b)

#############################################################################
# EXAMPLE 4: Rasch model
#############################################################################

#--- simulate data
set.seed(91834)
N <- 500
# latent variable
theta0 <- theta <- stats::rnorm(N)
# number of items
I <- 7
dat <- sirt::sim.raschtype( theta , b = seq(-1.5,1.5,len=I) )
colnames(dat) <- paste0("I",1:I)
dat$theta <- NA

#******************************************
# Model 1: ML approach

#--- define model
nodes <- seq(-4,4,len=13)
dep <- list("model" = "linreg", "formula" = theta ~  0 , "nodes" = nodes )
ind <- list()
for (ii in 1:I){
    ind_ii <- list( "model" = "logistic", formula = 
                 stats::as.formula( paste0("I",ii , " ~ offset(1*theta)") ) )
    ind[[ii]] <- ind_ii
}
names(ind) <- colnames(dat)[-(I+1)]

#--- estimate Rasch model with mdmb::frm_em
mod1 <- mdmb::frm_em(dat, dep, ind )
summary(mod1)

#--- estmate Rasch model with sirt package
library(sirt)
mod2 <- sirt::rasch.mml2( dat[,-(I+1)], theta.k=nodes, use.freqpatt=FALSE)
summary(mod2)

#** compare estimated parameters
round(cbind(coef(mod1), c( mod2$sd.trait , -mod2$item$thresh[ seq(I,1)] ) ), 3)

#############################################################################
# EXAMPLE 5: Regression model with measurement error in covariates
#############################################################################

#--- simulate data
set.seed(768)
N <- 1000
# true score
theta <- stats::rnorm(N)
# heterogeneous error variance
var_err <- stats::runif(N, .5 , 1)
# simulate observed score
x <- theta + stats::rnorm(N, sd = sqrt(var_err) )
# simulate outcome
y <- .3 + .7 * theta + stats::rnorm( N , sd = .8 )
dat0 <- dat <- data.frame( y=y, x=x, theta=theta )

#*** estimate model with true scores (which are unobserved in real datasets)
mod0 <- stats::lm( y ~ theta , data = dat0 )
summary(mod0)

#******************************************
# Model 1: Model-based approach

#--- specify model
dat$theta <- NA
nodes <- seq(-4,4,len=15)
dep <- list( "model" = "linreg", "formula" = y ~ theta , "nodes" = nodes , 
                "coef_inits" = c(NA, .4 ) )
ind <- list()
ind[["theta"]] <- list( "model" = "linreg", "formula" = theta ~ 1 , 
                         "nodes" = nodes  )
ind[["x"]] <- list( "model" = "linreg", "formula" = x ~ 0 + offset(theta) , 
                        "nodes" = nodes )
# assumption of heterogeneous known error variance
ind[["x"]]$sigma_fixed <- sqrt( var_err )

#--- estimate regression model
mod1 <- mdmb::frm_em(dat, dep, ind )
summary(mod1)

#******************************************
# Model 2: Fully Bayesian estimation

#--- specify model
dep <- list( "model" = "linreg", "formula" = y ~ theta )
ind <- list()
ind[["theta"]] <- list( "model" = "linreg", "formula" = theta ~ 1  )
ind[["x"]] <- list( "model" = "linreg", "formula" = x ~ 0 + offset(theta) )
# assumption of heterogeneous known error variance
ind[["x"]]$sigma_fixed <- sqrt( var_err )
data_init <- dat
data_init$theta <- dat$x

# estimate model
mod2 <- mdmb::frm_fb(dat, dep, ind, burnin=200, iter = 1000, data_init = data_init)
summary(mod2)
plot(mod2)

#############################################################################
# EXAMPLE 6: Non-normally distributed covariates
#############################################################################

# simulate data with chi-squared distributed covariate from 
# regression model Y ~ X
set.seed(876)
n <- 1500
df <- 2
x <- stats::rchisq( n , df=df )
x <- x / sqrt( 2*df) 
y <- 0 + 1*x
R2 <- .25    # explained variance
y <- y + stats::rnorm(n , sd = sqrt( (1-R2)/R2 * 1) )
dat0 <- dat <- data.frame( y = y , x = x )

# simulate missing responses
prop_miss <- .5
cor_miss <- .7
resp_tend <- cor_miss*(dat$y-mean(y) )/ stats::sd(y) + 
                 stats::rnorm(n , sd = sqrt( 1 - cor_miss^2) )
dat[ resp_tend < stats::qnorm( prop_miss ) , "x" ] <- NA
summary(dat)

#-- complete data
mod0 <- stats::lm( y ~ x, data = dat0 )
summary(mod0)
#-- listwise deletion
mod1 <- stats::lm( y ~ x, data = dat )
summary(mod1)

# normal distribution assumption for frm

# define models
dep <- list("model" = "linreg" , "formula" = y ~  x )
# normally distributed data
ind_x1 <- list( "model" = "linreg" , "formula" = x ~ 1 )
# distribution according to Box-Cox
ind_x2 <- list( "model" = "bctreg" , "formula" = x ~ 1 )
ind1 <- list( "x" = ind_x1 )
ind2 <- list( "x" = ind_x2 )

#--- incorrect normal distribution assumption
mod1 <- mdmb::frm_em(dat=dat, dep=dep, ind=ind1 )
summary(mod1)

#--- model chi-square distribution of predictor with Box-Cox transformation
mod2 <- mdmb::frm_em(dat=dat, dep=dep, ind=ind2 )
summary(mod2)

#############################################################################
# EXAMPLE 7: Latent interaction model
#############################################################################
                                     
# A latent interaction model Y ~ FX + FZ is of interest. Y is directly observed,
# FX and FZ are both indirectly observed by three items 

#--- simulate data
N <- 1000
set.seed(987)
# latent variable
FX <- stats::rnorm(N)
FZ <- stats::rnorm(N)
# simulate items
x1 <- 1.5 + 1*FX + stats::rnorm(N , sd = .7 )
x2 <- 1.9 + .7*FX + stats::rnorm(N , sd = 1 )
x3 <- .9 + .7*FX + stats::rnorm(N , sd = .2 )
z1 <- 1.5 + 1*FZ + stats::rnorm(N , sd = .7 )
z2 <- 1.9 + .7*FZ + stats::rnorm(N , sd = 1 )
z3 <- .9 + .7*FZ + stats::rnorm(N , sd = .2 )
dat <- data.frame(x1,x2,x3,z1,z2,z3)
dat$FX <- NA
dat$FZ <- NA
dat$y <- 2 + .5*FX + .3*FZ + .4*FX*FZ + rnorm( N , sd = 1 )

# estimate interaction model with ML

#--- define model
nodes <- seq(-4,4,len=11)
ind_x1 <- list("model" = "linreg" , "formula" = x1 ~  offset(1*FX) , 
                "nodes" = nodes )
ind_x2 <- list( "model" = "linreg", "formula" = x2 ~ FX, "nodes" = nodes, 
                "coef_inits" = c(NA,1) )
ind_x3 <- list( "model" = "linreg", "formula" = x3 ~ FX, "nodes" = nodes, 
                 "coef_inits" = c(1,1) )
ind_FX <- list( "model" = "linreg", "formula" = FX ~ 0 , "nodes" = nodes )
ind_z1 <- list("model" = "linreg" , "formula" = z1 ~  offset(1*FZ) , 
                "nodes" = nodes )
ind_z2 <- list( "model" = "linreg", "formula" = z2 ~ FZ, "nodes" = nodes, 
                "coef_inits" = c(NA,1) )
ind_z3 <- list( "model" = "linreg", "formula" = z3 ~ FZ, "nodes" = nodes, 
                 "coef_inits" = c(1,1) )
ind_FZ <- list( "model" = "linreg", "formula" = FZ ~ 0 + FX , "nodes" = nodes )
ind <- list( "x1" = ind_x1 ,  "x2" = ind_x2 , "x3" = ind_x3, "FX" = ind_FX ,
            "z1" = ind_z1 ,  "z2" = ind_z2 , "z3" = ind_z3, "FX" = ind_FZ )
dep <- list( "model" = "linreg", formula = y ~ FX+FZ+FX*FZ , "coef_inits" = c(1,.2,.2,0) )

#*** estimate model with mdmb::frm_em
mod1 <- mdmb::frm_em(dat, dep, ind)
summary(mod1)

#############################################################################
# EXAMPLE 8: Non-ignorable data in Y
#############################################################################

# regression Y ~ X in which Y is missing depending Y itself

library(mvtnorm)
cor_XY <- .4        # correlation between X and Y
prop_miss <- .5     # missing proportion
cor_missY <- .7     # correlation with missing propensity
N <- 3000           # sample size

#----- simulate data
set.seed(790)
Sigma <- matrix( c(1, cor_XY , cor_XY , 1) , 2 , 2 )
mu <- c(0,0)
dat <- mvtnorm::rmvnorm( N , mean = mu, sigma=Sigma )
colnames(dat) <- c("X","Y")
dat <- as.data.frame(dat)

#-- generate missing responses on Y depending on Y itself
y1 <- dat$Y
miss_tend <- cor_missY * y1  + rnorm(N , sd = sqrt( 1 - cor_missY^2) )
dat$Y[ miss_tend < quantile( miss_tend , prop_miss ) ] <- NA

#--- ML estimation under assumption of ignorability
nodes <- seq(-5,5,len=15)
dep <- list("model" = "linreg" , "formula" = Y ~  X , "nodes" = nodes )
ind_X <- list( "model" = "linreg" , "formula" = X ~ 1 , "nodes" = nodes )
ind <- list( "X" = ind_X )
mod1 <- mdmb::frm_em(dat=dat, dep=dep, ind=ind)
summary(mod1)

#--- ML estimation under assumption with specifying a model for non-ignorability
#    for response indicator resp_Y
dat$resp_Y <- 1* ( 1 - is.na(dat$Y) )
dep <- list("model" = "linreg" , "formula" = Y ~  X , "nodes" = nodes )
ind_X <- list( "model" = "linreg" , "formula" = X ~ 1 , "nodes" = nodes )
ind_respY <- list( "model" = "logistic" , "formula" = resp_Y ~ Y , "nodes" = nodes )
ind <- list( "X" = ind_X , "resp_Y" = ind_respY )
mod2 <- mdmb::frm_em(dat=dat, dep=dep, ind=ind)
summary(mod2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
