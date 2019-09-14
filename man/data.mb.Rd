%% File Name: data.mb.Rd
%% File Version: 0.24

\name{data.mb}
\alias{data.mb}

\alias{data.mb01}
\alias{data.mb02}
\alias{data.mb03}
\alias{data.mb04}
\alias{data.mb05}

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
data(data.mb03)
data(data.mb04)
data(data.mb05)
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


\item Dataset \code{data.mb03}. This dataset is from Enders, Baraldi & Cham (2014)
and contains three
variables primary school reading (\code{x}), primary school learning problems (\code{z})
and middle school reading (\code{y}) which all have missing values.

\code{'data.frame':   74 obs. of  3 variables:} \cr
\code{ $ x: num  NA NA NA NA NA 8.34 NA 8.36 6.89 8.56 ...} \cr
\code{ $ z: num  8.81 4.5 6.31 4.7 5.1 4 6.11 3.7 6.81 6.31 ...} \cr
\code{ $ y: num  5 5.1 6.3 9 9 9.3 NA 10.7 6.2 NA ...} \cr

\item Dataset \code{data.mb04}. This multilevel dataset contains three variables:
level-1 variables code{y}, \code{x} and the level-2 variable \code{w}.

\code{'data.frame':   500 obs. of  4 variables:} \cr
\code{ $ idcluster: int  1 1 1 1 1 2 2 2 2 2 ...} \cr
\code{ $ x        : num  NA NA -1.15 -1.65 0.25 ...} \cr
\code{ $ w        : num  -0.552 -0.552 -0.552 -0.552 -0.552 ...} \cr
\code{ $ y        : num  NA NA -0.0711 0.7165 -0.1917 ...} \cr

\item Dataset \code{data.mb05}. This dataset contains selected (and transformed)
variables of the German PISA 2012 data.

\code{'data.frame':   5001 obs. of  13 variables:} \cr
\code{ $ idschool : num  1001 1001 1001 1001 1001 ...} \cr
\code{ $ idstud   : num  1e+05 1e+05 1e+05 1e+05 1e+05 ...} \cr
\code{ $ female   : num  1 1 0 0 0 1 1 1 0 0 ...} \cr
\code{ $ books    : num  NA 3 3 1 NA 2 NA 1 NA 2 ...} \cr
\code{ $ hisced   : num  NA 6 6 2 NA 2 NA 2 NA 2 ...} \cr
\code{ $ hisei    : num  NA 30.6 57.7 26.9 NA ...} \cr
\code{ $ hisei10  : num  NA 0.257 0.596 0.211 NA ...} \cr
\code{ $ native   : num  NA NA 1 0 NA 0 NA 1 NA 1 ...} \cr
\code{ $ ANCINTMAT: num  NA 0.644 -0.096 1.057 NA ...} \cr
\code{ $ MATHEFF  : num  NA 0.34 0.54 -0.18 NA 0.15 NA NA NA NA ...} \cr
\code{ $ READ     : num  -0.25 -0.503 0.421 -1.664 -0.894 ...} \cr
\code{ $ MATH     : num  -0.565 -0.854 0.384 -0.896 -0.534 ...} \cr
\code{ $ W_FSTUWT : num  140 140 140 140 140 ...} \cr

}
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
Enders, C. K., Baraldi, A. N., & Cham, H. (2014). Estimating interaction effects with
incomplete predictor variables. \emph{Psychological Methods, 19}(1), 39-55.
\doi{10.1037/a0035314}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Linear interaction example from Enders et al. (2014)
#############################################################################

# load packages
library(mdmb)
library(mice)
library(mitools)
library(sandwich)

#--- attach example dataset (Enders et al., 2014) from mdmb package
data( data.mb03, package="mdmb")
dat <- data.mb03

#--- center data which speeds convergence of Bayesian estimation
#--- of the imputation model
for (vv in 1:3){
    M_vv <- mean( dat[,vv], na.rm=TRUE )
    dat[,vv] <- dat[,vv] - M_vv
}

#--- generate initial imputed values withj mice package
imp <- mice::mice( dat, m=, maxit=20 )
data_init <- mice::complete(imp, action=1)

#--- define number of iterations and number of imputed datasets
iter <- 50000; burnin <- 5000
Nimp <- 100

#******* imputation model M3 with quadratic effects

# model for dependent variable
dep <- list("model"="linreg", "formula"=y ~ x*z + I(x^2) + I(z^2) )
# covariate models
ind_x <- list( "model"="linreg", "formula"=x ~ z + I(z^2) )
ind_z <- list( "model"="linreg", "formula"=z ~ 1 )
ind <- list( x=ind_x, z=ind_z)

#generate imputations
imp <- mdmb::frm_fb(dat=dat, dep=dep, ind=ind, burnin=burnin, iter=iter,
              data_init=data_init, Nimp=Nimp)

#--- create list of multiply imputed datasets
datlist <- mdmb::frm2datlist(imp)

#-------------------------------
#--- analyze imputed datasets with mice package

# convert into object of class mids
imp2 <- miceadds::datlist2mids(datlist)
# estimate linear model on multiply imputed datasets
mod1 <- with(imp2, stats::lm( y ~ x*z ) )
summary( mice::pool(mod1) )

#-------------------------------
#--- analyze imputed datasets using sandwich standard errors

results <- list()
variances <- list()
Nimp <- length(datlist)
for (ii in 1:Nimp){
    mod_ii <- stats::lm( y ~ x*z, data=datlist[[ii]] )
    variances[[ii]] <- sandwich::vcovHC(mod_ii)
    results[[ii]] <- coef(mod_ii)
}

mod2 <- mitools::MIcombine(results=results,variances=variances,df.complete=69)
summary(mod2)
}
}
%% \keyword{datasets}
