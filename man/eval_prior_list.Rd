%% File Name: eval_prior_list.Rd
%% File Version: 0.09
%% File Last Change: 2017-08-20 14:23:42

\name{eval_prior_list}
\alias{eval_prior_list}
\alias{eval_prior_list_sumlog}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Evaluates Several Prior Distributions
}

\description{
The function \code{eval_prior_list} evaluates several prior distributions 
specified in a list. The function \code{eval_prior_list_sumlog}
computes the sum of the logarithms of all prior values.
}

\usage{
eval_prior_list(par, par_prior, log = FALSE, eps = 1e-50)

eval_prior_list_sumlog(par, par_prior, use_grad = 1)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
Parameter vector
}
  \item{par_prior}{
List of prior distributions specified in a list (see Examples)
}
\item{log}{Logical indicating whether the logarithm of the prior should be
computed}
\item{eps}{Summand to be added to the prior to avoid computation of the logarithm
for values of zero}
\item{use_grad}{Integer value for computation value for gradient}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Vector or a numeric value 
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[sirt:prior_model_parse]{sirt::prior_model_parse}}
}

\examples{
#############################################################################
# EXAMPLE 1: Evaluation of prior values
#############################################################################

# normal distribution
b0 <- list( "dnorm" , list(mean=0,sd=100) )
# t distribution with one degree of freedom (Cauchy distribution)
b1 <- list( "dt" , list(df=1) )
# define list of priors
beta_prior <- list( b0 , b1 )
# parameter value
beta <- c( 0.3 , 1 )

#-- evaluate priors
mdmb::eval_prior_list(par=beta, par_prior=beta_prior)
mdmb::eval_prior_list_sumlog(par=beta, par_prior=beta_prior)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
