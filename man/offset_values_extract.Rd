\name{offset_values_extract}
\alias{offset_values_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts Offset Values
}
\description{
Extracts offset values by applying a formula with an offset term
to a data frame.
}
\usage{
offset_values_extract(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
An \R Formula
}
  \item{data}{
Data frame
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Vector containing offset values
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
\code{\link[stats:offset]{stats::offset}},
\code{\link[stats:model.offset]{stats::model.offset}}
}


\examples{
#############################################################################
# EXAMPLE 1: Toy example for extraction of offset values
#############################################################################

data(data.ma01, package="miceadds")
dat <- data.ma01

dat1 <- mdmb::offset_values_extract( formula = ~ migrant + offset(books) , data = dat )
dat1[1:5]
  ##  [1] 6 6 5 2 6
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
