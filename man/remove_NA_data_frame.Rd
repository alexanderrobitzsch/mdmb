\name{remove_NA_data_frame}
\alias{remove_NA_data_frame}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Removes Rows with Some Missing Entries in a Data Frame
}

\description{
Removes rows with some missing entries in a data frame for variables
appearing in the \R formula \code{formula}. This operation is also
known as listwise deletion.
}

\usage{
remove_NA_data_frame(formula, data, weights = NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
An \R formula
}
  \item{data}{
Data frame
}
  \item{weights}{
Optional vector of sample weights
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Data frame with some rows removed according to missing data
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
\code{\link[stats:model.matrix]{stats::model.matrix}}
}


\examples{
#############################################################################
# EXAMPLE 1: Removing rows in a data frame (listwise deletion)
#############################################################################

data(data.ma01, package="miceadds")
dat <- data.ma01

#*** remove rows with some missings according to a formula
dat1 <- mdmb::remove_NA_data_frame( formula = read ~ migrant + books , data=dat)

#*** remove rows according to two formulas
formula1 <- read ~ migrant + books
formula2 <- paredu ~ migrant + female
# create formula consisting of formula1 and formula2
formula3 <- paste( "~" , deparse(formula1[[2]]), "+", deparse(formula1[[3]]), 
             " + " , deparse(formula2[[2]]) , "+" , deparse(formula2[[3]])  )
dat2 <- mdmb::remove_NA_data_frame( formula = as.formula(formula3) , data=dat)

dim(dat)
dim(dat1$data)
dim(dat2$data)
  ##  > dim(dat)
  ##  [1] 4073   11
  ##  > dim(dat1$data)
  ##  [1] 3371   11
  ##  > dim(dat2$data)
  ##  [1] 3090   11
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
