## File Name: mdmb_sample_probabilities.R
## File Version: 0.04

mdmb_sample_probabilities <- function(matr)
{
    matr1 <- sirt::rowCumsums.sirt(matr=matr)
    N <- nrow(matr)
    rn <- stats::runif(N)
    imp1 <- sirt::rowIntervalIndex.sirt(matr=matr1,rn=rn)
    return(imp1)
}
