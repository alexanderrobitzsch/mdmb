## File Name: logLik.bct_regression.R
## File Version: 0.02


logLik.bct_regression <- function (object, ...) {
	return( logLik_extract_ic(object=object) )
}
