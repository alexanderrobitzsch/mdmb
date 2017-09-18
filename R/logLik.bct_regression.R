## File Name: logLik.bct_regression.R
## File Version: 0.02
## File Last Change: 2017-01-23 19:35:13


logLik.bct_regression <- function (object, ...) {
	return( logLik_extract_ic(object=object) )
}
