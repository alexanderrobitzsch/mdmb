## File Name: logLik.logistic_regression.R
## File Version: 0.06
## File Last Change: 2017-01-23 19:35:13


logLik.logistic_regression <- function (object, ...) {
	return( logLik_extract_ic(object=object) )
}
