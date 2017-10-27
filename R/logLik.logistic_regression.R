## File Name: logLik.logistic_regression.R
## File Version: 0.06


logLik.logistic_regression <- function (object, ...) {
	return( logLik_extract_ic(object=object) )
}
