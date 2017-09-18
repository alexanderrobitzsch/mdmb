## File Name: logLik.yjt_regression.R
## File Version: 0.03
## File Last Change: 2017-01-23 19:35:13


logLik.yjt_regression <- function (object, ...) {
	return( logLik_extract_ic(object=object) )
}
