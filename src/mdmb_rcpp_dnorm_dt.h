//// File Name: mdmb_rcpp_dnorm_dt.h
//// File Version: 0.14

#ifndef _MDMB_MDMB_RCPP_DNORM_DT_H
#define _MDMB_MDMB_RCPP_DNORM_DT_H
 
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

Rcpp::NumericVector mdmb_rcpp_dnorm( Rcpp::NumericVector x, Rcpp::NumericVector mu,
    double sigma );

Rcpp::NumericVector mdmb_rcpp_log_dnorm( Rcpp::NumericVector x, Rcpp::NumericVector mu,
    double sigma );

Rcpp::NumericVector mdmb_rcpp_dnorm_double( Rcpp::NumericVector x, double mu,
    double sigma );

Rcpp::NumericVector mdmb_rcpp_log_dnorm_double( Rcpp::NumericVector x, double mu,
    double sigma );

Rcpp::NumericVector mdmb_rcpp_dt( Rcpp::NumericVector x, double df, bool use_log );

#endif // _MDMB_MDMB_RCPP_DNORM_DT_H
