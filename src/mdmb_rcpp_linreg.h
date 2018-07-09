//// File Name: mdmb_rcpp_linreg.h
//// File Version: 0.491

#ifndef _MDMB_MDMB_RCPP_LINREG_H
#define _MDMB_MDMB_RCPP_LINREG_H
 
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

double mdmb_rcpp_weighted_sd_centered( Rcpp::NumericVector x, Rcpp::NumericVector w );

double mdmb_rcpp_weighted_sd( Rcpp::NumericVector x, Rcpp::NumericVector w );

Rcpp::List mdmb_rcpp_lm_wfit( arma::mat x, arma::colvec y, arma::colvec w );

Rcpp::NumericVector mdmb_rcpp_frm_normalize_posterior(
            Rcpp::NumericVector post, Rcpp::IntegerVector case_ );

Rcpp::NumericVector mdmb_rcpp_dnorm( Rcpp::NumericVector x, Rcpp::NumericVector mu,
    double sigma );

Rcpp::NumericVector mdmb_rcpp_log_dnorm( Rcpp::NumericVector x, Rcpp::NumericVector mu,
    double sigma );

#endif // _MDMB_MDMB_RCPP_LINREG_H
