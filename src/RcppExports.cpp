//// File Name: RcppExports.cpp
//// File Version: 0.008009
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>

using namespace Rcpp; using namespace arma;

// mdmb_rcpp_weighted_sd_centered
double mdmb_rcpp_weighted_sd_centered(Rcpp::NumericVector x, Rcpp::NumericVector w);
RcppExport SEXP _mdmb_mdmb_rcpp_weighted_sd_centered(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_weighted_sd_centered(x, w));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_weighted_sd
double mdmb_rcpp_weighted_sd(Rcpp::NumericVector x, Rcpp::NumericVector w);
RcppExport SEXP _mdmb_mdmb_rcpp_weighted_sd(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_weighted_sd(x, w));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_lm_wfit
Rcpp::List mdmb_rcpp_lm_wfit(arma::mat x, arma::colvec y, arma::colvec w);
RcppExport SEXP _mdmb_mdmb_rcpp_lm_wfit(SEXP xSEXP, SEXP ySEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_lm_wfit(x, y, w));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_frm_normalize_posterior
Rcpp::NumericVector mdmb_rcpp_frm_normalize_posterior(Rcpp::NumericVector post, Rcpp::IntegerVector case_);
RcppExport SEXP _mdmb_mdmb_rcpp_frm_normalize_posterior(SEXP postSEXP, SEXP case_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type post(postSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type case_(case_SEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_frm_normalize_posterior(post, case_));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_dnorm
Rcpp::NumericVector mdmb_rcpp_dnorm(Rcpp::NumericVector x, Rcpp::NumericVector mu, double sigma);
RcppExport SEXP _mdmb_mdmb_rcpp_dnorm(SEXP xSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_dnorm(x, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_test_rtnorm2
double mdmb_test_rtnorm2(double mu, double sigma, double lower, double upper);
RcppExport SEXP _mdmb_mdmb_test_rtnorm2(SEXP muSEXP, SEXP sigmaSEXP, SEXP lowerSEXP, SEXP upperSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< double >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_test_rtnorm2(mu, sigma, lower, upper));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mdmb_mdmb_rcpp_weighted_sd_centered", (DL_FUNC) &_mdmb_mdmb_rcpp_weighted_sd_centered, 2},
    {"_mdmb_mdmb_rcpp_weighted_sd", (DL_FUNC) &_mdmb_mdmb_rcpp_weighted_sd, 2},
    {"_mdmb_mdmb_rcpp_lm_wfit", (DL_FUNC) &_mdmb_mdmb_rcpp_lm_wfit, 3},
    {"_mdmb_mdmb_rcpp_frm_normalize_posterior", (DL_FUNC) &_mdmb_mdmb_rcpp_frm_normalize_posterior, 2},
    {"_mdmb_mdmb_rcpp_dnorm", (DL_FUNC) &_mdmb_mdmb_rcpp_dnorm, 3},
    {"_mdmb_mdmb_test_rtnorm2", (DL_FUNC) &_mdmb_mdmb_test_rtnorm2, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_mdmb(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
