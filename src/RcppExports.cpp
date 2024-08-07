//// File Name: RcppExports.cpp
//// File Version: 1.010001
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>

using namespace Rcpp; using namespace arma;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

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
// mdmb_rcpp_log_dnorm
Rcpp::NumericVector mdmb_rcpp_log_dnorm(Rcpp::NumericVector x, Rcpp::NumericVector mu, double sigma);
RcppExport SEXP _mdmb_mdmb_rcpp_log_dnorm(SEXP xSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_log_dnorm(x, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_dnorm_double
Rcpp::NumericVector mdmb_rcpp_dnorm_double(Rcpp::NumericVector x, double mu, double sigma);
RcppExport SEXP _mdmb_mdmb_rcpp_dnorm_double(SEXP xSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_dnorm_double(x, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_log_dnorm_double
Rcpp::NumericVector mdmb_rcpp_log_dnorm_double(Rcpp::NumericVector x, double mu, double sigma);
RcppExport SEXP _mdmb_mdmb_rcpp_log_dnorm_double(SEXP xSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_log_dnorm_double(x, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_dt
Rcpp::NumericVector mdmb_rcpp_dt(Rcpp::NumericVector x, double df, bool use_log);
RcppExport SEXP _mdmb_mdmb_rcpp_dt(SEXP xSEXP, SEXP dfSEXP, SEXP use_logSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type df(dfSEXP);
    Rcpp::traits::input_parameter< bool >::type use_log(use_logSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_dt(x, df, use_log));
    return rcpp_result_gen;
END_RCPP
}
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
// mdmb_rcpp_oprobit_density
Rcpp::NumericVector mdmb_rcpp_oprobit_density(Rcpp::NumericVector ypred, Rcpp::NumericVector thresh_low, Rcpp::NumericVector thresh_upp, Rcpp::NumericVector y, bool use_log, double eps);
RcppExport SEXP _mdmb_mdmb_rcpp_oprobit_density(SEXP ypredSEXP, SEXP thresh_lowSEXP, SEXP thresh_uppSEXP, SEXP ySEXP, SEXP use_logSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ypred(ypredSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_low(thresh_lowSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_upp(thresh_uppSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< bool >::type use_log(use_logSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_oprobit_density(ypred, thresh_low, thresh_upp, y, use_log, eps));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_oprobit_derivative_ypred
Rcpp::List mdmb_rcpp_oprobit_derivative_ypred(Rcpp::NumericVector ypred, Rcpp::NumericVector thresh_low, Rcpp::NumericVector thresh_upp, Rcpp::NumericVector y);
RcppExport SEXP _mdmb_mdmb_rcpp_oprobit_derivative_ypred(SEXP ypredSEXP, SEXP thresh_lowSEXP, SEXP thresh_uppSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ypred(ypredSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_low(thresh_lowSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_upp(thresh_uppSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_oprobit_derivative_ypred(ypred, thresh_low, thresh_upp, y));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_oprobit_derivative_logthresh
Rcpp::NumericMatrix mdmb_rcpp_oprobit_derivative_logthresh(Rcpp::NumericVector ypred, Rcpp::NumericVector thresh_low, Rcpp::NumericVector thresh_upp, Rcpp::NumericVector y, Rcpp::NumericMatrix ll0, double eps, double h, int y_value);
RcppExport SEXP _mdmb_mdmb_rcpp_oprobit_derivative_logthresh(SEXP ypredSEXP, SEXP thresh_lowSEXP, SEXP thresh_uppSEXP, SEXP ySEXP, SEXP ll0SEXP, SEXP epsSEXP, SEXP hSEXP, SEXP y_valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ypred(ypredSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_low(thresh_lowSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_upp(thresh_uppSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type ll0(ll0SEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< double >::type h(hSEXP);
    Rcpp::traits::input_parameter< int >::type y_value(y_valueSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_oprobit_derivative_logthresh(ypred, thresh_low, thresh_upp, y, ll0, eps, h, y_value));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_oprobit_derivative_logthresh_chain_rule
Rcpp::NumericMatrix mdmb_rcpp_oprobit_derivative_logthresh_chain_rule(Rcpp::NumericVector thresh_der, Rcpp::NumericVector probs, Rcpp::NumericVector dens_upp, Rcpp::NumericVector dens_low, Rcpp::IntegerVector y, double eps, int y_value);
RcppExport SEXP _mdmb_mdmb_rcpp_oprobit_derivative_logthresh_chain_rule(SEXP thresh_derSEXP, SEXP probsSEXP, SEXP dens_uppSEXP, SEXP dens_lowSEXP, SEXP ySEXP, SEXP epsSEXP, SEXP y_valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type thresh_der(thresh_derSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dens_upp(dens_uppSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dens_low(dens_lowSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< int >::type y_value(y_valueSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_oprobit_derivative_logthresh_chain_rule(thresh_der, probs, dens_upp, dens_low, y, eps, y_value));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_yj_trafo
Rcpp::NumericVector mdmb_rcpp_yj_trafo(Rcpp::NumericVector y, double lambda);
RcppExport SEXP _mdmb_mdmb_rcpp_yj_trafo(SEXP ySEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_yj_trafo(y, lambda));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_yj_trafo_adjustment_derivative
Rcpp::NumericVector mdmb_rcpp_yj_trafo_adjustment_derivative(Rcpp::NumericVector y, double lambda);
RcppExport SEXP _mdmb_mdmb_rcpp_yj_trafo_adjustment_derivative(SEXP ySEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_yj_trafo_adjustment_derivative(y, lambda));
    return rcpp_result_gen;
END_RCPP
}
// mdmb_rcpp_yj_trafo_derivative
Rcpp::List mdmb_rcpp_yj_trafo_derivative(Rcpp::NumericVector y, double lambda, bool probit);
RcppExport SEXP _mdmb_mdmb_rcpp_yj_trafo_derivative(SEXP ySEXP, SEXP lambdaSEXP, SEXP probitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< bool >::type probit(probitSEXP);
    rcpp_result_gen = Rcpp::wrap(mdmb_rcpp_yj_trafo_derivative(y, lambda, probit));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mdmb_mdmb_rcpp_dnorm", (DL_FUNC) &_mdmb_mdmb_rcpp_dnorm, 3},
    {"_mdmb_mdmb_rcpp_log_dnorm", (DL_FUNC) &_mdmb_mdmb_rcpp_log_dnorm, 3},
    {"_mdmb_mdmb_rcpp_dnorm_double", (DL_FUNC) &_mdmb_mdmb_rcpp_dnorm_double, 3},
    {"_mdmb_mdmb_rcpp_log_dnorm_double", (DL_FUNC) &_mdmb_mdmb_rcpp_log_dnorm_double, 3},
    {"_mdmb_mdmb_rcpp_dt", (DL_FUNC) &_mdmb_mdmb_rcpp_dt, 3},
    {"_mdmb_mdmb_rcpp_weighted_sd_centered", (DL_FUNC) &_mdmb_mdmb_rcpp_weighted_sd_centered, 2},
    {"_mdmb_mdmb_rcpp_weighted_sd", (DL_FUNC) &_mdmb_mdmb_rcpp_weighted_sd, 2},
    {"_mdmb_mdmb_rcpp_lm_wfit", (DL_FUNC) &_mdmb_mdmb_rcpp_lm_wfit, 3},
    {"_mdmb_mdmb_rcpp_frm_normalize_posterior", (DL_FUNC) &_mdmb_mdmb_rcpp_frm_normalize_posterior, 2},
    {"_mdmb_mdmb_rcpp_oprobit_density", (DL_FUNC) &_mdmb_mdmb_rcpp_oprobit_density, 6},
    {"_mdmb_mdmb_rcpp_oprobit_derivative_ypred", (DL_FUNC) &_mdmb_mdmb_rcpp_oprobit_derivative_ypred, 4},
    {"_mdmb_mdmb_rcpp_oprobit_derivative_logthresh", (DL_FUNC) &_mdmb_mdmb_rcpp_oprobit_derivative_logthresh, 8},
    {"_mdmb_mdmb_rcpp_oprobit_derivative_logthresh_chain_rule", (DL_FUNC) &_mdmb_mdmb_rcpp_oprobit_derivative_logthresh_chain_rule, 7},
    {"_mdmb_mdmb_rcpp_yj_trafo", (DL_FUNC) &_mdmb_mdmb_rcpp_yj_trafo, 2},
    {"_mdmb_mdmb_rcpp_yj_trafo_adjustment_derivative", (DL_FUNC) &_mdmb_mdmb_rcpp_yj_trafo_adjustment_derivative, 2},
    {"_mdmb_mdmb_rcpp_yj_trafo_derivative", (DL_FUNC) &_mdmb_mdmb_rcpp_yj_trafo_derivative, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_mdmb(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
