//// File Name: mdmb_rcpp_dnorm_dt.cpp
//// File Version: 0.14

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;



// user includes
const double pi1 = 3.14159265359;
const double pi2 = 1 / std::sqrt(2*pi1);  // 1/sqrt(2*pi)



///********************************************************************
// univariate normal density
///** mdmb_rcpp_dnorm
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_dnorm( Rcpp::NumericVector x, Rcpp::NumericVector mu,
    double sigma )
{
    int N = x.size();
    Rcpp::NumericVector fx(N);
    double fac = pi2;  // 1/sqrt(2*pi)
    fac = fac / sigma;
    double tmp = 0;
    double sq2 = 1 / std::sqrt(2) / sigma;
    for (int nn=0; nn<N; nn++){
        tmp = sq2 * (x[nn] - mu[nn]);
        fx[nn] = fac * std::exp( - tmp * tmp);
    }
    //---- OUTPUT
    return fx;
}
///********************************************************************

///********************************************************************
// univariate normal density logarithmized
///** mdmb_rcpp_log_dnorm
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_log_dnorm( Rcpp::NumericVector x, Rcpp::NumericVector mu,
    double sigma )
{
    int N = x.size();
    Rcpp::NumericVector fx(N);
    double fac = pi2;  // 1/sqrt(2*pi)
    fac = std::log( fac / sigma);
    double tmp = 0;
    double sq2 = 1 / std::sqrt(2) / sigma;
    for (int nn=0; nn<N; nn++){
        tmp = sq2 * (x[nn] - mu[nn]);
        fx[nn] = fac - tmp * tmp;
    }
    //---- OUTPUT
    return fx;
}
///********************************************************************

///********************************************************************
// univariate normal density
///** mdmb_rcpp_dnorm_double
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_dnorm_double( Rcpp::NumericVector x, double mu,
    double sigma )
{
    int N = x.size();
    Rcpp::NumericVector fx(N);
    double fac = pi2;  // 1/sqrt(2*pi)
    fac = fac / sigma;
    double tmp = 0;
    double sq2 = 1 / std::sqrt(2) / sigma;
    for (int nn=0; nn<N; nn++){
        tmp = sq2 * (x[nn] - mu);
        fx[nn] = fac * std::exp( - tmp * tmp);
    }
    //---- OUTPUT
    return fx;
}
///********************************************************************

///********************************************************************
// univariate normal density logarithmized
///** mdmb_rcpp_log_dnorm_double
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_log_dnorm_double( Rcpp::NumericVector x, double mu,
    double sigma )
{
    int N = x.size();
    Rcpp::NumericVector fx(N);
    double fac = pi2;  // 1/sqrt(2*pi)
    fac = std::log( fac / sigma);
    double tmp = 0;
    double sq2 = 1 / std::sqrt(2) / sigma;
    for (int nn=0; nn<N; nn++){
        tmp = sq2 * (x[nn] - mu);
        fx[nn] = fac - tmp * tmp;
    }
    //---- OUTPUT
    return fx;
}
///********************************************************************

///********************************************************************
///** mdmb_rcpp_dt
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_dt( Rcpp::NumericVector x, double df, bool use_log )
{
    int N = x.size();
    Rcpp::NumericVector fx(N);
    double df1 = (df+1)/2;
    double g1 = ::Rf_lgammafn(df1);
    double g2 = ::Rf_lgammafn(df/2.0);
    double g3 = std::log( std::sqrt(df*pi1) );
    double fac = g1 - g2 - g3;
    for (int nn=0; nn<N; nn++){
        fx[nn] = fac - df1 * std::log( 1 + x[nn]*x[nn] / df );
        if ( ! use_log ){
            fx[nn] = std::exp(fx[nn]);
        }
    }
    //---- OUTPUT
    return fx;
}
///********************************************************************
