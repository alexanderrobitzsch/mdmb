//// File Name: mdmb_rcpp_yjtreg.cpp
//// File Version: 0.15

// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
//using namespace arma;


// user includes


///********************************************************************
///** mdmb_rcpp_yj_trafo
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_yj_trafo( Rcpp::NumericVector y, double lambda )
{
    int N = y.size();
    Rcpp::NumericVector yt(N);
    double lam2 = 2 - lambda;

    for (int nn=0; nn<N; nn++){
        if (y[nn] >= 0 ){
            // positive values
            // ( ( y + 1 )^lambda -1 ) / lambda
            // yt[nn] = ( std::pow( y[nn] + 1, lambda) -1 ) / lambda;
            yt[nn] = ( std::exp( std::log( y[nn] + 1 ) * lambda ) -1 ) / lambda;
        } else {
            // negative values
            // - ( (  - yt + 1 )^(2-lambda) -1 ) / (2-lambda)
            // yt[nn] = - ( std::pow( - y[nn] + 1, 2-lambda) -1 ) / (2-lambda);
            yt[nn] = - ( std::exp( std::log( - y[nn] + 1 )*lam2 ) - 1 ) / lam2;
        }
    }
    //--- OUTPUT
    return yt;
}
///********************************************************************

///********************************************************************
///** mdmb_rcpp_yj_trafo_adjustment_derivative
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_yj_trafo_adjustment_derivative( Rcpp::NumericVector y, double lambda )
{
    int N = y.size();
    Rcpp::NumericVector yt(N);
    double lam2 = 1 - lambda;
    double lam1 = - lam2;

    for (int nn=0; nn<N; nn++){
        if (y[nn] >= 0 ){
            // #*** y >=0
            // yt <- ifelse( y >=0,  ( yt + 1 )^(lambda-1), yt )
            yt[nn] = std::exp( std::log( y[nn] + 1 )*lam1 );
        } else {
            // #*** y <=0
            // yt <- ifelse( y < 0, ( - yt + 1 )^(1-lambda), yt )
            yt[nn] = std::exp( std::log( - y[nn] + 1 )*lam2 );
        }
    }
    //--- OUTPUT
    return yt;
}
///********************************************************************


// Rcpp::Rcout << "cc=" << cc << " ii_start= " << ii_start <<
//                " ii_end=" << ii_end << " count=" << count << std::endl;
