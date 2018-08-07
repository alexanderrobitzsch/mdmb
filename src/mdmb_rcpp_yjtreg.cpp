//// File Name: mdmb_rcpp_yjtreg.cpp
//// File Version: 0.417

// [[Rcpp::depends(RcppArmadillo)]]
// [[RcppNOplugins(unwindProtect)]]

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



///********************************************************************
///** mdmb_rcpp_yj_trafo_derivative
// [[Rcpp::export]]
Rcpp::List mdmb_rcpp_yj_trafo_derivative( Rcpp::NumericVector y, double lambda, bool probit  )
{
    int N = y.size();
    Rcpp::NumericVector yt(N);
    Rcpp::NumericVector dyt(N);
    double lam2 = 2 - lambda;
    double temp = 0;
    double temp2 = 0;
    double y1 = 0;
    dyt.fill(1);

    for (int nn=0; nn<N; nn++){
        yt[nn] = y[nn];
        // probit transformation
        // Y = Phi( X )
        if (probit){
            yt[nn] = ::Rf_qnorm5( yt[nn], 0.0, 1.0, TRUE, FALSE);
            dyt[nn] *= 1 / ::Rf_dnorm4(yt[nn], 0, 1, FALSE);
        }

        if (yt[nn] >= 0 ){
            y1 = yt[nn] + 1;
            // positive values
            // ( ( y + 1 )^lambda -1 ) / lambda
            // yt[nn] = ( std::pow( y[nn] + 1, lambda) -1 ) / lambda;
            temp = std::log( y1 );
            temp2 = std::exp( temp * lambda );
            yt[nn] = ( temp2 - 1 ) / lambda;

            // yt <- ifelse( y >=0,  ( yt + 1 )^(lambda-1), yt )
            dyt[nn] *= temp2 / ( y1 );
        } else {
            y1 = - yt[nn] + 1;
            // negative values
            // - ( (  - yt + 1 )^(2-lambda) -1 ) / (2-lambda)
            // yt[nn] = - ( std::pow( - y[nn] + 1, 2-lambda) -1 ) / (2-lambda);
            temp = std::log( y1 );
            temp2 = std::exp( temp*lam2 );
            yt[nn] = - ( temp2 - 1 ) / lam2;

            // yt <- ifelse( y < 0, ( - yt + 1 )^(1-lambda), yt )
            // dyt[nn] = std::exp( temp*lam3 );
            dyt[nn] *= temp2 / y1;
        }

    }

    /////////////////////////////////////////////
    // OUTPUT:
    return Rcpp::List::create(
                Rcpp::Named("yt") = yt,
                Rcpp::Named("dyt") = dyt
            );
}
///********************************************************************

// Rcpp::Rcout << "cc=" << cc << " ii_start= " << ii_start <<
//                " ii_end=" << ii_end << " count=" << count << std::endl;
