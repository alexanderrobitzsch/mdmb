//// File Name: mdmb_rcpp_oprobit.cpp
//// File Version: 0.498


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(miceadds)]]

#include <RcppArmadillo.h>
//#include <Rcpp.h>


#include <miceadds.h>
// #include "p:\Eigene_Projekte\R-Routinen\IRT-Functions\miceadds_Package\2.14\miceadds_work\inst\include\miceadds.h"

// [include_header_file]
#include "mdmb_rcpp_linreg.h"

using namespace Rcpp;
using namespace arma;
using namespace miceadds;



///********************************************************************
///** mdmb_rcpp_oprobit_density
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_oprobit_density( Rcpp::NumericVector ypred,
    Rcpp::NumericVector thresh_low, Rcpp::NumericVector thresh_upp,
    Rcpp::NumericVector y, bool use_log, double eps)
{
    // stats::pnorm( thresh_upp[y+1] - ypred ) - stats::pnorm( thresh_low[y+1] - ypred )
    double sigma0 = 1.0;
    int N = y.size();
    Rcpp::NumericVector thresh_upp1(N);
    Rcpp::NumericVector thresh_low1(N);
    for (int nn=0; nn<N; nn++){
        thresh_upp1[nn] = thresh_upp[ y[nn] ];
        thresh_low1[nn] = thresh_low[ y[nn] ];
    }
    Rcpp::NumericVector y2 = miceadds::miceadds_rcpp_pnorm( thresh_upp1, ypred, sigma0 );
    Rcpp::NumericVector y1 = miceadds::miceadds_rcpp_pnorm( thresh_low1, ypred, sigma0 );
    Rcpp::NumericVector z = y2 - y1;
    if (use_log){
        for (int nn=0; nn<N; nn++){
            z[nn] = std::log( z[nn] + eps);
        }
    }
    return z;
}
///********************************************************************

///********************************************************************
///** mdmb_rcpp_oprobit_derivative_ypred
// [[Rcpp::export]]
Rcpp::List mdmb_rcpp_oprobit_derivative_ypred( Rcpp::NumericVector ypred,
    Rcpp::NumericVector thresh_low, Rcpp::NumericVector thresh_upp,
    Rcpp::NumericVector y)
{
    // stats::pnorm( thresh_upp[y+1] - ypred ) - stats::pnorm( thresh_low[y+1] - ypred )
    double sigma0 = 1.0;
    int N = y.size();
    Rcpp::NumericVector thresh_upp1(N);
    Rcpp::NumericVector thresh_low1(N);
    Rcpp::NumericMatrix der1(N,1);
    Rcpp::NumericMatrix ll0(N,1);
    for (int nn=0; nn<N; nn++){
        thresh_upp1[nn] = thresh_upp[ y[nn] ];
        thresh_low1[nn] = thresh_low[ y[nn] ];
    }

    // distribution function
    Rcpp::NumericVector y2 = miceadds::miceadds_rcpp_pnorm( thresh_upp1, ypred, sigma0 );
    Rcpp::NumericVector y1 = miceadds::miceadds_rcpp_pnorm( thresh_low1, ypred, sigma0 );
    Rcpp::NumericVector z = y2 - y1;

    // density
    double eps = 1e-50;
    Rcpp::NumericVector d2 = mdmb_rcpp_dnorm( thresh_upp1, ypred, sigma0 );
    Rcpp::NumericVector d1 = mdmb_rcpp_dnorm( thresh_low1, ypred, sigma0 );
    for (int nn=0; nn<N; nn++){
        der1(nn,0) = ( - d1[nn] + d2[nn] )/ ( z[nn] + eps);
        ll0(nn,0) = std::log( z[nn] + eps );
    }
    //---- OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("ll0") = ll0,
                Rcpp::Named("der1") = der1,
                Rcpp::Named("probs") = z,
                Rcpp::Named("dens_upp") = d2,
                Rcpp::Named("dens_low") = d1
        );
}
///********************************************************************

///********************************************************************
///** mdmb_rcpp_oprobit_derivative_logthresh
// [[Rcpp::export]]
Rcpp::NumericMatrix mdmb_rcpp_oprobit_derivative_logthresh( Rcpp::NumericVector ypred,
    Rcpp::NumericVector thresh_low, Rcpp::NumericVector thresh_upp,
    Rcpp::NumericVector y, Rcpp::NumericMatrix ll0, double eps, double h,
    int y_value )
{
    int N = y.size();
    double thresh_upp1=0.0;
    double thresh_low1=0.0;
    Rcpp::NumericMatrix der1(N,1);
    der1.fill(0);
    double ll1=0;
    double tmp1=0;
    double tmp2=0;
    for (int nn=0; nn<N; nn++){
        if (y[nn] >= y_value){
            thresh_upp1 = thresh_upp[ y[nn] ];
            thresh_low1 = thresh_low[ y[nn] ];
            tmp2 = ::Rf_pnorm5( thresh_upp1, ypred[nn], 1.0, 1, 0);
            tmp1 = ::Rf_pnorm5( thresh_low1, ypred[nn], 1.0, 1, 0);
            ll1 = std::log( tmp2 - tmp1 + eps );
            der1(nn,0) = ( - ll1 + ll0(nn,0) ) / h;
        }
    }

    //---- OUTPUT
    return der1;
}
///********************************************************************

///********************************************************************
///** mdmb_rcpp_oprobit_derivative_logthresh_chain_rule
// [[Rcpp::export]]
Rcpp::NumericMatrix mdmb_rcpp_oprobit_derivative_logthresh_chain_rule(
    Rcpp::NumericVector thresh_der, Rcpp::NumericVector probs, Rcpp::NumericVector dens_upp,
    Rcpp::NumericVector dens_low, Rcpp::IntegerVector y, double eps, int y_value )
{
    int N = y.size();
    int K = thresh_der.size() - 2;
    double tmp=0;
    Rcpp::NumericMatrix der1(N,1);
    der1.fill(0);
    for (int nn=0; nn<N; nn++){
        if (y[nn] >= y_value){
            tmp=0;
            // der1b <- ( - dens_upp * thresh_der[ y + 2] + dens_low * thresh_der[ y + 1] ) / probs
            if (y[nn] < K){
                tmp = -dens_upp[nn] * thresh_der[ y[nn] + 1];
            }
            if (y[nn] >= 2){
                tmp += dens_low[nn] * thresh_der[ y[nn] ];
            }
            tmp = tmp / ( probs[nn] + eps );
            der1(nn,0) = tmp;
        }
    }
    //---- OUTPUT
    return der1;
}
///********************************************************************
