//// File Name: mdmb_rcpp_linreg.cpp
//// File Version: 0.506

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;



// user includes
const double pi1 = 3.14159265359;
const double pi2 = 1 / std::sqrt(2*pi1);  // 1/sqrt(2*pi)


///********************************************************************
// weighted sd for centered variable
///** mdmb_rcpp_weighted_sd_centered
// [[Rcpp::export]]
double mdmb_rcpp_weighted_sd_centered( Rcpp::NumericVector x, Rcpp::NumericVector w )
{
    double val=0;
    double W=0;
    int N = x.size();
    Rcpp::NumericVector wgt(N);
    double s=0;
    double W2=0;

    for (int nn=0; nn<N; nn++){
        s += w[nn];
    }

    for (int nn=0; nn<N; nn++){
        wgt[nn] = w[nn] / s;
        val += wgt[nn] * x[nn] * x[nn];
        W += wgt[nn];
        W2 += wgt[nn] * wgt[nn];
    }
    //*** copied from stats::cov.wt
    // cov <- switch(match.arg(method), unbiased = crossprod(x)/(1 -
    //    sum(wt^2)), ML = crossprod(x))
    val = std::sqrt( val / ( 1 - W2 ) );

    //--- OUTPUT
    return val;
}
///********************************************************************

///********************************************************************
// weighted sd
///** mdmb_rcpp_weighted_sd
// [[Rcpp::export]]
double mdmb_rcpp_weighted_sd( Rcpp::NumericVector x, Rcpp::NumericVector w )
{
    double val=0;
    double W=0;
    int N = x.size();
    Rcpp::NumericVector wgt(N);
    double s=0;
    double W2=0;
    double M=0;

    for (int nn=0; nn<N; nn++){
        s += w[nn];
    }

    for (int nn=0; nn<N; nn++){
        wgt[nn] = w[nn] / s;
        val += wgt[nn] * x[nn] * x[nn];
        M += wgt[nn] * x[nn];
        W += wgt[nn];
        W2 += wgt[nn] * wgt[nn];
    }
    //*** copied from stats::cov.wt
    // cov <- switch(match.arg(method), unbiased = crossprod(x)/(1 -
    //    sum(wt^2)), ML = crossprod(x))
    val = std::sqrt( ( val - M*M )/ ( 1 - W2 ) );

    //--- OUTPUT
    return val;
}
///********************************************************************

///********************************************************************
// weighted linear regression
///** mdmb_rcpp_lm_wfit
// [[Rcpp::export]]
Rcpp::List mdmb_rcpp_lm_wfit( arma::mat x, arma::colvec y, arma::colvec w )
{
    int np = x.n_cols;
    int N = x.n_rows;
    arma::mat xa(N,np);
    arma::colvec ya(N);
    double w_nn=0;

    // multiply original matrices by weights
    for (int nn=0; nn<N; nn++){
        if (w(nn,0) == 1){
            ya(nn,0) = y(nn,0);
            for (int pp=0; pp<np; pp++){
                xa(nn,pp) = x(nn,pp);
            }
        } else {
            w_nn = std::sqrt( w(nn,0) );
            ya(nn,0) = y(nn,0) * w_nn;
            for (int pp=0; pp<np; pp++){
                xa(nn,pp) = x(nn,pp) * w_nn;
            }
        }
    }

    // regression estimation
    arma::colvec coef = arma::solve(xa, ya);    // fit model y ~ X
    arma::colvec fitted = x*coef;   // fitted values
    arma::colvec res  = y - fitted;           // residuals

    //---- OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("coef") = coef,
                Rcpp::Named("fitted") = fitted,
                Rcpp::Named("res") = res
        );
}
///********************************************************************

///********************************************************************
// normalize posterior
///** mdmb_rcpp_frm_normalize_posterior
// [[Rcpp::export]]
Rcpp::NumericVector mdmb_rcpp_frm_normalize_posterior(
            Rcpp::NumericVector post, Rcpp::IntegerVector case_ )
{
    int NC = post.size();
    Rcpp::NumericVector post1(NC);
    post1.fill(1);
    double val=0;
    int case_temp=0;

    int ii_start=0;
    int ii_end=0;
    int count = 0;

    for (int cc=0; cc<NC; cc++){
        if ( case_[cc] > case_temp){
            if (cc>0){
                if (count!=1){
                    for (int ii=ii_start; ii<ii_end; ii++){
                        post1[ii] = post[ii] / val;
                    }
                }
            }
            case_temp = case_[cc];
            ii_start=cc;
            val = post[cc];
            count = 1;
        } else {
            val += post[cc];
            count ++;
        }
        ii_end=cc+1;
    }
    for (int ii=ii_start; ii<ii_end; ii++){
        post1[ii] = post[ii] / val;
    }

    //---- OUTPUT
    return post1;
}
///********************************************************************

// Rcpp::Rcout << "cc=" << cc << " ii_start= " << ii_start <<
//                " ii_end=" << ii_end << " count=" << count << std::endl;
