//// File Name: mdmb_rcpp_test_miceadds_include.cpp
//// File Version: 0.12


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(miceadds)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>


#include <miceadds.h>
// #include "p:\Eigene_Projekte\R-Routinen\IRT-Functions\miceadds_Package\2.11\miceadds_work\inst\include\miceadds.h"


using namespace Rcpp;
using namespace arma;
using namespace miceadds;


///********************************************************************
///** mdmb_test_rtnorm2
// [[Rcpp::export]]
double mdmb_test_rtnorm2( double mu, double sigma, double lower, double upper)
{
    double z = miceadds::miceadds_rcpp_rtnorm_double( mu, sigma, lower, upper );
    return z;
}
///********************************************************************
