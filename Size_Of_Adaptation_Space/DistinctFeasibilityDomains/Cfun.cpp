#include <Rcpp.h>
#include <iostream>     // std::cout
#include <valarray>     // std::valarray
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;
using  Eigen::Map;
using  Eigen::VectorXd;
typedef  Map<VectorXd>  MapVecd;


// [[Rcpp::export]]
NumericMatrix cnt_cpp(NumericMatrix x, NumericVector a, NumericVector b) {
  int r = a.size(), c = b.size();
  NumericMatrix g = (clone(x)); // To don't modify the original matrix
  for(int i = 0; i < r; i++){
    for(int j = 0; j < c; j++){
      g(a(i)-1,b(j) - 1) = 0;
    }
  }
  
  return g;
}
