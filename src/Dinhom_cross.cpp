#include <Rcpp.h>
#include "Pp.h"
#include "graph.h"

using namespace Rcpp;


bool is_in(int i, IntegerVector v){
  for(int k=0; k < v.size(); k++) if(i==v(k)) return true;
  return false;
}

//unoptimized
// [[Rcpp::export]]
NumericVector Dinhom_pair_c(int from, int to,
                             NumericMatrix x,
                             NumericMatrix bbox,
                             NumericVector lambda,
                             IntegerVector type,
                             NumericVector bdist,
                             NumericVector rho0,
                             NumericVector r, int nmarks) {
  int nr=r.size();
  Pp pp(x, bbox);
  NumericVector out(nr);
  int i, j, ri;
  double w;
  int n;
  NumericVector li;
//   NumericVector rho0(nmarks);
//   for(i = 0; i < nmarks; i++) rho0(i) = 99999999;
//   for(i=0; i < x.nrow(); i++) if(lambda(i) < rho0(type(i)-1)) rho0(type(i)-1) = lambda(i);

  for(ri = 0; ri < nr; ri++){
    n = 0;
    for(i=0; i < x.nrow(); i++) {
      if(bdist(i) > r(ri) && type(i) == from){
        n++;
        w = 1;
        for(j = 0; j < x.nrow(); j++) {
          if(i != j && type(j) == to) {
            if(pp.getDist(&i, &j) < r(ri)) {
              w *= 1 - rho0(type(j)-1) / lambda(j);
            }
          }
        }
        out(ri) += w;
      }
    }
    if(n>0) out(ri) /= n;
  }
  return out;
}

// faster
// [[Rcpp::export]]
NumericVector Dinhom_pair_2_c(int from, int to,
                            NumericMatrix x,
                            NumericMatrix bbox,
                            NumericVector lambda,
                            IntegerVector type,
                            NumericVector bdist,
                            NumericVector rho0,
                            NumericVector r, int nmarks) {
  int nr=r.size();
  Pp pp(x, bbox);
  NumericVector out(nr);
  int i, j, ri;
  NumericVector li;
  NumericVector ww(nr);
  NumericVector nn(nr);
  // defaults
  for(i = 0; i < nr; i++) {ww(i) = 1; nn(i)=0;}
//   NumericVector rho0(nmarks);
//   for(i = 0; i < nmarks; i++) rho0(i) = 99999999;
//   for(i=0; i < x.nrow(); i++) if(lambda(i) < rho0(type(i)-1)) rho0(type(i)-1) = lambda(i);
  double d;
  int n;
  //
  for(i=0; i < x.nrow(); i++) {
    if(type(i) == from){
      n=0;
      for(j = 0; j < x.nrow(); j++) {
        if(i != j && type(j) == to) {
          n++;
          d = pp.getDist(&i, &j);
          for(ri = 0; ri < nr; ri++){
            if(bdist(i) > r(ri) && d < r(ri)) {
                ww(ri) *= 1 - rho0(type(j)-1) / lambda(j);
              }
          }
        }
      }
      for(ri = 0; ri < nr; ri++){
        if(bdist(i) > r(ri)){
          nn(ri) += 1/lambda(i);
          if(n>0) out(ri) += ww(ri) / lambda(i);
        }
        ww(ri) = 1; // reset for next i
      }
    }
  }
  // one more time, for the averaging
  for(ri = 0; ri < nr; ri++) {
    if(nn(ri)>0) out(ri) /= nn(ri);
  }
  return out;
}



// using graph. Slower.
// [[Rcpp::export]]
NumericVector Dinhom_pair_graph_c(int from, int to,
                                  NumericMatrix x,
                                  NumericMatrix bbox,
                                  NumericVector lambda,
                                  IntegerVector type,
                                  NumericVector bdist,
                                  NumericVector rho0,
                                  NumericVector r,
                                  int nmarks) {
  int nr=r.size();
  Pp pp(x, bbox);
  Graph graph(pp, 1, r(0), 0);
  NumericVector out(nr);
  int i, j, k, ri;
  double w;
  int n;
  NumericVector li;
//   NumericVector rho0(nmarks);
//   for(i = 0; i < nmarks; i++) rho0(i) = 99999999;
//   for(i=0; i < x.nrow(); i++) if(lambda(i) < rho0(type(i)-1)) rho0(type(i)-1) = lambda(i);

  for(ri = nr-1; ri > -1; ri--){
    n = 0;
    graph.set_par(r(ri));
    graph.sg_calc();
    for(i=0; i < x.nrow(); i++) {
      if(bdist(i) > r(ri) && type(i) == from){
        n++;
        w = 1;
        for(j = 0; j < graph.edges.at(i).size(); j++) {
          k = graph.edges.at(i).at(j)-1;
          if(type(k) == to) {
            w *= 1 - rho0(type(k)-1) / lambda(k);
          }
        }
        out(ri) += w;
      }
    }
    if(n>0) out(ri) /= n;
  }
  return out;
}
