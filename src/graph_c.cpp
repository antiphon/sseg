#include <Rcpp.h>
#include "Pp.h"
#include "graph.h"
using namespace Rcpp;



//unoptimized
// [[Rcpp::export]]
List graph_c(NumericMatrix x, NumericMatrix bb,
             int gtype=1,
             NumericVector gpar=0,
             double MaxR=0){
  Pp pp(x, bb);
  Graph g(pp, gtype, gpar, MaxR);
  g.sg_calc();
  return g.toList();
}
