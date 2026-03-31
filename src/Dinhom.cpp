#include <Rcpp.h>
#include "Pp.h"
#include "Graph.h"

using namespace Rcpp;


// optimized to use graph
// [[Rcpp::export]]
NumericVector Dinhom_c(NumericMatrix x,
                       NumericMatrix bbox,
                       NumericVector lambda,
                       NumericVector bdist,
                       double inf_int,
                       NumericVector r) {
  int nr=r.size();
  Pp pp(x, bbox);
  Graph graph(pp, 1, r(nr-1), 0);
  //graph.setdbg(1);
  NumericVector out(nr);
  int i, j, ri;
  double w;
  int n;
  double l0 = inf_int;
  //double l0 = 99999999;
  // infimum of lambda per type
  //for(i=0; i < lambda.size();i++) if(lambda(i) < l0) l0 = lambda(i);

  for(ri = nr-1; ri >-1; ri--){
    n = 0;
    graph.set_par(r(ri));
    graph.sg_calc();
    for(i=0; i < x.nrow(); i++) {
      if(bdist(i) > r(ri)){
        w = 1;
        n++;
        for(j = 0; j < graph.edges.at(i).size(); j++) {
          w *= 1 - l0 / lambda(graph.edges.at(i).at(j)-1);
        }
        out(ri) += w;
      }
    }
    if(n>0) out(ri) /= n;
  }
  return out;
}

// // [[Rcpp::export]]
// NumericVector Dinhom_slow_c(NumericMatrix x,
//                             NumericMatrix bbox,
//                             NumericVector lambda,
//                             NumericVector bdist, NumericVector r) {
//   int nr=r.size();
//   Pp pp(x, bbox);
//   NumericVector out(nr);
//   int i, j, ri;
//   double w;
//   int ni, n;
//   double l0 = 99999999;
//   for(i=0; i < lambda.size();i++) if(lambda(i) < l0) l0 = lambda(i);
//
//   for(ri = 0; ri < nr; ri++){
//     n = 0;
//     for(i=0; i < x.nrow(); i++) {
//       if(bdist(i) > r(ri)){
//         w = 1;
//         n++;
//         ni = 0;
//         for(j = 0; j < x.nrow(); j++) {
//           if(i != j) {
//             if(pp.getDist(&i, &j) < r(ri)) {
//               ni++;
//               w *= 1 - l0 / lambda(j);
//             }
//           }
//         }
//         out(ri) += w;
//       }
//     }
//     if(n>0) out(ri) /= n;
//   }
//   return out;
// }
