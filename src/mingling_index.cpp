#include <Rcpp.h>
#include "Pp.h"
#include "Graph.h"

using namespace Rcpp;


// optimized to use graph
// 1st: 1 - E(same)/E(any)

// [[Rcpp::export]]
NumericVector mingling_index_c_1st(NumericMatrix x,
                                   IntegerVector type, int ntypes,
                                   NumericMatrix bbox,
                                   NumericVector bdist,
                                   NumericVector r,
                                   IntegerVector targets,
                                   int ntype) {
  int nr=r.size();
  Pp pp(x, bbox);
  Graph graph(pp, ntype, r(nr-1), 0); // ntype 1 geometric 2 knn, 0 for no pre-graph needed
  //graph.setdbg(1);
  bool isgeom = ntype == 1;
  int ntargets = targets.size();
  NumericMatrix out(nr, ntargets);
  int i, j, ri, itype;
  int ni, n = pp.size();
  NumericVector any(ntargets);
  bool isin;
  int ti;
  // gather each points target type index
  IntegerVector tivec(n);
  for(i = 0; i < n; i++) {
    isin = false;
    for(ti = 0; ti < ntargets; ti++ ) if(type(i) == targets(ti)) {isin=true;break;}
    tivec(i) = isin ? ti : -1;
  }
  //traverse r backwards = clip largest graph
  for(ri = nr-1; ri >-1; ri--){
    checkUserInterrupt();
    graph.set_par(r(ri));
    graph.sg_calc();
    // zero counters
    for(i=0;i < ntargets;i++)any(i) = 0.0;
    // inspect each point
    for(i=0; i < n; i++) {
      ti = tivec(i);
      if( (ti > -1) && (!isgeom || bdist(i) > r(ri) ) ){ // relevant point
        itype = type(i);
        // ok go
        ni = graph.edges.at(i).size();
        any(ti) += ni; // any types
        for(j = 0; j < ni; j++) { // same types
            if( type(graph.edges.at(i).at(j)-1) == itype ) out(ri, ti)++;
        }
      }
    }
    for(ti = 0; ti < ntargets; ti++) {
      if(any(ti)>0) out(ri, ti) = 1 - out(ri,ti) / any(ti);
      else out(ri, ti) = NA_REAL;
    }
  }
  return out;
}


// 2nd: 1 - E(same/any)
// [[Rcpp::export]]
NumericVector mingling_index_c_2nd(NumericMatrix x,
                                   IntegerVector type, int ntypes,
                                   NumericMatrix bbox,
                                   NumericVector bdist,
                                   NumericVector r,
                                   IntegerVector targets,
                                   int ntype) {
  int nr=r.size();
  Pp pp(x, bbox);
  Graph graph(pp, ntype, r(nr-1), 0);
  //graph.setdbg(1);
  bool isgeom = ntype == 1;
  int ntargets = targets.size();
  NumericMatrix out(nr, ntargets);
  int i, j, ri, itype;
  int n = pp.size();
  NumericVector any(ntargets);
  bool isin;
  int ti;
  // gather each points target type index
  IntegerVector tivec(n);
  for(i = 0; i < n; i++) {
    isin = false;
    for(ti = 0; ti < ntargets; ti++ ) if(type(i) == targets(ti)) {isin=true;break;}
    tivec(i) = isin ? ti : -1;
  }
  NumericVector ok(ntargets);
  double ni, nisame;
  //traverse r backwards = clip largest graph
  for(ri = nr-1; ri >-1; ri--){
    checkUserInterrupt();
    graph.set_par(r(ri));
    graph.sg_calc();
    // zero counter for averaging
    for(ti=0; ti < ntargets; ti++) ok(ti) = 0.0;
    // inspect each point
    for(i=0; i < n; i++) {
      ti = tivec(i);
      if( (ti > -1) && (!isgeom || bdist(i) > r(ri) ) ){ // relevant point?
        // ok go
        itype = type(i);
        nisame = 0.0;
        ni = graph.edges.at(i).size(); // any types
        for(j = 0; j < ni; j++) { // same types
          if( type( graph.edges.at(i).at(j)-1 ) == itype ) nisame++;
        }
        if(ni > 0) {
          out(ri, ti) += nisame / ni;
          ok(ti)++;
        }
      }
    }
    for(ti = 0; ti < ntargets; ti++)
      if(ok(ti)>0) out(ri, ti) = 1.0 - out(ri,ti)/ok(ti);
      else out(ri, ti) = NA_REAL;
  }
  return out;
}



