#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

#ifndef PP_H_
#define PP_H_

class Pp
{
  int npoints;
  int dim;
  NumericMatrix X;
  NumericMatrix bbox;
  NumericVector boxlen;
  NumericVector marks;
  IntegerVector types;
  std::vector<std::vector <int> > edges;
  double (Pp::*dist)(int*, int*);
  std::vector<double> distTriangle;
  std::vector<double> * pdists;

  double distEuclidian(int*, int*);
  double distGreatCircle(int*, int*);
  double distPrecalculated(int*, int*);

  double (Pp::*translationWeight)(int*, int*);
  double translationWeightAll1(int *, int *);
  std::vector<double> translationWeightTriangle;

  std::vector<int> typevec;

public:
  Pp(NumericMatrix, NumericMatrix );
  virtual ~Pp();

  void setMarks(NumericVector );
  void setTypes(IntegerVector );

  double getCoord(int *i, int *d);
  int    size();
  int    getDim();
  double getMark(int *);
  int    getType(int *);
  double getDist(int *, int *);
  double getAngle(int *, int *);
  double getTranslationWeight(int *, int *);
};

#endif /*PP_H_*/
