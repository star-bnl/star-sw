#ifndef StiMath_H_INCLUDED
#define StiMath_H_INCLUDED
#include "TObject.h"

class StiMath : public TObject
{
 public:
  virtual ~StiMath(){};
  static void initialize();
  static double chi2(double x, int n);
  static double gamma(double n);
  static double logGamma(double n);

 protected:
  StiMath(){};
  static double _logGamma[200]; 

  ClassDef(StiMath, 1)
};

#endif
