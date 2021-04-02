#ifndef ROOT_TGraphAsymmErrorsWithReset
#define ROOT_TGraphAsymmErrorsWithReset

#include "TGraphAsymmErrors.h"

class TGraphAsymmErrorsWithReset : public TGraphAsymmErrors {
 public:
  void Reset();
  ClassDef(TGraphAsymmErrorsWithReset,1)
};

#endif
