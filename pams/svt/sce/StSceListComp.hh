#ifndef STSCELISTCOMP_HH
#define STSCELISTCOMP_HH
#include <stdlib.h>
#include <math.h>
#include <stdiostream.h>
#include "StSceComp.hh"

class StSceListComp
{
 public:

  StSceListComp();
  ~StSceListComp();

  StSceComp*     next(StSceComp *ptr);
  StSceComp*     prev(StSceComp *ptr);
  StSceComp*     first();
  StSceComp*     last();
  int            addNewComp(StSceComp *ptr);
  void           exchangeTwoComps(StSceComp *ptr1,StSceComp *ptr2);
  StSceListComp* sortComp();
  int            getSize();

 private:
  int            mListLength;
  StSceComp     *mFirstC;
  StSceComp     *mLastC;
};
#endif
