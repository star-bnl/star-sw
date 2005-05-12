// $Id: StSceListComp.hh,v 1.2 2005/05/12 08:22:10 lmartin Exp $
//
// $Log: StSceListComp.hh,v $
// Revision 1.2  2005/05/12 08:22:10  lmartin
// cvs tags added and histograms in the .hist branch
//

#ifndef STSCELISTCOMP_HH
#define STSCELISTCOMP_HH
#include <stdlib.h>
#include <math.h>
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
