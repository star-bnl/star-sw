#ifndef STSSDSTRIPLIST_HH
#define STSSDSTRIPLIST_HH
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StSsdStrip.hh"
#include "sls_ctrl.h"

class StSsdStripList
{
 public:
  StSsdStripList();
  ~StSsdStripList();

  StSsdStrip* next(StSsdStrip *ptr);
  StSsdStrip* prev(StSsdStrip *ptr);
  StSsdStrip* first();
  StSsdStrip* last();
  StSsdStrip* getStrip(int idStrip);
  int        addNewStrip(StSsdStrip *ptr);
  void       exchangeTwoStrips(StSsdStrip *ptr1, StSsdStrip *ptr2);
  void       sortStrip();
  void       setSigma(int iStrip, int iSigma, sls_ctrl_st *sls_ctrl);
  int        removeStrip(StSsdStrip *ptr);
  int        getSize();
  int*       getListAdc(int idStrip, int SizeCluster);
  int        isSorted();

 private:
  int        mListLength;
  StSsdStrip *mFirstStrip;
  StSsdStrip *mLastStrip;
};
#endif
