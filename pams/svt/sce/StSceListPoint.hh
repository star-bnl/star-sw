#ifndef STSCELISTPOINT_HH
#define STSCELISTPOINT_HH
#include <stdlib.h>
#include <math.h>
#include <stdiostream.h>
#include "StScePoint.hh"

class StSceListPoint
{
 public:

  StSceListPoint();
  ~StSceListPoint();

  StScePoint*     next(StScePoint *ptr);
  StScePoint*     prev(StScePoint *ptr);
  StScePoint*     first();
  StScePoint*     last();
  int             addNewPoint(StScePoint *ptr);
  int             removePoint(StScePoint *ptr);
  StSceListPoint* addListPoint(StSceListPoint *list);
  StSceListPoint* substractListPoint(StSceListPoint *list);
  StSceListPoint* removeMultipleCount();
  int             renumHits(int last);
  void            exchangeTwoPoints(StScePoint *ptr1,StScePoint *ptr2);
  StSceListPoint* sortPoint();
  int             getSize();

 private:
  int             mListLength;
  StScePoint     *mFirstP;
  StScePoint     *mLastP;
};
#endif
