// $Id: StSceListPoint.hh,v 1.2 2005/05/12 08:22:10 lmartin Exp $
//
// $Log: StSceListPoint.hh,v $
// Revision 1.2  2005/05/12 08:22:10  lmartin
// cvs tags added and histograms in the .hist branch
//

#ifndef STSCELISTPOINT_HH
#define STSCELISTPOINT_HH
#include <stdlib.h>
#include <math.h>
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
