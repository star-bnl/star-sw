#ifndef STSCMLISTPOINT_HH
#define STSCMLISTPOINT_HH
#include "StScmPoint.hh"

class StScmListPoint
{
 public:

  StScmListPoint();
  ~StScmListPoint();

  StScmPoint*     next(StScmPoint *ptr);
  StScmPoint*     prev(StScmPoint *ptr);
  StScmPoint*     first();
  StScmPoint*     last();
  int             getSize();
  int             addNewPoint(StScmPoint *ptr);
  int             removePoint(StScmPoint *ptr);
  void            exchangeTwoPoints(StScmPoint *ptr1,StScmPoint *ptr2);
  StScmListPoint* addListPoint(StScmListPoint *list);
  StScmListPoint* substractListPoint(StScmListPoint *list);
  StScmListPoint* removeMultipleCount();
  StScmListPoint* sortPoint();
  int             renumHits(int last);

 private:
  StScmListPoint(const StScmListPoint & originalListPoint);
  StScmListPoint& operator=(const StScmListPoint originalListPoint);

  int             mListLength;
  StScmPoint     *mFirstS;
  StScmPoint     *mLastS;
};
#endif
