#ifndef STSSDPOINTLIST_HH
#define STSSDPOINTLIST_HH
#include "StSsdPoint.hh"

class StSsdPointList
{
 public:

  StSsdPointList();
  ~StSsdPointList();

  StSsdPoint*     next(StSsdPoint *ptr);
  StSsdPoint*     prev(StSsdPoint *ptr);
  StSsdPoint*     first();
  StSsdPoint*     last();
  int             getSize();
  int             addNewPoint(StSsdPoint *ptr);
  int             removePoint(StSsdPoint *ptr);
  void            exchangeTwoPoints(StSsdPoint *ptr1,StSsdPoint *ptr2);
  StSsdPointList* addPointList(StSsdPointList *list);
  StSsdPointList* substractPointList(StSsdPointList *list);
  StSsdPointList* removeMultipleCount();
  StSsdPointList* sortPoint();
  int             renumHits(int last);

 private:
  StSsdPointList(const StSsdPointList & originalPointList);
  StSsdPointList& operator=(const StSsdPointList originalPointList);

  int             mListLength;
  StSsdPoint     *mFirstPoint;
  StSsdPoint     *mLastPoint;
};
#endif
