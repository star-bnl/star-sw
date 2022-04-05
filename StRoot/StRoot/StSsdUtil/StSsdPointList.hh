// $Id: StSsdPointList.hh,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdPointList.hh,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:19:05  lmartin
// missing CVS header added
//

#ifndef STSSDPOINTLIST_HH
#define STSSDPOINTLIST_HH

#include "StSsdPoint.hh"

class StSsdPointList
{
 public:
  StSsdPointList();
  ~StSsdPointList();
  StSsdPointList(const StSsdPointList & originalPointList);
  StSsdPointList& operator=(const StSsdPointList originalPointList);

  StSsdPoint*     next(StSsdPoint *ptr);
  StSsdPoint*     prev(StSsdPoint *ptr);
  StSsdPoint*     first();
  StSsdPoint*     last();
  Int_t             getSize();
  Int_t             addNewPoint(StSsdPoint *ptr);
  Int_t             removePoint(StSsdPoint *ptr);
  void            exchangeTwoPoints(StSsdPoint *ptr1,StSsdPoint *ptr2);
  StSsdPointList* addPointList(StSsdPointList *list);
  StSsdPointList* substractPointList(StSsdPointList *list);
  StSsdPointList* removeMultipleCount();
  StSsdPointList* sortPoint();

  Int_t             renumHits(Int_t last);

 private:
  Int_t             mListLength;
  StSsdPoint     *mFirstPoint;
  StSsdPoint     *mLastPoint;
};
#endif
