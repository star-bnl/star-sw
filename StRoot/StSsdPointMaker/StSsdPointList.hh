// $Id: StSsdPointList.hh,v 1.2 2005/03/18 14:19:05 lmartin Exp $
//
// $Log: StSsdPointList.hh,v $
// Revision 1.2  2005/03/18 14:19:05  lmartin
// missing CVS header added
//

#ifndef STSSDPOINTLIST_HH
#define STSSDPOINTLIST_HH

class StSsdPoint;

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
  int             mListLength;
  StSsdPoint     *mFirstPoint;
  StSsdPoint     *mLastPoint;
};
#endif
