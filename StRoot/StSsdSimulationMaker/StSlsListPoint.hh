// $Id: StSlsListPoint.hh,v 1.2 2005/05/13 08:39:31 lmartin Exp $
//
// $Log: StSlsListPoint.hh,v $
// Revision 1.2  2005/05/13 08:39:31  lmartin
// CVS tags added
//

#ifndef STSLSLISTPOINT_HH
#define STSLSLISTPOINT_HH
#include "StSlsPoint.hh"

class StSlsListPoint
{
 public:

  StSlsListPoint();
  ~StSlsListPoint();

  StSlsPoint*     next(StSlsPoint *ptr);
  StSlsPoint*     prev(StSlsPoint *ptr);
  StSlsPoint*     first();
  StSlsPoint*     last();
  int             getSize();
  int             addNewPoint(StSlsPoint *ptr);
  int             removePoint(StSlsPoint *ptr);
  void            exchangeTwoPoints(StSlsPoint *ptr1, StSlsPoint *ptr2);
  StSlsListPoint* addListPoint(StSlsListPoint *list);
  StSlsListPoint* substractListPoint(StSlsListPoint *list);
  StSlsListPoint* removeMultipleCount();
  StSlsListPoint* sortPoint();
  int             renumHits(int last);

private:
  int             mListLength;
  StSlsPoint     *mFirstP;
  StSlsPoint     *mLastP;
};
#endif
