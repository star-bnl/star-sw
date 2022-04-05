//$Id: StSstPointList.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstPointList.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTPOINTLIST_HH
#define STSSTPOINTLIST_HH

#include "StSstPoint.hh"

class StSstPointList
{
 public:
  StSstPointList();
  ~StSstPointList();
  StSstPointList(const StSstPointList & originalPointList);
  StSstPointList& operator=(const StSstPointList originalPointList);

  StSstPoint*     next(StSstPoint *ptr);
  StSstPoint*     prev(StSstPoint *ptr);
  StSstPoint*     first();
  StSstPoint*     last();
  Int_t             getSize();
  Int_t             addNewPoint(StSstPoint *ptr);
  Int_t             removePoint(StSstPoint *ptr);
  void            exchangeTwoPoints(StSstPoint *ptr1,StSstPoint *ptr2);
  StSstPointList* addPointList(StSstPointList *list);
  StSstPointList* substractPointList(StSstPointList *list);
  StSstPointList* removeMultipleCount();
  StSstPointList* sortPoint();

  Int_t             renumHits(Int_t last);

 private:
  Int_t             mListLength;
  StSstPoint     *mFirstPoint;
  StSstPoint     *mLastPoint;
};
#endif
