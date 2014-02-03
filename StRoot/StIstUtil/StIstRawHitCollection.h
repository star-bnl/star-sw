/***************************************************************************
*
* $Id: StIstRawHitCollection.h,v 1.3 2014/02/03 16:12:20 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* A collection of StIstRawHit classes, and basically is a wrapper for a
* raw hits vector. One instance corresponds to one ladder.
****************************************************************************
*
* $Log: StIstRawHitCollection.h,v $
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHitCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstRawHitCollection_hh
#define StIstRawHitCollection_hh

#include "StObject.h"
#include "StIstRawHit.h"
#include <algorithm>

class StIstRawHitCollection : public StObject
{
public:
   //constructors
   StIstRawHitCollection(unsigned char ladder = 0);
   //deconstructor
   ~StIstRawHitCollection();

   vector<StIstRawHit *> &getRawHitVec();
   const vector<StIstRawHit *> &getRawHitVec() const;

   //sort internal vector by raw hit geometry ID
   void sortByGeoId();

   //remove all hits with negative geometry IDs
   void removeFlagged();

   //size of internal vector
   size_t getNumRawHits() const;

   //modify/access the ladder
   unsigned char getLadder() const;
   void setLadder( unsigned char ladder );

   //clear
   void Clear( Option_t *opt = "" );

   //get pointer to a raw hit by channel ID
   StIstRawHit *getRawHit( int elecId );

protected:
   //function used for sorting raw hits by geoId
   static bool rawHitIdLessThan( const StIstRawHit *h1, const StIstRawHit *h2 );

   //data members
   unsigned char mLadder;
   std::vector<StIstRawHit *> mRawHitVec;

   //temporary copy of the pointers to add raw hit indexed by elec Id.
   std::vector<StIstRawHit *> mRawHitElecIdVec;

private:
   ClassDef(StIstRawHitCollection, 1);
};
#endif
