/***************************************************************************
*
* $Id: StIstRawHitCollection.h,v 1.8 2015/12/22 20:16:27 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* A collection of StIstRawHit classes, and basically is a wrapper for a
* raw hits vector. One instance corresponds to one ladder.
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
   StIstRawHitCollection(int ladder = 0);
   //deconstructor
   ~StIstRawHitCollection();

   vector<StIstRawHit *> &getRawHitVec();
   const vector<StIstRawHit *> &getRawHitVec() const;

   //sort internal vector by raw hit geometry ID
   void sortByGeoId();

   //size of internal vector
   size_t getNumRawHits() const;

   //modify/access the ladder
   unsigned char getLadder() const;
   void setLadder( int ladder );

   //clear
   void Clear( Option_t *opt = "" );
   using StObject::Print;
   void Print(int nTimeBins) const;

   //get pointer to a raw hit by channel ID
   StIstRawHit *getRawHit( int elecId );
   void         addRawHit( int elecId, StIstRawHit* istRawHit);

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


/***************************************************************************
*
* $Log: StIstRawHitCollection.h,v $
* Revision 1.8  2015/12/22 20:16:27  smirnovd
* StIstRawHitCollection: Added method to add/update hits in internal stl container
*
* Revision 1.7  2014/09/09 08:23:46  ypwang
* all unsgined char was updated to int type as Victor P. suggested
*
* Revision 1.6  2014/09/08 19:06:57  smirnovd
* Added Print() methods to print out properties of StIstCluster and StIstRawHit objects and their respective collections
*
* Revision 1.5  2014/02/24 14:24:40  ypwang
* get rid of StIstRawHitCollection::removeFlagged()
*
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHitCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/
