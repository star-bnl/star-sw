// $Id: StIstRawHitCollection.h,v 1.10 2016/01/07 22:15:29 smirnovd Exp $

#ifndef StIstRawHitCollection_hh
#define StIstRawHitCollection_hh

#include "StObject.h"
#include "StIstRawHit.h"
#include <algorithm>


/**
 * A collection of StIstRawHit classes, and basically is a wrapper for a
 * raw hits vector. One instance corresponds to one ladder.
 *
 * \author Yaping Wang
 * \date March 2013
 */
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

   /// Adds or sets/overwrites a new StIstRawHit corresponding to electronic channel
   /// StIstRawHit::mChannelId.
   void addRawHit(StIstRawHit* istRawHit);

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
