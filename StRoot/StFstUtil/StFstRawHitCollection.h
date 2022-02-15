#ifndef StFstRawHitCollection_hh
#define StFstRawHitCollection_hh

#include "StObject.h"
#include "StFstRawHit.h"
#include <algorithm>


/**
 * A collection of StFstRawHit classes, and basically is a wrapper for a
 * raw hits vector. One instance corresponds to one wedge.
 *
 * \author Shenghui Zhang
 * \date Sep 2021
 */
class StFstRawHitCollection : public StObject
{
public:
   //constructors
   StFstRawHitCollection(int wedge = 0);
   //deconstructor
   ~StFstRawHitCollection();

   vector<StFstRawHit *> &getRawHitVec();
   const vector<StFstRawHit *> &getRawHitVec() const;

   //sort internal vector by raw hit geometry ID
   void sortByGeoId();

   //size of internal vector
   size_t getNumRawHits() const;

   //modify/access the wedge
   unsigned char getWedge() const;
   void setWedge( int wedge );

   //clear
   void Clear( Option_t *opt = "" );
   using StObject::Print;
   void Print(int nTimeBins) const;

   //get pointer to a raw hit by channel ID
   StFstRawHit *getRawHit( int elecId );

   /// Adds or sets/overwrites a new StFstRawHit corresponding to electronic channel
   /// StFstRawHit::mChannelId.
   void addRawHit(StFstRawHit* fstRawHit);

protected:
   //function used for sorting raw hits by geoId
   static bool rawHitIdLessThan( const StFstRawHit *h1, const StFstRawHit *h2 );

   //data members
   unsigned char mWedge;
   std::vector<StFstRawHit *> mRawHitVec;

   //temporary copy of the pointers to add raw hit indexed by elec Id.
   std::vector<StFstRawHit *> mRawHitElecIdVec;

private:
   ClassDef(StFstRawHitCollection, 1);
};

#endif
