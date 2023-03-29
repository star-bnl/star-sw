#include "StEvent/StFstConsts.h"
#include "StFstUtil/StFstCollection.h"
#include "StEvent/StEnumerations.h"
//constructor
StFstCollection::StFstCollection() : StObject()
{
   // set the wedge field for some of the collections
   for ( int i = 0; i < kFstNumWedges; ++i ) {
      mRawHitCollection[i].setWedge( i );
      mClusterCollection[i].setWedge( i );
   }

   mNumTimeBins = kFstNumTimeBins; //reasonable default
};

//deconstructor
StFstCollection::~StFstCollection()
{
   for ( int i = 0; i < kFstNumWedges; ++i ) {
      mRawHitCollection[i].Clear("");
      mClusterCollection[i].Clear("");
   }
};

unsigned char StFstCollection::getNumTimeBins() const
{
   return mNumTimeBins;
};

void StFstCollection::setNumTimeBins(unsigned char nTimeBins)
{
   mNumTimeBins = nTimeBins;
};

StFstRawHitCollection *StFstCollection::getRawHitCollection( int wedge )
{
   return ((wedge >= 0 && wedge < kFstNumWedges) ? &mRawHitCollection[wedge] : 0 );
};

const StFstRawHitCollection *StFstCollection::getRawHitCollection(int wedge ) const
{
   return ((wedge >= 0 && wedge < kFstNumWedges) ? &mRawHitCollection[wedge] : 0  );
};

StFstClusterCollection *StFstCollection::getClusterCollection( int wedge )
{
   return ((wedge >= 0 && wedge < kFstNumWedges) ? &mClusterCollection[wedge] : 0 );
};

const StFstClusterCollection *StFstCollection::getClusterCollection( int wedge ) const
{
   return ((wedge >= 0 && wedge < kFstNumWedges) ? &mClusterCollection[wedge] : 0 );
};

//sum of all the raw hits over all wedges
size_t StFstCollection::getNumRawHits() const
{
   size_t n = 0;

   for ( const StFstRawHitCollection *ptr = &mRawHitCollection[0]; ptr != &mRawHitCollection[kFstNumWedges]; ++ptr )
      n += ptr->getNumRawHits();

   return n;
};

//number of raw hits on one wedge
size_t StFstCollection::getNumRawHits( int wedge ) const
{
   return ((wedge >= 0 && wedge < kFstNumWedges) ? mRawHitCollection[wedge].getNumRawHits() : 0 );
};

//sum of all the clusters over all wedges
size_t StFstCollection::getNumClusters() const
{
   size_t n = 0;

   for ( const StFstClusterCollection *ptr = &mClusterCollection[0]; ptr != &mClusterCollection[kFstNumWedges]; ++ptr )
      n += ptr->getNumClusters();

   return n;
};

//number of clusters on one wedge
size_t StFstCollection::getNumClusters( int wedge ) const
{
   return ((wedge >= 0 && wedge < kFstNumWedges) ? mClusterCollection[wedge].getNumClusters() : 0 );
};


ClassImp(StFstCollection);
