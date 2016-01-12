// $Id: StIstCollection.cxx,v 1.10 2016/01/05 16:29:25 smirnovd Exp $

#include "StIstConsts.h"
#include "StIstCollection.h"


//constructor
StIstCollection::StIstCollection() : StObject()
{
   // set the ladder field for some of the collections
   for ( int i = 0; i < kIstNumLadders; ++i ) {
      mRawHitCollection[i].setLadder( i );
      mClusterCollection[i].setLadder( i );
   }

   mNumTimeBins = kIstNumTimeBins; //reasonable default
};

//deconstructor
StIstCollection::~StIstCollection()
{
   for ( int i = 0; i < kIstNumLadders; ++i ) {
      mRawHitCollection[i].Clear("");
      mClusterCollection[i].Clear("");
   }
};

unsigned char StIstCollection::getNumTimeBins() const
{
   return mNumTimeBins;
};

void StIstCollection::setNumTimeBins(unsigned char nTimeBins)
{
   mNumTimeBins = nTimeBins;
};

StIstRawHitCollection *StIstCollection::getRawHitCollection( int ladder )
{
   return ((ladder >= 0 && ladder < kIstNumLadders) ? &mRawHitCollection[ladder] : 0 );
};

const StIstRawHitCollection *StIstCollection::getRawHitCollection(int ladder ) const
{
   return ((ladder >= 0 && ladder < kIstNumLadders) ? &mRawHitCollection[ladder] : 0  );
};

StIstClusterCollection *StIstCollection::getClusterCollection( int ladder )
{
   return ((ladder >= 0 && ladder < kIstNumLadders) ? &mClusterCollection[ladder] : 0 );
};

const StIstClusterCollection *StIstCollection::getClusterCollection( int ladder ) const
{
   return ((ladder >= 0 && ladder < kIstNumLadders) ? &mClusterCollection[ladder] : 0 );
};

//sum of all the raw hits over all ladders
size_t StIstCollection::getNumRawHits() const
{
   size_t n = 0;

   for ( const StIstRawHitCollection *ptr = &mRawHitCollection[0]; ptr != &mRawHitCollection[kIstNumLadders]; ++ptr )
      n += ptr->getNumRawHits();

   return n;
};

//number of raw hits on one ladder
size_t StIstCollection::getNumRawHits( int ladder ) const
{
   return ((ladder >= 0 && ladder < kIstNumLadders) ? mRawHitCollection[ladder].getNumRawHits() : 0 );
};

//sum of all the clusters over all ladders
size_t StIstCollection::getNumClusters() const
{
   size_t n = 0;

   for ( const StIstClusterCollection *ptr = &mClusterCollection[0]; ptr != &mClusterCollection[kIstNumLadders]; ++ptr )
      n += ptr->getNumClusters();

   return n;
};

//number of clusters on one ladder
size_t StIstCollection::getNumClusters( int ladder ) const
{
   return ((ladder >= 0 && ladder < kIstNumLadders) ? mClusterCollection[ladder].getNumClusters() : 0 );
};


ClassImp(StIstCollection);
