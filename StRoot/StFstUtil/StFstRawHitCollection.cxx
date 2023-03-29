#include "StFstRawHit.h"
#include "StFstRawHitCollection.h"
#include "St_base/StMessMgr.h"
#include <cmath>
#include <iostream>


StFstRawHitCollection::StFstRawHitCollection( int wedge ) : StObject(), mWedge(wedge), mRawHitVec(), mRawHitElecIdVec(kFstNumElecIds)
{
}


/** Free memory and clear the vector */
StFstRawHitCollection::~StFstRawHitCollection()
{
   while (!mRawHitVec.empty()) delete mRawHitVec.back(), mRawHitVec.pop_back();

   mRawHitElecIdVec.clear();
};

//sort internal vector by raw hit geometry ID
void StFstRawHitCollection::sortByGeoId()
{
   std::sort( mRawHitVec.begin(), mRawHitVec.end(), &StFstRawHitCollection::rawHitIdLessThan );
};

bool StFstRawHitCollection::rawHitIdLessThan( const StFstRawHit *h1, const StFstRawHit *h2 )
{
   return (h1->getGeoId() < h2->getGeoId());
};

vector<StFstRawHit *> &StFstRawHitCollection::getRawHitVec()
{
   return mRawHitVec;
};

const vector<StFstRawHit *> &StFstRawHitCollection::getRawHitVec() const
{
   return mRawHitVec;
};

size_t StFstRawHitCollection::getNumRawHits() const
{
   return mRawHitVec.size();
};

void StFstRawHitCollection::setWedge( int wedge )
{
   mWedge = wedge;
};

unsigned char StFstRawHitCollection::getWedge() const
{
   return mWedge;
};

void StFstRawHitCollection::Clear( Option_t *opt )
{
   while (!mRawHitVec.empty()) delete mRawHitVec.back(), mRawHitVec.pop_back();

   //clear the vector for alternate lookups
   for (unsigned int i = 0; i < mRawHitElecIdVec.size(); i++)
      mRawHitElecIdVec[i] = 0;
};


void StFstRawHitCollection::Print(int nTimeBins) const
{
   // The usage of nTimeBins is a bit crazy here but I took it directly from the
   // former debug output at the end of StFstClusterMaker::Make()
   int rawHitIdx = 0;

   for (std::vector<StFstRawHit*>::const_iterator it = mRawHitVec.begin(); it != mRawHitVec.end(); ++it, ++rawHitIdx)
   {
      LOG_DEBUG << "raw hit: Idx=" << rawHitIdx << endm;
      (*it)->Print(nTimeBins);
   }
}


/**
 * Adds or sets/overwrites a new StFstRawHit corresponding to electronic channel
 * StFstRawHit::mChannelId. If fstRawHit is nullptr the function does nothing.
 * If a hit with channelId already exists it will be overwritten by the new one
 * and the resources will be freed. A silent check is performed to make sure the
 * channel electronic index is within the allowed range.
 */
void StFstRawHitCollection::addRawHit(StFstRawHit* fstRawHit)
{
   if (!fstRawHit) return;

   int elecId = fstRawHit->getChannelId();

   if (elecId < 0 || elecId >= kFstNumElecIds) return;

   StFstRawHit *fstRawHitCurrent = mRawHitElecIdVec[elecId];

   // In case channel elecId has been added previously: Remove the existing
   // hit/channel entry from memory and assign the new one to the same position
   // in the main container mRawHitVec
   if (fstRawHitCurrent) {
      auto hitPtr = std::find(mRawHitVec.begin(), mRawHitVec.end(), fstRawHitCurrent);
      *hitPtr = fstRawHit;

      delete fstRawHitCurrent;

   } else { // Otherwise just push back the new hit
      mRawHitVec.push_back(fstRawHit);
   }

   mRawHitElecIdVec[elecId] = fstRawHit;
}


StFstRawHit *StFstRawHitCollection::getRawHit( int elecId )
{
   StFstRawHit *&rawHitPtr = mRawHitElecIdVec[elecId];

   if ( !rawHitPtr ) {
      rawHitPtr = new StFstRawHit();
      mRawHitVec.push_back( rawHitPtr );
   }

   return rawHitPtr;
};


ClassImp(StFstRawHitCollection);
