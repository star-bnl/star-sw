// $Id: StIstRawHitCollection.cxx,v 1.21 2018/01/04 17:34:38 smirnovd Exp $

#include "StIstRawHit.h"
#include "StIstRawHitCollection.h"
#include "St_base/StMessMgr.h"
#include <cmath>
#include <iostream>


StIstRawHitCollection::StIstRawHitCollection( int ladder ) : StObject(), mLadder(ladder), mRawHitVec(), mRawHitElecIdVec(kIstNumElecIds)
{
}


/** Free memory and clear the vector */
StIstRawHitCollection::~StIstRawHitCollection()
{
   while (!mRawHitVec.empty()) delete mRawHitVec.back(), mRawHitVec.pop_back();

   mRawHitElecIdVec.clear();
};

//sort internal vector by raw hit geometry ID
void StIstRawHitCollection::sortByGeoId()
{
   std::sort( mRawHitVec.begin(), mRawHitVec.end(), &StIstRawHitCollection::rawHitIdLessThan );
};

bool StIstRawHitCollection::rawHitIdLessThan( const StIstRawHit *h1, const StIstRawHit *h2 )
{
   return (h1->getGeoId() < h2->getGeoId());
};

vector<StIstRawHit *> &StIstRawHitCollection::getRawHitVec()
{
   return mRawHitVec;
};

const vector<StIstRawHit *> &StIstRawHitCollection::getRawHitVec() const
{
   return mRawHitVec;
};

size_t StIstRawHitCollection::getNumRawHits() const
{
   return mRawHitVec.size();
};

void StIstRawHitCollection::setLadder( int ladder )
{
   mLadder = ladder;
};

unsigned char StIstRawHitCollection::getLadder() const
{
   return mLadder;
};

void StIstRawHitCollection::Clear( Option_t *opt )
{
   while (!mRawHitVec.empty()) delete mRawHitVec.back(), mRawHitVec.pop_back();

   //clear the vector for alternate lookups
   for (unsigned int i = 0; i < mRawHitElecIdVec.size(); i++)
      mRawHitElecIdVec[i] = 0;
};


void StIstRawHitCollection::Print(int nTimeBins) const
{
   // The usage of nTimeBins is a bit crazy here but I took it directly from the
   // former debug output at the end of StIstClusterMaker::Make()
   int rawHitIdx = 0;

   for (std::vector<StIstRawHit*>::const_iterator it = mRawHitVec.begin(); it != mRawHitVec.end(); ++it, ++rawHitIdx)
   {
      LOG_DEBUG << "raw hit: Idx=" << rawHitIdx << endm;
      (*it)->Print(nTimeBins);
   }
}


/**
 * Adds or sets/overwrites a new StIstRawHit corresponding to electronic channel
 * StIstRawHit::mChannelId. If istRawHit is nullptr the function does nothing.
 * If a hit with channelId already exists it will be overwritten by the new one
 * and the resources will be freed. A silent check is performed to make sure the
 * channel electronic index is within the allowed range.
 */
void StIstRawHitCollection::addRawHit(StIstRawHit* istRawHit)
{
   if (!istRawHit) return;

   int elecId = istRawHit->getChannelId();

   if (elecId < 0 || elecId >= kIstNumElecIds) return;

   StIstRawHit *istRawHitCurrent = mRawHitElecIdVec[elecId];

   // In case channel elecId has been added previously: Remove the existing
   // hit/channel entry from memory and assign the new one to the same position
   // in the main container mRawHitVec
   if (istRawHitCurrent) {
      auto hitPtr = std::find(mRawHitVec.begin(), mRawHitVec.end(), istRawHitCurrent);
      *hitPtr = istRawHit;

      delete istRawHitCurrent;

   } else { // Otherwise just push back the new hit
      mRawHitVec.push_back(istRawHit);
   }

   mRawHitElecIdVec[elecId] = istRawHit;
}


StIstRawHit *StIstRawHitCollection::getRawHit( int elecId )
{
   StIstRawHit *&rawHitPtr = mRawHitElecIdVec[elecId];

   if ( !rawHitPtr ) {
      rawHitPtr = new StIstRawHit();
      mRawHitVec.push_back( rawHitPtr );
   }

   return rawHitPtr;
};


ClassImp(StIstRawHitCollection);
