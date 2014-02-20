/***************************************************************************
* $Id: StIstRawHitCollection.cxx,v 1.7 2014/02/20 02:29:55 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************/

#include "StIstRawHit.h"
#include "StIstRawHitCollection.h"
#include <cmath>
#include <iostream>


//constructor
StIstRawHitCollection::StIstRawHitCollection( unsigned char ladder ) : StObject(), mLadder( ladder )
{
   mRawHitElecIdVec.resize( kIstNumElecIds );
   for (unsigned int i = 0; i < mRawHitElecIdVec.size(); i++)
      mRawHitElecIdVec[i] = static_cast< StIstRawHit * >(0);
};

//deconstructor
StIstRawHitCollection::~StIstRawHitCollection()
{
   //free memory and clear the vector
   std::vector< StIstRawHit * >::iterator vecIter;
   for ( vecIter = mRawHitVec.begin(); vecIter != mRawHitVec.end(); ++vecIter ) {
      if (*vecIter != NULL) {
         delete *vecIter;
         *vecIter = NULL;
      }
   }
   mRawHitVec.clear();

   //free memory and clear the vector
   std::vector< StIstRawHit * >::iterator vecIterT;
   for ( vecIterT = mRawHitElecIdVec.begin(); vecIterT != mRawHitElecIdVec.end(); ++vecIterT ) {
      delete[] (*vecIterT);
   }
   mRawHitElecIdVec.clear();

};

//sort internal vector by raw hit geometry ID
void StIstRawHitCollection::sortByGeoId()
{
   std::sort( mRawHitVec.begin(), mRawHitVec.end(), &StIstRawHitCollection::rawHitIdLessThan );
   return;
};

//remove all hits with negative geoIds
void StIstRawHitCollection::removeFlagged()
{
   if ( mRawHitVec.empty() ) return;

      // container to hold a copy
      std::vector< StIstRawHit * > copy;
      copy.reserve( mRawHitVec.size() );
      sortByGeoId();

      // copy all valid events
      std::vector< StIstRawHit * >::iterator srcIter;
      for ( srcIter = mRawHitVec.begin(); srcIter != mRawHitVec.end(); ++srcIter )
         if ( (*srcIter) && (*srcIter)->getChannelId() >= 0 )
            copy.push_back( new StIstRawHit( *(*srcIter) ) );

      if ( copy.size() != mRawHitVec.size() ) {
         // this deletes the objects
         mRawHitVec.clear();
         // note: ownership of new objects passed to StSPtrVec
         std::vector< StIstRawHit * >::iterator copyIter;
         for ( copyIter = copy.begin(); copyIter != copy.end(); ++copyIter )
            mRawHitVec.push_back( *copyIter );
      }
}

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

void StIstRawHitCollection::setLadder( unsigned char ladder )
{
   mLadder = ladder;
};

unsigned char StIstRawHitCollection::getLadder() const
{
   return mLadder;
};

void StIstRawHitCollection::Clear( Option_t *opt )
{
   //clear the vector 
   mRawHitVec.clear();

   //clear the vector for alternate lookups
   for (unsigned int i = 0; i < mRawHitElecIdVec.size(); i++)
      mRawHitElecIdVec[i] = static_cast< StIstRawHit * >(0);
};

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


/***************************************************************************
* $Log: StIstRawHitCollection.cxx,v $
* Revision 1.7  2014/02/20 02:29:55  smirnovd
* Reverse if statement to remove extra indentation
*
* Revision 1.6  2014/02/15 23:32:57  ypwang
* update destructor and Clear() function
*
* Revision 1.5  2014/02/14 14:51:06  ypwang
* update Clear() function, and call Clear() function in deconstructor
*
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
****************************************************************************
* StIstRawHitCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/
