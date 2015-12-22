/***************************************************************************
* $Id: StIstRawHitCollection.cxx,v 1.17 2015/12/22 20:16:27 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************/

#include "StIstRawHit.h"
#include "StIstRawHitCollection.h"
#include "StRoot/St_base/StMessMgr.h"
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
 * Adds or sets a new StIstRawHit corresponding to electronic channel elecId. If
 * istRawHit is nullptr the function does nothing. If a hit with elecId already
 * exists the old one will be overwritten by the new one and the resources will
 * be freed. No check is performed to test the validity of elecId.
 */
void StIstRawHitCollection::addRawHit(int elecId, StIstRawHit* istRawHit)
{
   if (!istRawHit) return;

   StIstRawHit *istRawHitCurrent = mRawHitElecIdVec[elecId];

   if (istRawHitCurrent)
      delete istRawHitCurrent;

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


/***************************************************************************
* $Log: StIstRawHitCollection.cxx,v $
* Revision 1.17  2015/12/22 20:16:27  smirnovd
* StIstRawHitCollection: Added method to add/update hits in internal stl container
*
* Revision 1.16  2014/09/09 08:23:46  ypwang
* all unsgined char was updated to int type as Victor P. suggested
*
* Revision 1.15  2014/09/08 19:06:57  smirnovd
* Added Print() methods to print out properties of StIstCluster and StIstRawHit objects and their respective collections
*
* Revision 1.14  2014/03/27 22:46:47  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.13  2014/02/24 14:49:23  ypwang
* update Clear( Option_t *opt ) to delete StIstRawHit objects from mRawHitVec
*
* Revision 1.12  2014/02/24 14:24:40  ypwang
* get rid of StIstRawHitCollection::removeFlagged()
*
* Revision 1.11  2014/02/20 02:31:22  smirnovd
* Minor style corrections
*
* Revision 1.10  2014/02/20 02:31:00  smirnovd
* Use constructor list to initialize vectors of pointers and arrays
*
* Revision 1.9  2014/02/20 02:30:45  smirnovd
* Simplified the destructor
*
* Revision 1.8  2014/02/20 02:30:26  smirnovd
* Remove one level of indentation
*
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
