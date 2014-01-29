/***************************************************************************
*
* $Id: StIstRawHitCollection.cxx,v 1.2 2014/01/29 18:25:03 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file. 
****************************************************************************
*
* $Log: StIstRawHitCollection.cxx,v $
* Revision 1.2  2014/01/29 18:25:03  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHitCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

#include "StIstRawHit.h"
#include "StIstRawHitCollection.h"
#include <cmath>
#include <iostream>

//constructor
StIstRawHitCollection::~StIstRawHitCollection() {/* no op */}

// remove all hits with negative geoIds
void StIstRawHitCollection::removeFlagged(){
    if( !mRawHitVec.empty() ){
        // container to hold a copy
        std::vector< StIstRawHit* > copy;
        copy.reserve( mRawHitVec.size() );
        sortByGeoId();
       
        // copy all valid events
	std::vector< StIstRawHit* >::iterator srcIter;
        for( srcIter = mRawHitVec.begin(); srcIter != mRawHitVec.end(); ++srcIter )
            if( (*srcIter) && (*srcIter)->getChannelId() >= 0 )
                copy.push_back( new StIstRawHit( *(*srcIter) ) );
        
        if ( copy.size() != mRawHitVec.size() ){
            // this deletes the objects
            mRawHitVec.clear();
            // note: ownership of new objects passed to StSPtrVec
            std::vector< StIstRawHit* >::iterator copyIter;
            for( copyIter = copy.begin(); copyIter != copy.end(); ++copyIter )
                mRawHitVec.push_back( *copyIter );
        }
    }
}

bool StIstRawHitCollection::rawHitIdLessThan( const StIstRawHit* h1, const StIstRawHit* h2 ){
    return (h1->getGeoId() < h2->getGeoId());
};

void StIstRawHitCollection::Clear( Option_t *opt ){
    // clear the vector
    mRawHitVec.clear();
    // clear the vector for alternate lookups
    for (unsigned int i=0; i<mRawHitElecIdVec.size(); i++)
        mRawHitElecIdVec[i] = static_cast< StIstRawHit* >(0);
}

StIstRawHit* StIstRawHitCollection::getRawHit( int elecId ){
    StIstRawHit* &rawHitPtr = mRawHitElecIdVec[elecId];
    if( !rawHitPtr ){
        rawHitPtr = new StIstRawHit();
        mRawHitVec.push_back( rawHitPtr );
        }
    return rawHitPtr;
}

ClassImp(StIstRawHitCollection);
