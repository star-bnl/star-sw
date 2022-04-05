/***************************************************************************
 *
 * $Id: StFgtStripCollection.cxx,v 2.2 2013/11/13 19:18:34 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtStripCollection.cxx,v $
 * Revision 2.2  2013/11/13 19:18:34  ullrich
 * Commented unused variable.
 *
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#include "StContainers.h"
#include "StFgtStrip.h"
#include "StFgtStripCollection.h"
#include <cmath>
#include <iostream>

using namespace std;

// deconstructor
StFgtStripCollection::~StFgtStripCollection() {/* no op */}

// remove all hits with negative geoIds or with clusterSeedType set to
// kFgtDeadStrip
void StFgtStripCollection::removeFlagged(){
    if( !mStripVec.empty() ){
        // container to hold a copy
        std::vector< StFgtStrip* > copy;
        copy.reserve( mStripVec.size() );
        sortByGeoId();
        
        // iterators
        StSPtrVecFgtStripIterator srcIter;
        // StSPtrVecFgtStripIterator lastCopied=mStripVec.begin()-1;
        
        // copy all valid events
        for( srcIter = mStripVec.begin(); srcIter != mStripVec.end(); ++srcIter )
            if( (*srcIter) && (*srcIter)->getClusterSeedType() != kFgtDeadStrip && (*srcIter)->getGeoId() >= 0 )
                copy.push_back( new StFgtStrip( *(*srcIter) ) );
        
        if ( copy.size() != mStripVec.size() ){
            // this deletes the objects
            mStripVec.clear();
            // note: ownership of new objects passed to StSPtrVec
            std::vector< StFgtStrip* >::iterator copyIter;
            for( copyIter = copy.begin(); copyIter != copy.end(); ++copyIter )
                mStripVec.push_back( *copyIter );
        }
    }
}

bool StFgtStripCollection::hitGeoIdLessThan( const StFgtStrip* h1, const StFgtStrip* h2 ){
    return h1->getGeoId() < h2->getGeoId();
};

void StFgtStripCollection::Clear( Option_t *opt ){
    
    // no need to delete the objects in mStripVec, is done within its
    // clear function.
    
    // clear the vector
    mStripVec.clear();
    
    // clear the vector for alternate lookups
    for (unsigned int i=0; i<mStripElecIdVec.size(); i++) mStripElecIdVec[i] = static_cast< StFgtStrip* >(0);
}

StFgtStrip* StFgtStripCollection::getStrip( Int_t elecId ){
    StFgtStrip* &stripPtr = mStripElecIdVec[elecId]; 
    if( !stripPtr ){
        stripPtr = new StFgtStrip();
        mStripVec.push_back( stripPtr );
    }
    return stripPtr;
}

ClassImp(StFgtStripCollection);
