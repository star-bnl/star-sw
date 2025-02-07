/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtStripCollection
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************/

#include "St_base/StMessMgr.h"

#include "StContainers.h"
#include "StGmtStrip.h"
#include "StGmtStripCollection.h"

#include <cmath>
#include <iostream>
#include <vector>

//________________
StGmtStripCollection::~StGmtStripCollection() {/* no op */}

//________________
StGmtStripCollection::StGmtStripCollection( short module ) : StObject(), mModule( module ) {
    mStripGeoIdVec.resize( kGmtNumGeoIds );
    for (unsigned int i=0; i<mStripGeoIdVec.size(); i++) {
        mStripGeoIdVec[i] = static_cast< StGmtStrip* >(0);
    }
};

//________________
void StGmtStripCollection::removeFlagged(){
    // remove all hits with negative geoIds or with clusterSeedType set to kGmtDeadStrip
    if( !mStripVec.empty() ){
        // container to hold a copy
        std::vector< StGmtStrip* > copy;
        copy.reserve( mStripVec.size() );
        sortByGeoId();
        
        // iterators
        StSPtrVecGmtStripIterator srcIter;
        StSPtrVecGmtStripIterator lastCopied=mStripVec.begin()-1;
        
        // copy all valid events
        for( srcIter = mStripVec.begin(); srcIter != mStripVec.end(); ++srcIter )
            if( (*srcIter) && (*srcIter)->getGeoId() >= 0 ) {
                copy.push_back( new StGmtStrip( *(*srcIter) ) );
            }
        
        if ( copy.size() != mStripVec.size() ){
            // this deletes the objects
            mStripVec.clear();
            // note: ownership of new objects passed to StSPtrVec
            std::vector< StGmtStrip* >::iterator copyIter;
            for( copyIter = copy.begin(); copyIter != copy.end(); ++copyIter ) {
                mStripVec.push_back( *copyIter );
            }
        }
    }
}

//________________
bool StGmtStripCollection::hitGeoIdLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ){
    return h1->getGeoId() < h2->getGeoId();
};

//________________
bool StGmtStripCollection::hitCoordLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ){
    return h1->getCoordNum() < h2->getCoordNum();
};

//________________
bool StGmtStripCollection::hitLayerLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ){
    return h1->isY() < h2->isY();
};

//________________
void StGmtStripCollection::Clear( Option_t *opt ){
    
    // no need to delete the objects in mStripVec, is done within its
    // clear function.
    
    // clear the vector
    mStripVec.clear();
    
    // clear the vector for alternate lookups
    for (unsigned int i=0; i<mStripElecIdVec.size(); i++) mStripElecIdVec[i] = static_cast< StGmtStrip* >(0);

    // clear the other vector for alternate lookups
    for (unsigned int i=0; i<mStripGeoIdVec.size(); i++) mStripGeoIdVec[i] = static_cast< StGmtStrip* >(0);

}

//________________
StGmtStrip* StGmtStripCollection::getStrip( Int_t Id ) {  
    // using geoId now instead of elecId so now using more generic index name
    StGmtStrip* stripPtr = mStripGeoIdVec[Id]; 
    if( !stripPtr ){
        stripPtr = new StGmtStrip();
        mStripVec.push_back( stripPtr );
    }
    return stripPtr;
}

//________________
StGmtStrip* StGmtStripCollection::getSortedStrip( Int_t Id ) {  
    // using geoId now instead of elecId so now using more generic index name
    StGmtStrip* stripPtr = mStripVec[Id]; 
    if( !stripPtr ){
      LOG_ERROR << "StGmtStripCollection::getSortedStrip no such Id: " << Id << endm;
      return 0;
    }
    return stripPtr;
}

// sort by geoId
void StGmtStripCollection::sortByGeoId(){
    std::sort( mStripVec.begin(), mStripVec.end(), &StGmtStripCollection::hitGeoIdLessThan );
}

// sort by layer (X first then Y)
void StGmtStripCollection::sortByLayer(){
    std::sort( mStripVec.begin(), mStripVec.end(), &StGmtStripCollection::hitLayerLessThan );
};

// sort by coordinate number
void StGmtStripCollection::partialSortByCoord(){
    std::partial_sort( mStripVec.begin(), mStripVec.begin()+kGmtNumStrips, mStripVec.begin()+kGmtNumStrips, &StGmtStripCollection::hitCoordLessThan );
};

// sort by coordinate number
void StGmtStripCollection::sortByCoord(){
    std::sort( mStripVec.begin(), mStripVec.end(), &StGmtStripCollection::hitCoordLessThan );
};

