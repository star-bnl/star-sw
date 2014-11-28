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

using namespace std;

// deconstructor
StGmtStripCollection::~StGmtStripCollection() {/* no op */}

// remove all hits with negative geoIds or with clusterSeedType set to
// kGmtDeadStrip
void StGmtStripCollection::removeFlagged(){
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
//             if( (*srcIter) && (*srcIter)->getClusterSeedType() != kGmtDeadStrip && (*srcIter)->getGeoId() >= 0 )
            if( (*srcIter) && (*srcIter)->getGeoId() >= 0 )
                copy.push_back( new StGmtStrip( *(*srcIter) ) );
        
        if ( copy.size() != mStripVec.size() ){
            // this deletes the objects
            mStripVec.clear();
            // note: ownership of new objects passed to StSPtrVec
            std::vector< StGmtStrip* >::iterator copyIter;
            for( copyIter = copy.begin(); copyIter != copy.end(); ++copyIter )
                mStripVec.push_back( *copyIter );
        }
    }
}

bool StGmtStripCollection::hitGeoIdLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ){
    return h1->getGeoId() < h2->getGeoId();
};

bool StGmtStripCollection::hitCoordLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ){
    return h1->getCoordNum() < h2->getCoordNum();
//     if( (h1->isY() && h2->isY()) || ( !(h1->isY()) && !(h2->isY()) ) )
//     {
// //       LOG_ERROR << "StGmtStripCollection::hitCoordLessThan sort CASE 1: " << 
// //       "\n\t h1 " << h1 << 
// //       "\n\t h1->isY() " << h1->isY() << 
// //       "\n\t h1->GetCoordNum() " << h1->getCoordNum() << 
// //       "\n\t h2 " << h2 << 
// //       "\n\t h2->isY() " << h2->isY() << 
// //       "\n\t h2->GetCoordNum() " << h2->getCoordNum() << 
// //       "\n\t will return: " << h1->getCoordNum() << " < " << h2->getCoordNum() << endm;
//       return h1->getCoordNum() < h2->getCoordNum();
//     }
//     else if( h1->isY() && !(h2->isY()) )
//     {
// //       LOG_ERROR << "StGmtStripCollection::hitCoordLessThan sort CASE 2: " << 
// //       "\n\t h1 " << h1 << 
// //       "\n\t h1->isY() " << h1->isY() << 
// //       "\n\t h1->GetCoordNum() " << h1->getCoordNum() << 
// //       "\n\t h2 " << h2 << 
// //       "\n\t h2->isY() " << h2->isY() << 
// //       "\n\t h2->GetCoordNum() " << h2->getCoordNum() << 
// //       "\n\t will return: " << h1->getCoordNum()+kGmtNumStrips << " < " << h2->getCoordNum() << endm;
//       return (h1->getCoordNum()+kGmtNumStrips) < h2->getCoordNum(); // order X first 
//     }
//     else if( !(h1->isY()) && h2->isY() )
//     {
// //       LOG_ERROR << "StGmtStripCollection::hitCoordLessThan sort CASE 3: " << 
// //       "\n\t h1 " << h1 << 
// //       "\n\t h1->isY() " << h1->isY() << 
// //       "\n\t h1->GetCoordNum() " << h1->getCoordNum() << 
// //       "\n\t h2 " << h2 << 
// //       "\n\t h2->isY() " << h2->isY() << 
// //       "\n\t h2->GetCoordNum() " << h2->getCoordNum() << 
// //       "\n\t will return: " << h1->getCoordNum() << " < " <<  h2->getCoordNum()+kGmtNumStrips << endm;
//       return h1->getCoordNum() < (h2->getCoordNum()+kGmtNumStrips); // order X first 
//     }
//     else
//     {
// //       LOG_ERROR << "StGmtStripCollection::hitCoordLessThan sort FAILED: " << 
// //       "\n\t h1 " << h1 << 
// //       "\n\t h1->isY() " << h1->isY() << 
// //       "\n\t h1->GetCoordNum() " << h1->getCoordNum() << 
// //       "\n\t h2 " << h2 << 
// //       "\n\t h2->isY() " << h2->isY() << 
// //       "\n\t h2->GetCoordNum() " << h2->getCoordNum() << 
// //       "\n\t will return: " << -1 << endm;
//       return -1;
//     }
};

bool StGmtStripCollection::hitLayerLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ){
    return h1->isY() < h2->isY();
};

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

StGmtStrip* StGmtStripCollection::getStrip( Int_t Id ){  // using geoId now instead of elecId so now using more generic index name
//     StGmtStrip* &stripPtr = mStripElecIdVec[Id]; 
    StGmtStrip* &stripPtr = mStripGeoIdVec[Id]; 
    if( !stripPtr ){
        stripPtr = new StGmtStrip();
        mStripVec.push_back( stripPtr );
    }
    return stripPtr;
}

// StGmtStrip* StGmtStripCollection::getStrip( Int_t Id ){  // using geoId now instead of elecId so now using more generic index name
//     StGmtStrip* &stripPtr = mStripVec[Id]; 
//     if( !stripPtr ){
//         stripPtr = new StGmtStrip();
//         mStripVec.push_back( stripPtr );
//     }
//     return stripPtr;
// }

StGmtStrip* StGmtStripCollection::getSortedStrip( Int_t Id ){  // using geoId now instead of elecId so now using more generic index name
//     StGmtStrip* &stripPtr = mStripElecIdVec[Id]; 
    StGmtStrip* &stripPtr = mStripVec[Id]; 
    if( !stripPtr ){
      LOG_ERROR << "StGmtStripCollection::getSortedStrip no such Id: " << Id << endm;
      return 0;
    }
    return stripPtr;
}

ClassImp(StGmtStripCollection)
