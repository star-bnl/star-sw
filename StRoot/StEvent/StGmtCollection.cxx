/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtCollection
 *
 ***************************************************************************
 *
 * Description: GMT data collection for StEvent.
 *
 ***************************************************************************/

#include "StGmtCollection.h"

// constructor
StGmtCollection::StGmtCollection() : StObject() {
    // set the module field for some of the collections
    for( int i=0; i<kGmtNumModules; ++i ){
        mStripCollection[i].setModule( i );
        mHitCollection[i].setModule( i );
    }
}

// deconstructor
StGmtCollection::~StGmtCollection(){
    // nothing to do
}

void StGmtCollection::Clear( Option_t *opt ){
    for( int i=0; i<kGmtNumModules; ++i ){
        mStripCollection[i].Clear( opt );
        mHitCollection[i].Clear( opt );
    }
    mPointCollection.Clear( opt );
}

ClassImp(StGmtCollection)
