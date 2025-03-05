/**
 * \class StGmtColection
 * \brief Holds collections of GMT data
 * 
 * GMT data collection for StEvent (based on StFgtCollection)
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

// Load header of the GMT collection
#include "StGmtCollection.h"

//________________
StGmtCollection::StGmtCollection() : StObject() {
  // Set the module field for some of the collections
  for( int i=0; i<kGmtNumModules; ++i ) {
    mStripCollection[i].setModule( i );
    mHitCollection[i].setModule( i );
  }
}

//________________
StGmtCollection::~StGmtCollection() {
  /* empty */
}

//__________
void StGmtCollection::Clear( Option_t *opt ) {
  for( int i=0; i<kGmtNumModules; ++i ){
    mStripCollection[i].Clear( opt );
    mHitCollection[i].Clear( opt );
  }
  mPointCollection.Clear( opt );
}

//________________
size_t StGmtCollection::getNumStrips() const { 
  size_t n = 0;
  for( const StGmtStripCollection* ptr = &mStripCollection[0]; 
       ptr != &mStripCollection[kGmtNumModules]; ++ptr ) {
    n += ptr->getNumStrips();
  }
  return n;
}

//________________
size_t StGmtCollection::getNumHits() const {
  size_t n = 0;
  for( const StGmtHitCollection* ptr = &mHitCollection[0]; 
       ptr != &mHitCollection[kGmtNumModules]; ++ptr ) {
    n += ptr->getNumHits();
  }
  return n;
}
