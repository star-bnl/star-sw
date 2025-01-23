/**
 * \class StGmtHitColection
 * \brief Holds collections of GMT hits
 * 
 * A collection of StGmtHit classes for StEvent.
 * Basically a wrapper for an StSPtrVecGmtHit.  Note, one instance of
 * this class corresponds to one module.
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

// StEvent headers
#include "StContainers.h"
#include "StGmtHit.h"
#include "StGmtHitCollection.h"

//________________
StGmtHitCollection::StGmtHitCollection( short moduleId ) : StObject(), mModule( moduleId ) {
  /* empty*/
}

//________________
StGmtHitCollection::~StGmtHitCollection() {
	/* empty */
}

//________________
void StGmtHitCollection::Clear( Option_t *opt ) {
  // no need to delete the objects in mStripVec, is done within 
	// its clear function.

  // clear the vector
  mHitVec.clear();
}


