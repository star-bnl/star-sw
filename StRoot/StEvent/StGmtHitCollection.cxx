/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHitCollection
 *
 ***************************************************************************
 *
 * Description: A collection of StGmtHit classes for StEvent.
 * Basically a wrapper for an StSPtrVecGmtHit.  Note, one instance of
 * this class corresponds to one module.
 *
 ***************************************************************************/

#include "StContainers.h"
#include "StGmtHit.h"
#include "StGmtHitCollection.h"

// deconstructor
StGmtHitCollection::~StGmtHitCollection() {
   // nothing to do
}

void StGmtHitCollection::Clear( Option_t *opt ){

   // no need to delete the objects in mStripVec, is done within its
   // clear function.

   // clear the vector
   mHitVec.clear();
}

ClassImp(StGmtHitCollection)
