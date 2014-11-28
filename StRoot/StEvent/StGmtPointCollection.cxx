/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtPointCollection
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************/

#include "StContainers.h"
#include "StGmtPoint.h"
#include "StGmtPointCollection.h"

// deconstructor
StGmtPointCollection::~StGmtPointCollection() {
   // nothing to do
}

inline void StGmtPointCollection::Clear( Option_t *opt ){

   // no need to delete the objects in mStripVec, is done within its
   // clear function.

   // clear the vector
   mPointVec.clear();
}

ClassImp(StGmtPointCollection)
