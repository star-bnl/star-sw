/***************************************************************************
 *
 * $Id: StFgtHitCollection.cxx,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtHitCollection.cxx,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#include "StContainers.h"
#include "StFgtHit.h"
#include "StFgtHitCollection.h"

// deconstructor
StFgtHitCollection::~StFgtHitCollection() {
   // nothing to do
}

void StFgtHitCollection::Clear( Option_t *opt ){

   // no need to delete the objects in mStripVec, is done within its
   // clear function.

   // clear the vector
   mHitVec.clear();
}

ClassImp(StFgtHitCollection);
