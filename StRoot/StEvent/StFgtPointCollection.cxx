/***************************************************************************
 *
 * $Id: StFgtPointCollection.cxx,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtPointCollection.cxx,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#include "StContainers.h"
#include "StFgtPoint.h"
#include "StFgtPointCollection.h"

// deconstructor
StFgtPointCollection::~StFgtPointCollection() {
   // nothing to do
}

inline void StFgtPointCollection::Clear( Option_t *opt ){

   // no need to delete the objects in mStripVec, is done within its
   // clear function.

   // clear the vector
   mPointVec.clear();
}

ClassImp(StFgtPointCollection);
