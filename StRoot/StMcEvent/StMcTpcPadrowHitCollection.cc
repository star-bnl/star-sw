/***************************************************************************
 *
 * $Id: StMcTpcPadrowHitCollection.cc,v 2.1 2000/05/11 14:27:23 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Padrow Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcPadrowHitCollection.cc,v $
 * Revision 2.1  2000/05/11 14:27:23  calderon
 * use clear() in destructors to reduce size of containers
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcTpcPadrowHitCollection.hh"
#include "StMcTpcHit.hh"

static const char rcsid[] = "$Id: StMcTpcPadrowHitCollection.cc,v 2.1 2000/05/11 14:27:23 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcTpcPadrowHitCollection)
#endif

StMcTpcPadrowHitCollection::StMcTpcPadrowHitCollection() { /* noop */ }

StMcTpcPadrowHitCollection::~StMcTpcPadrowHitCollection()
{
    // mHits
    // is a polymorphic container and StMcTpcHit  
    // provides its own new/delete operator.
    //
    for (unsigned int i=0; i<mHits.size(); i++) {
        delete mHits[i];
        mHits[i] = 0;
    }
    mHits.clear();
}

const StSPtrVecMcTpcHit&
StMcTpcPadrowHitCollection::hits() const { return mHits; }

StSPtrVecMcTpcHit&
StMcTpcPadrowHitCollection::hits() { return mHits; }
