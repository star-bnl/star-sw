/***************************************************************************
 *
 * $Id: StMcTpcPadrowHitCollection.hh,v 2.0 1999/11/17 02:01:00 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Padrow Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcPadrowHitCollection.hh,v $
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcTpcPadrowHitCollection_hh
#define StMcTpcPadrowHitCollection_hh

#include "StMcContainers.h"

#ifdef PERSISTENT
#include "StObject.h"
#endif

class StMcTpcHit;

class StMcTpcPadrowHitCollection
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StMcTpcPadrowHitCollection();
    ~StMcTpcPadrowHitCollection();
    // StMcTpcPadrowHitCollection(const StMcTpcPadrowHitCollection&); use default
    // const StMcTpcPadrowHitCollection&
    // operator=(const StMcTpcPadrowHitCollection&);                use default
    
    StSPtrVecMcTpcHit&       hits();
    const StSPtrVecMcTpcHit& hits() const;

private:
    StSPtrVecMcTpcHit mHits;
    
#ifdef PERSISTENT
    ClassDef(StMcTpcPadrowHitCollection,1)
#endif
};
#endif
