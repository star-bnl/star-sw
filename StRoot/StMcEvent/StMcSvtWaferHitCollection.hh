/***************************************************************************
 *
 * $Id: StMcSvtWaferHitCollection.hh,v 2.1 1999/11/19 19:06:33 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Wafer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtWaferHitCollection.hh,v $
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcSvtWaferHitCollection_hh
#define StMcSvtWaferHitCollection_hh
#include "StMcContainers.hh"

#ifdef PERSISTENT
#include "StObject.h"
#endif

class StMcSvtHit;

class StMcSvtWaferHitCollection
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StMcSvtWaferHitCollection();
    // StMcSvtWaferHitCollection(const StMcSvtWaferHitCollection&); use default
    // const StMcSvtWaferHitCollection& operator=(const StMcSvtWaferHitCollection&); use default
    ~StMcSvtWaferHitCollection();
    
    StSPtrVecMcSvtHit&       hits();
    const StSPtrVecMcSvtHit& hits() const;

private:
    StSPtrVecMcSvtHit mHits;
    
#ifdef PERSISTENT
    ClassDef(StMcSvtWaferHitCollection,1)
#endif
};
#endif
