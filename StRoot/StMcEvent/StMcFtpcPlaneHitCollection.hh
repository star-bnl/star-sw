/***************************************************************************
 *
 * $Id: StMcFtpcPlaneHitCollection.hh,v 2.0 1999/11/17 02:00:59 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ftpc Plane Hit Collection class
 *
 * The hits are stored here, and there is no Sector Hit collection, because
 * the sector assignment depends on the reconstruction, B-Field, etc.  The
 * Monte Carlo Data doesn't shouldn't assign the sector.
 *
 ***************************************************************************
 *
 * $Log: StMcFtpcPlaneHitCollection.hh,v $
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcFtpcPlaneHitCollection_hh
#define StMcFtpcPlaneHitCollection_hh

#include "StMcContainers.hh"

#ifdef PERSISTENT
#include "StObject.h"
#endif

class StMcFtpcHit;

class StMcFtpcPlaneHitCollection
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StMcFtpcPlaneHitCollection();
    ~StMcFtpcPlaneHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcFtpcHit&       hits();
    const StSPtrVecMcFtpcHit& hits() const; 

private:
    StSPtrVecMcFtpcHit mHits;
        
#ifdef PERSISTENT
    ClassDef(StMcFtpcPlaneHitCollection,1)
#endif
};
#endif
