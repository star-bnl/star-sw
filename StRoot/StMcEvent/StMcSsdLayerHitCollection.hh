/***************************************************************************
 *
 * $Id: StMcSsdLayerHitCollection.hh,v 2.2 2005/01/27 23:40:48 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Layer Hit Collection class from Kai
 *
 * The ssd detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcSsdLayerHitCollection.hh,v $
 * Revision 2.2  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Ssd classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#ifndef StMcSsdLayerHitCollection_hh
#define StMcSsdLayerHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcSsdHit;

class StMcSsdLayerHitCollection : public StObject
{
public:
    StMcSsdLayerHitCollection();
    virtual ~StMcSsdLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcSsdHit&       hits();
    const StSPtrVecMcSsdHit& hits() const; 

private:
    StSPtrVecMcSsdHit mHits;
    ClassDef(StMcSsdLayerHitCollection,1)
};
#endif
