/***************************************************************************
 *
 * $Id: StMcSsdLayerHitCollection.hh,v 2.1 2004/09/14 05:00:30 calderon Exp $
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

class StMcSsdHit;

class StMcSsdLayerHitCollection
{
public:
    StMcSsdLayerHitCollection();
    ~StMcSsdLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcSsdHit&       hits();
    const StSPtrVecMcSsdHit& hits() const; 

private:
    StSPtrVecMcSsdHit mHits;
        
};
#endif
