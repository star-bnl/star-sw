/***************************************************************************
 *
 * $Id: StMcPixelLayerHitCollection.hh,v 2.1 2003/08/20 18:50:21 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Layer Hit Collection class from Kai
 *
 * The pixel detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcPixelLayerHitCollection.hh,v $
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#ifndef StMcPixelLayerHitCollection_hh
#define StMcPixelLayerHitCollection_hh

#include "StMcContainers.hh"

class StMcPixelHit;

class StMcPixelLayerHitCollection
{
public:
    StMcPixelLayerHitCollection();
    ~StMcPixelLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcPixelHit&       hits();
    const StSPtrVecMcPixelHit& hits() const; 

private:
    StSPtrVecMcPixelHit mHits;
        
};
#endif
