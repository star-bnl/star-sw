/***************************************************************************
 *
 * $Id: StMcIstLayerHitCollection.hh,v 2.1 2004/09/14 05:00:30 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Layer Hit Collection class from Kai
 *
 * The pixel detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcIstLayerHitCollection.hh,v $
 * Revision 2.1  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Ist classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#ifndef StMcIstLayerHitCollection_hh
#define StMcIstLayerHitCollection_hh

#include "StMcContainers.hh"

class StMcIstHit;

class StMcIstLayerHitCollection
{
public:
    StMcIstLayerHitCollection();
    ~StMcIstLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcIstHit&       hits();
    const StSPtrVecMcIstHit& hits() const; 

private:
    StSPtrVecMcIstHit mHits;
        
};
#endif
