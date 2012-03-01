/***************************************************************************
 *
 * $Id: StMcPixelLayerHitCollection.hh,v 2.4 2012/03/01 16:48:29 perev Exp $
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
 * Revision 2.4  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.3  2009/07/24 19:08:08  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
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
#include "StObject.h"

class StMcPixelHit;

class StMcPixelLayerHitCollection : public StObject
{
public:
    StMcPixelLayerHitCollection();
    virtual ~StMcPixelLayerHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    unsigned long numberOfHits() const;

    StSPtrVecMcPixelHit&       hits();
    const StSPtrVecMcPixelHit& hits() const; 

private:
    StSPtrVecMcPixelHit mHits;
    ClassDef(StMcPixelLayerHitCollection,1)
};
#endif
