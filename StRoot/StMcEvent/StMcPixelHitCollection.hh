/***************************************************************************
 *
 * $Id: StMcPixelHitCollection.hh,v 2.1 2003/08/20 18:50:21 calderon Exp $
 * $Log: StMcPixelHitCollection.hh,v $
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcPixelHitCollection_hh
#define StMcPixelHitCollection_hh

#include "StMcPixelLayerHitCollection.hh"
class StMcPixelHit;

class StMcPixelHitCollection {
public:

    StMcPixelHitCollection();
    ~StMcPixelHitCollection();
    
    bool addHit(StMcPixelHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcPixelLayerHitCollection*       layer(unsigned int);
    const StMcPixelLayerHitCollection* layer(unsigned int) const;
private:
    enum { mNumberOfLayers = 2 };
    StMcPixelLayerHitCollection mLayers[mNumberOfLayers];
};
#endif
