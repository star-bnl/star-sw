/***************************************************************************
 *
 * $Id: StMcIstHitCollection.hh,v 2.1 2004/09/14 05:00:29 calderon Exp $
 * $Log: StMcIstHitCollection.hh,v $
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcIstHitCollection_hh
#define StMcIstHitCollection_hh

#include "StMcIstLayerHitCollection.hh"
class StMcIstHit;

class StMcIstHitCollection {
public:

    StMcIstHitCollection();
    ~StMcIstHitCollection();
    
    bool addHit(StMcIstHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcIstLayerHitCollection*       layer(unsigned int);
    const StMcIstLayerHitCollection* layer(unsigned int) const;
private:
    enum { mNumberOfLayers = 3 };
    StMcIstLayerHitCollection mLayers[mNumberOfLayers];
};
#endif
