/***************************************************************************
 *
 * $Id: StMcSsdHitCollection.hh,v 2.2 2005/01/27 23:40:48 calderon Exp $
 * $Log: StMcSsdHitCollection.hh,v $
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
 **************************************************************************/
#ifndef StMcSsdHitCollection_hh
#define StMcSsdHitCollection_hh

#include "StMcSsdLayerHitCollection.hh"

class StMcSsdHit;

class StMcSsdHitCollection : public StObject {
public:

    StMcSsdHitCollection();
    virtual ~StMcSsdHitCollection();
    
    bool addHit(StMcSsdHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcSsdLayerHitCollection*       layer(unsigned int);
    const StMcSsdLayerHitCollection* layer(unsigned int) const;
private:
    enum { mNumberOfLayers = 1 };
    StMcSsdLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcSsdHitCollection,1)
};
#endif
