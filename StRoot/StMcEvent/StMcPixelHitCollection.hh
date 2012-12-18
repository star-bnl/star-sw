/***************************************************************************
 *
 * $Id: StMcPixelHitCollection.hh,v 2.5 2012/12/18 21:02:26 perev Exp $
 * $Log: StMcPixelHitCollection.hh,v $
 * Revision 2.5  2012/12/18 21:02:26  perev
 * Pixel development (Jonathan)
 *
 * Revision 2.4  2012/03/22 00:45:54  perev
 * private => protected
 *
 * Revision 2.3  2009/07/24 19:08:07  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.2  2005/05/11 20:54:29  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
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

class StMcPixelHitCollection : public StObject {
public:

    StMcPixelHitCollection();
    virtual ~StMcPixelHitCollection();
    
    bool addHit(StMcPixelHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcPixelLayerHitCollection*       layer(unsigned int);
    const StMcPixelLayerHitCollection* layer(unsigned int) const;
protected:
    enum { mNumberOfLayers = 10 };
    StMcPixelLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcPixelHitCollection,1)
};
#endif
