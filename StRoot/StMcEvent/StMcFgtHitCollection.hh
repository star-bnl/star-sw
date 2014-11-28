/***************************************************************************
 *
 * $Id: StMcFgtHitCollection.hh,v 2.4 2012/03/22 00:39:27 perev Exp $
 *
 * $Log: StMcFgtHitCollection.hh,v $
 * Revision 2.4  2012/03/22 00:39:27  perev
 * private => protected
 *
 * Revision 2.3  2009/10/13 19:14:27  perev
 * Wei-Ming update
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#ifndef StMcFgtHitCollection_hh
#define StMcFgtHitCollection_hh

#include "StMcFgtLayerHitCollection.hh"
class StMcFgtHit;

class StMcFgtHitCollection : public StObject {
public:
    
    StMcFgtHitCollection();
    ~StMcFgtHitCollection();
    
    bool addHit(StMcFgtHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcFgtLayerHitCollection*       layer(unsigned int);
    const StMcFgtLayerHitCollection* layer(unsigned int) const;
protected:
    enum { mNumberOfLayers = 9 }; // layer = disk in StFgtGeom WMZ
    StMcFgtLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcFgtHitCollection,1)
};
#endif
