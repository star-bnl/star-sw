/***************************************************************************
 *
 * $Id: StMcFgtHitCollection.hh,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * $Log: StMcFgtHitCollection.hh,v $
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
private:
    enum { mNumberOfLayers = 1 };
    StMcFgtLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcFgtHitCollection,1)
};
#endif
