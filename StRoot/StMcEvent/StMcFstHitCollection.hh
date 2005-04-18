/***************************************************************************
 *
 * $Id: StMcFstHitCollection.hh,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * $Log: StMcFstHitCollection.hh,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#ifndef StMcFstHitCollection_hh
#define StMcFstHitCollection_hh

#include "StMcFstLayerHitCollection.hh"
class StMcFstHit;

class StMcFstHitCollection : public StObject {
public:

    StMcFstHitCollection();
    ~StMcFstHitCollection();
    
    bool addHit(StMcFstHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcFstLayerHitCollection*       layer(unsigned int);
    const StMcFstLayerHitCollection* layer(unsigned int) const;
private:
    enum { mNumberOfLayers = 3 };
    StMcFstLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcFstHitCollection,1)
};
#endif
