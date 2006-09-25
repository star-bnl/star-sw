/***************************************************************************
 * First version of StMcHpdHitCollection.hh
 **************************************************************************/
#ifndef StMcHpdHitCollection_hh
#define StMcHpdHitCollection_hh

#include "StMcHpdLayerHitCollection.hh"
class StMcHpdHit;

class StMcHpdHitCollection : public StObject {
public:

    StMcHpdHitCollection();
    ~StMcHpdHitCollection();
    
    bool addHit(StMcHpdHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcHpdLayerHitCollection*       layer(unsigned int);
    const StMcHpdLayerHitCollection* layer(unsigned int) const;
private:
    enum { mNumberOfLayers = 3 };
    StMcHpdLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcHpdHitCollection,1)
};
#endif
