/*!
 * \class  StMcIgtHitCollection
 * \brief  Monte Carlo Hit Collection class for the GEM Tracker.
 * \author Gerrit van Nieuwenhuizen, Manuel Calderon de la Barca Sanchez
 * \date   July 2005
 *
 ***************************************************************************
 *
 * $Id: StMcIgtHitCollection.hh,v 2.1 2005/07/07 18:20:49 calderon Exp $
 *
 ***************************************************************************
 *
 * $Log: StMcIgtHitCollection.hh,v $
 * Revision 2.1  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 *
 **************************************************************************/
#ifndef StMcIgtHitCollection_hh
#define StMcIgtHitCollection_hh

#include "StMcIgtLayerHitCollection.hh"
class StMcIgtHit;

class StMcIgtHitCollection : public StObject {
public:

    StMcIgtHitCollection();
    ~StMcIgtHitCollection();
    
    bool addHit(StMcIgtHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcIgtLayerHitCollection*       layer(unsigned int);
    const StMcIgtLayerHitCollection* layer(unsigned int) const;
private:
    enum { mNumberOfLayers = 2 };
    StMcIgtLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcIgtHitCollection,1)
};
#endif
