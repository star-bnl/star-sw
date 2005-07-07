/*!
 * \class StMcIgtLayerHitCollection
 * \brief Monte Carlo Igt Layer Hit Collection class
 * Used to access the hits of an Igt layer.
 * \author Gerrit van Nieuwenhuizen, Manuel Calderon de la Barca Sanchez
 * \date   July 2005
 *
 ***************************************************************************
 *
 * $Id: StMcIgtLayerHitCollection.hh,v 2.1 2005/07/07 18:20:49 calderon Exp $
 *
 ***************************************************************************
 *
 * $Log: StMcIgtLayerHitCollection.hh,v $
 * Revision 2.1  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 *
 *
 **************************************************************************/
#ifndef StMcIgtLayerHitCollection_hh
#define StMcIgtLayerHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcIgtHit;

class StMcIgtLayerHitCollection : public StObject {
public:
    StMcIgtLayerHitCollection();
    ~StMcIgtLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcIgtHit&       hits();
    const StSPtrVecMcIgtHit& hits() const; 

private:
    StSPtrVecMcIgtHit mHits;
    ClassDef(StMcIgtLayerHitCollection,1)
};
#endif
