/***************************************************************************
 *
 * $Id: StMcFgtLayerHitCollection.hh,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Fgt Layer Hit Collection class from Kai
 *
 * The Fgt detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcFgtLayerHitCollection.hh,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#ifndef StMcFgtLayerHitCollection_hh
#define StMcFgtLayerHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcFgtHit;

class StMcFgtLayerHitCollection : public StObject
{
public:
    StMcFgtLayerHitCollection();
    ~StMcFgtLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcFgtHit&       hits();
    const StSPtrVecMcFgtHit& hits() const; 

private:
    StSPtrVecMcFgtHit mHits;
    ClassDef(StMcFgtLayerHitCollection,1)
};
#endif
