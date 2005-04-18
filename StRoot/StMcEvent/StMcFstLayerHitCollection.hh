/***************************************************************************
 *
 * $Id: StMcFstLayerHitCollection.hh,v 2.1 2005/04/18 20:11:33 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Fst Layer Hit Collection class from Kai
 *
 * The pixel detector hits are stored here.
 *
 ***************************************************************************
 *
 * $Log: StMcFstLayerHitCollection.hh,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#ifndef StMcFstLayerHitCollection_hh
#define StMcFstLayerHitCollection_hh

#include "StMcContainers.hh"
#include "StObject.h"

class StMcFstHit;

class StMcFstLayerHitCollection : public StObject
{
public:
    StMcFstLayerHitCollection();
    ~StMcFstLayerHitCollection();
    
    unsigned long numberOfHits() const;

    StSPtrVecMcFstHit&       hits();
    const StSPtrVecMcFstHit& hits() const; 

private:
    StSPtrVecMcFstHit mHits;
    ClassDef(StMcFstLayerHitCollection,1)
};
#endif
