/***************************************************************************
 *
 * $Id: StMcEmcModuleHitCollection.hh,v 2.1 2000/06/06 23:01:09 calderon Exp $
 *
 * Author: Aleksei Pavlinov, May 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Emc Module Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcEmcModuleHitCollection.hh,v $
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#ifndef StMcEmcModuleHitCollection_hh
#define StMcEmcModuleHitCollection_hh
#include "StMcContainers.hh"

class StMcCalorimeterHit;

class StMcEmcModuleHitCollection
{
public:
    StMcEmcModuleHitCollection();
    virtual ~StMcEmcModuleHitCollection();
    
    unsigned long numberOfHits() const;
    float sum() const;

    StSPtrVecMcCalorimeterHit&       hits();
    const StSPtrVecMcCalorimeterHit& hits() const;

private:
    StSPtrVecMcCalorimeterHit mHits;
    
};
#endif
