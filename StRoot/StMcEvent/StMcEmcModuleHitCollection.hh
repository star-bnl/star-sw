/***************************************************************************
 *
 * $Id: StMcEmcModuleHitCollection.hh,v 2.2 2000/08/30 14:52:03 calderon Exp $
 *
 * Author: Aleksei Pavlinov, May 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Emc Module Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcEmcModuleHitCollection.hh,v $
 * Revision 2.2  2000/08/30 14:52:03  calderon
 * New changes made by Aleksei.
 *
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#ifndef StMcEmcModuleHitCollection_hh
#define StMcEmcModuleHitCollection_hh
#include "StMcContainers.hh"
#include "TDataSet.h"

class StMcCalorimeterHit;

class StMcEmcModuleHitCollection : public TDataSet {
public:
    StMcEmcModuleHitCollection();
    StMcEmcModuleHitCollection(const unsigned int m);
    virtual ~StMcEmcModuleHitCollection();
    void init(const unsigned int m);
    
    unsigned long numberOfHits() const;
    float sum() const;

    StSPtrVecMcCalorimeterHit&       hits();
    const StSPtrVecMcCalorimeterHit& hits() const;

    virtual Bool_t IsFolder() {return kFALSE;}
    virtual void   Browse(TBrowser *b);

    void operator()(const unsigned int m) { init(m); } 

private:
    StSPtrVecMcCalorimeterHit mHits; 
};
#endif
