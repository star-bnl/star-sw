/***************************************************************************
 *
 * $Id $
 * $Log $
 *
 **************************************************************************/
#ifndef StMcEmcHitCollection_hh
#define StMcEmcHitCollection_hh

#include "StMcEmcModuleHitCollection.hh"
#include "TDataSet.h"

class StMcCalorimeterHit;

class StMcEmcHitCollection : public TDataSet {
public:
  enum  EAddHit {kNull, kErr, kNew, kAdd};
public:
    StMcEmcHitCollection();
    StMcEmcHitCollection(char*);
    virtual ~StMcEmcHitCollection();
    
    StMcEmcHitCollection::EAddHit  addHit(StMcCalorimeterHit*);
    unsigned long numberOfHits() const;
    unsigned int numberOfModules() const;
    float    sum() const;
    
    StMcEmcModuleHitCollection*       module(unsigned int);
    const StMcEmcModuleHitCollection* module(unsigned int) const;

    virtual Bool_t IsFolder() {return kFALSE;} // KTRUE means it is directory
    void Browse(TBrowser *b);
private:
    enum {mNumberOfModules=120};
    StMcEmcModuleHitCollection mModules[mNumberOfModules];
    
};

#endif
