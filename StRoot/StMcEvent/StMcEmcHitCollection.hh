/***************************************************************************
 *
 * $Id: StMcEmcHitCollection.hh,v 2.2 2000/08/30 14:52:03 calderon Exp $
 * $Log: StMcEmcHitCollection.hh,v $
 * Revision 2.2  2000/08/30 14:52:03  calderon
 * New changes made by Aleksei.
 *
 *
 **************************************************************************/
#ifndef StMcEmcHitCollection_hh
#define StMcEmcHitCollection_hh

#include "StMcEmcModuleHitCollection.hh"

class StMcCalorimeterHit;

class StMcEmcHitCollection : public TDataSet {
public:
  enum  EAddHit {kNull, kErr, kNew, kAdd};
public:
    StMcEmcHitCollection();
    StMcEmcHitCollection(char*);
    StMcEmcHitCollection(const char*);
    virtual ~StMcEmcHitCollection();
    
    StMcEmcHitCollection::EAddHit  addHit(StMcCalorimeterHit*);
    unsigned long numberOfHits() const;
    unsigned int numberOfModules() const;
    float    sum() const;
    
    StMcEmcModuleHitCollection*       module(unsigned int);
    const StMcEmcModuleHitCollection* module(unsigned int) const;

    virtual Bool_t IsFolder() {return kTRUE;} // It is a directory for modules 
    virtual void Browse(TBrowser *b);
    void    print();

private:
    enum {mNumberOfModules=120};
    StMcEmcModuleHitCollection mModules[mNumberOfModules];
};

#endif
