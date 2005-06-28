/***************************************************************************
 *
 * $Id: StMcEmcHitCollection.hh,v 2.6 2005/06/28 18:06:41 fine Exp $
 * $Log: StMcEmcHitCollection.hh,v $
 * Revision 2.6  2005/06/28 18:06:41  fine
 * Remove the redundant data-member StMcEmcModuleHitCollection mModules[mNumberOfModules] causing the crash duw double destruction of one and the same object
 *
 * Revision 2.5  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.4  2001/05/31 02:45:55  perev
 * const(ing)
 *
 * Revision 2.3  2001/05/13 21:12:10  calderon
 * Modifications by Aleksei to the Emc Hit Collections on indexing of
 * module numbers
 *
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
    
    StMcEmcModuleHitCollection*       module(unsigned int m);
    const StMcEmcModuleHitCollection* module(unsigned int m) const;

    StMcEmcModuleHitCollection&       Module(unsigned int m);
    const StMcEmcModuleHitCollection& Module(unsigned int m) const;
    
    virtual Bool_t IsFolder() const {return kTRUE;} // It is a directory for modules 
    virtual void Browse(TBrowser *b);
    void    print();
protected:
    void MakeHitCollection();
    StMcEmcModuleHitCollection&       thisModule(unsigned int m);
    const StMcEmcModuleHitCollection& thisModule(unsigned int m) const;
private:
    enum {mNumberOfModules=120};
    ClassDef(StMcEmcHitCollection,1)
};

inline       StMcEmcModuleHitCollection& StMcEmcHitCollection::thisModule(unsigned int m)
{
   TObjArray &modules = *GetObjArray();
   return *(StMcEmcModuleHitCollection *)modules[m];
}
inline const StMcEmcModuleHitCollection& StMcEmcHitCollection::thisModule(unsigned int m) const
{    
   TObjArray &modules = *GetObjArray();
   return *(StMcEmcModuleHitCollection *)modules[m];
}
#endif
