/***************************************************************************
 *
 * $Id: StMcEmcHitCollection.hh,v 2.11 2018/03/15 22:00:35 smirnovd Exp $
 * $Log: StMcEmcHitCollection.hh,v $
 * Revision 2.11  2018/03/15 22:00:35  smirnovd
 * Fix linker error by removing declared but undefined functions
 *
 * Revision 2.10  2012/06/11 14:51:46  fisyak
 * std namespace
 *
 * Revision 2.9  2012/03/22 00:33:58  perev
 * private => protected
 *
 * Revision 2.8  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.7  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
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
  enum {mNumberOfModules=120};
  StMcEmcHitCollection();
  StMcEmcHitCollection(char*);
  StMcEmcHitCollection(const char*);
  virtual ~StMcEmcHitCollection();
  
  StMcEmcHitCollection::EAddHit  addHit(StMcCalorimeterHit*);
  unsigned long numberOfHits() const;
  unsigned int numberOfModules() const { return mNumberOfModules; }
  float    sum() const;
  
  StMcEmcModuleHitCollection*       module(unsigned int m);
  const StMcEmcModuleHitCollection* module(unsigned int m) const;
  
  StMcEmcModuleHitCollection&       thisModule(unsigned int m) {
    TObjArray &modules = *GetObjArray();
    return *(StMcEmcModuleHitCollection *)modules[m];
  }
  const StMcEmcModuleHitCollection& thisModule(unsigned int m) const {
    TObjArray &modules = *GetObjArray();
    return *(const StMcEmcModuleHitCollection *)modules[m];
  }
  
  virtual Bool_t IsFolder() const {return kTRUE;} // It is a directory for modules 
  virtual void Browse(TBrowser *b);
  void    print();
  virtual void Print(Option_t *option="") const; // *MENU* 
  
 protected:
  void MakeHitCollection();
 protected:
  ClassDef(StMcEmcHitCollection,1)
};

std::ostream&  operator<<(std::ostream& os, const StMcEmcHitCollection&);
#endif
