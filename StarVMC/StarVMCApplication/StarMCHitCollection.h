// $Id: StarMCHitCollection.h,v 1.1 2007/01/05 21:36:22 potekhin Exp $
// $Log: StarMCHitCollection.h,v $
// Revision 1.1  2007/01/05 21:36:22  potekhin
// Add a stub for the hit collection
//
#ifndef StarMCHitCollection_h
#define StarMCHitCollection_h

#include "TLorentzVector.h"
#include "StarMCHit.h"

class StarMCHitCollection : public TObjArray {
 public:
  StarMCHitCollection() {};
  virtual ~StarMCHitCollection() {};

  Int_t Count(void) {return GetEntriesFast();}

 private:

  ClassDef(StarMCHitCollection,1)

};
#endif
