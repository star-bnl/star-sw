/***************************************************************************
 *
 * $Id: StEmcTowerHit.h,v 1.2 1999/04/28 22:27:31 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTowerHit.h,v $
 * Revision 1.2  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/02/23 15:45:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcTowerHit_hh
#define StEmcTowerHit_hh

#include "StArray.h"
#include "StEmcHit.h"
    StEmcTowerHit(Int_t, Float_t, Float_t, Float_t);
class StEmcTowerHit : public StEmcHit {
public:
  StEmcTowerHit(Int_t i=0, Float_t E=0, Float_t p=0, Float_t e=0) : StEmcHit(i, E, p, e) { /* noop */ }

//
//    Inline member functions
//
inline StEmcTowerHit::StEmcTowerHit(Int_t i, Float_t E, Float_t p, Float_t e) : StEmcHit(i, E, p, e) { /* noop */ }
  ClassDef(StEmcTowerHit,1)  //StEmcTowerHit structure
};
StCollectionDef(EmcTowerHit)

#endif
