/***************************************************************************
 *
 * $Id: StEmcTowerHit.h,v 1.3 1999/05/02 00:00:16 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTowerHit.h,v $
 * Revision 1.3  1999/05/02 00:00:16  fisyak
 * Add default ctors
 *
 * Revision 1.3  1999/05/02 00:00:16  fisyak
 * Add default ctors
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

class StEmcTowerHit : public StEmcHit {
public:
  StEmcTowerHit(Int_t i=0, Float_t E=0, Float_t p=0, Float_t e=0) : StEmcHit(i, E, p, e) { /* noop */ }
  ClassDef(StEmcTowerHit,1)  //StEmcTowerHit structure
};
StCollectionDef(EmcTowerHit)

#endif
