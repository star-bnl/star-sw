/***************************************************************************
 *
 * $Id: StSmdPhiHit.h,v 1.3 1999/05/02 00:00:17 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdPhiHit.h,v $
 * Revision 1.3  1999/05/02 00:00:17  fisyak
 * Add default ctors
 *
 * Revision 1.3  1999/05/02 00:00:17  fisyak
 * Add default ctors
 *
 * Revision 1.2  1999/04/28 22:27:35  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/02/23 15:45:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSmdPhiHit_hh
#define StSmdPhiHit_hh
#include "StArray.h"
#include "StEmcHit.h"

class StSmdPhiHit : public StEmcHit {
public:
  StSmdPhiHit(Int_t i=0, Float_t E=0, Float_t p=0, Float_t e=0) : StEmcHit(i, E, p, e) { /* noop */ };
  ClassDef(StSmdPhiHit,1)  //StSmdPhiHit structure
};
StCollectionDef(SmdPhiHit)

#endif
