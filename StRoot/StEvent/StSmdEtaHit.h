/***************************************************************************
 *
 * $Id: StSmdEtaHit.h,v 1.3 1999/05/02 00:00:17 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdEtaHit.h,v $
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
#ifndef StSmdEtaHit_hh
#define StSmdEtaHit_hh
#include "StArray.h"
#include "StEmcHit.h"

class StSmdEtaHit : public StEmcHit {
public:
  StSmdEtaHit(Int_t i=0, Float_t E=0, Float_t p=0, Float_t e=0) : StEmcHit(i, E, p, e) { /* noop */ };
  ClassDef(StSmdEtaHit,1)  //StSmdEtaHit structure
};
StCollectionDef(SmdEtaHit)

#endif
