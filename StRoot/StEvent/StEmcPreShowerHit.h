/***************************************************************************
 *
 * $Id: StEmcPreShowerHit.h,v 1.1 1999/04/27 01:24:18 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcPreShowerHit.h,v $
 * Revision 1.1  1999/04/27 01:24:18  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.2  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/02/23 15:45:39  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcPreShowerHit_hh
#define StEmcPreShowerHit_hh

#include "StArray.h"
#include "StEmcHit.h"
    StEmcPreShowerHit(Int_t, Float_t, Float_t, Float_t);
	ClassDef(StEmcPreShowerHit,1)  //StEmcPreShowerHit structure
public:
  StEmcPreShowerHit(Int_t i=0, Float_t E=0, Float_t p=0, Float_t e=0) :StEmcHit(i, E, p, e) { /* noop */ };
//
//    Inline member functions
//
inline StEmcPreShowerHit::StEmcPreShowerHit(Int_t i, Float_t E, Float_t p, Float_t e) : StEmcHit(i, E, p, e) { /* noop */ }

  ClassDef(StEmcPreShowerHit,1)  //StEmcPreShowerHit structure
};
StCollectionDef(EmcPreShowerHit)
#endif
