/***************************************************************************
 *
 * $Id: StSmdPhiHit.h,v 1.1 1999/04/27 01:24:24 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdPhiHit.h,v $
 * Revision 1.1  1999/04/27 01:24:24  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.2  1999/04/28 22:27:35  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/02/23 15:45:43  ullrich
 * Initial Revision
 *

#ifndef StSmdPhiHit_hh
#define StSmdPhiHit_hh
#include "StArray.h"
#include "StEmcHit.h"
    StSmdPhiHit(Int_t, Float_t, Float_t, Float_t);
    ClassDef(StSmdPhiHit,1)  //StSmdPhiHit structure
public:
  StSmdPhiHit(Int_t i=0, Float_t E=0, Float_t p=0, Float_t e=0) : StEmcHit(i, E, p, e) { /* noop */ };

//
//    Inline member functions
//
inline StSmdPhiHit::StSmdPhiHit(Int_t i, Float_t E, Float_t p, Float_t e) : StEmcHit(i, E, p, e) { /* noop */ }
  ClassDef(StSmdPhiHit,1)  //StSmdPhiHit structure
};
StCollectionDef(SmdPhiHit)

#endif
