/***************************************************************************
 *
 * $Id: StSmdPhiHit.hh,v 1.1 1999/02/23 15:45:43 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdPhiHit.hh,v $
 * Revision 1.1  1999/02/23 15:45:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSmdPhiHit_hh
#define StSmdPhiHit_hh

#include "StEmcHit.hh"

class StSmdPhiHit : public StEmcHit {
public:
    StSmdPhiHit(int, float, float, float);
};

//
//    Inline member functions
//
inline StSmdPhiHit::StSmdPhiHit(int i, float E, float p, float e) : StEmcHit(i, E, p, e) { /* noop */ }

#endif
