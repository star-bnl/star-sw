/***************************************************************************
 *
 * $Id: StEmcPreShowerHit.hh,v 1.1 1999/02/23 15:45:39 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcPreShowerHit.hh,v $
 * Revision 1.1  1999/02/23 15:45:39  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcPreShowerHit_hh
#define StEmcPreShowerHit_hh

#include "StEmcHit.hh"

class StEmcPreShowerHit : public StEmcHit {
public:
    StEmcPreShowerHit(int, float, float, float);
};

//
//    Inline member functions
//
inline StEmcPreShowerHit::StEmcPreShowerHit(int i, float E, float p, float e) : StEmcHit(i, E, p, e) { /* noop */ }

#endif
