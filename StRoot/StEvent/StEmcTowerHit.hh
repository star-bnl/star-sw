/***************************************************************************
 *
 * $Id: StEmcTowerHit.hh,v 1.1 1999/02/23 15:45:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTowerHit.hh,v $
 * Revision 1.1  1999/02/23 15:45:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcTowerHit_hh
#define StEmcTowerHit_hh

#include "StEmcHit.hh"

class StEmcTowerHit : public StEmcHit {
public:
    StEmcTowerHit(int, float, float, float);
};

//
//    Inline member functions
//
inline StEmcTowerHit::StEmcTowerHit(int i, float E, float p, float e) : StEmcHit(i, E, p, e) { /* noop */ }

#endif
