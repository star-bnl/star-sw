/***************************************************************************
 *
 * $Id: StSmdEtaHit.hh,v 1.1 1999/02/23 15:45:45 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdEtaHit.hh,v $
 * Revision 1.1  1999/02/23 15:45:45  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSmdEtaHit_hh
#define StSmdEtaHit_hh

#include "StEmcHit.hh"

class StSmdEtaHit : public StEmcHit {
public:
    StSmdEtaHit(int, float, float, float);
};

//
//    Inline member functions
//
inline StSmdEtaHit::StSmdEtaHit(int i, float E, float p, float e) : StEmcHit(i, E, p, e) { /* noop */ }

#endif
