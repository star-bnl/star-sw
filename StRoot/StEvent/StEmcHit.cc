/***************************************************************************
 *
 * $Id: StEmcHit.cc,v 1.1 1999/02/23 15:45:37 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcHit.cc,v $
 * Revision 1.1  1999/02/23 15:45:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcHit.hh"
#include <iostream.h>

ostream& operator<< (ostream& os, const StEmcHit& h)
{
    os << '(' << h.eta() << ", " << h.phi() << "; " << h.energy() << ')';
    return os;
}
