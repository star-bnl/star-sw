/***************************************************************************
 *
 * $Id: StTpcLocalHit_recon.cc,v 1.2 1999/09/23 21:25:23 calderon Exp $
 * $Log: StTpcLocalHit_recon.cc,v $
 * Revision 1.2  1999/09/23 21:25:23  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/

#include "StTpcLocalHit_recon.hh"

//________________________________
StTpcLocalHit_recon::StTpcLocalHit_recon(const StTpcHit* hit, float xLocal, float zGlobal):StLocalHit(xLocal,zGlobal) {
  mHit = hit;
}

//________________________________
StTpcLocalHit_recon::~StTpcLocalHit_recon(){ /*noop*/ }

//________________________________

ostream& operator<<(ostream& os, const StTpcLocalHit_recon& h)
{
    return os << (StLocalHit&) h;
}
