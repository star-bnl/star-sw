/***************************************************************************
 *
 * $Id: StTpcLocalHit_mc.cc,v 1.2 1999/09/23 21:25:22 calderon Exp $
 * $Log: StTpcLocalHit_mc.cc,v $
 * Revision 1.2  1999/09/23 21:25:22  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/

#include "StTpcLocalHit_mc.hh"

//________________________________
StTpcLocalHit_mc::StTpcLocalHit_mc(const StMcTpcHit* hit, float xLocal, float zGlobal):StLocalHit(xLocal,zGlobal) {
  mHit = hit;
}

//________________________________
StTpcLocalHit_mc::~StTpcLocalHit_mc(){ /*noop*/ }

//________________________________

ostream& operator<<(ostream& os, const StTpcLocalHit_mc& h)
{
    return os << (StLocalHit&) h;
}
