
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
