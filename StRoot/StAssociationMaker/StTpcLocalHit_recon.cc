
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
