#include "StLocalHit.hh"
#include "StMcParameterDB.h"
#include <iostream.h>
#include <cmath>

//ClassImp(StLocalHit.cc)

//__________________________
StLocalHit::StLocalHit()
{
  mLocalX = 0.0;
  mGlobalZ = 0.0;
}
//__________________________
StLocalHit::StLocalHit(const float& x, const float& z)
{
  mLocalX = x;
  mGlobalZ = z;
}

//__________________________

StLocalHit::~StLocalHit()
{
  /* noop */
}


//__________________________

bool StLocalHit::operator==(const StLocalHit& x) const
{    
    return abs(x.mLocalX  -  mLocalX) < StMcParameterDB::instance()->xCut() &&
	   abs(x.mGlobalZ - mGlobalZ) < StMcParameterDB::instance()->zCut() ;
	
}
//__________________________

bool StLocalHit::operator!=(const StLocalHit& x) const
{
    return !(*this == x);  // use operator==()
}
//__________________________

ostream& operator<<(ostream &os, const StLocalHit& h)
{
    return os << '(' << h.localX() << ", " << h.globalZ() << ')';
}



