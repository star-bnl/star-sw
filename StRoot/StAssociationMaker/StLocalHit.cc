/***************************************************************************
 *
 * $Id: StLocalHit.cc,v 1.3 1999/09/23 21:25:20 calderon Exp $
 * $Log: StLocalHit.cc,v $
 * Revision 1.3  1999/09/23 21:25:20  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StLocalHit.hh"
#include "StMcParameterDB.h"
#include <iostream.h>
#include <math.h>

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

int StLocalHit::operator==(const StLocalHit& x) const
{    
    return fabs(x.mLocalX  -  mLocalX) < StMcParameterDB::instance()->xCut() &&
	   fabs(x.mGlobalZ - mGlobalZ) < StMcParameterDB::instance()->zCut() ;
	
}
//__________________________

int StLocalHit::operator!=(const StLocalHit& x) const
{
    return !(*this == x);  // use operator==()
}
//__________________________

ostream& operator<<(ostream &os, const StLocalHit& h)
{
    return os << '(' << h.localX() << ", " << h.globalZ() << ')';
}



