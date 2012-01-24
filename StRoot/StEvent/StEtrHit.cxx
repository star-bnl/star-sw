/***************************************************************************
 *
 * $Id: StEtrHit.cxx,v 2.1 2012/01/24 03:06:12 perev Exp $
 *
 * Author: Ming Shao, Jan 2012
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEtrHit.cxx,v $
 * Revision 2.1  2012/01/24 03:06:12  perev
 * Add Etr
 *
 *
 * Revision 1.0  2012/01/05 Ming
 * Initial Version
 *
 **************************************************************************/
#include "StEtrHit.h"
#include "StTrack.h"
static StThreeVectorF zero3V(0,0,9);

ClassImp(StEtrHit)

//______________________________________________________________________________
StEtrHit::StEtrHit()
{
}   

//______________________________________________________________________________
StEtrHit::StEtrHit(const StThreeVectorF& p,
		   int sector, int layer, float charge)
  : StHit(p, zero3V, sector*10+layer, charge, 0)
{
}

//______________________________________________________________________________
StEtrHit::~StEtrHit() {/* noop */}




