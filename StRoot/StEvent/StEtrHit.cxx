/***************************************************************************
 *
 * $Id: StEtrHit.cxx,v 2.2 2012/03/22 00:07:32 perev Exp $
 *
 * Author: Ming Shao, Jan 2012
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEtrHit.cxx,v $
 * Revision 2.2  2012/03/22 00:07:32  perev
 * Section added
 *
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
		   int sector, int layer, int section, float charge)
//          volume_id = section + 100*layer + 10000*sector
  : StHit(p, zero3V, section+100*(layer+100*sector), charge, 0)
{}

//______________________________________________________________________________
StEtrHit::~StEtrHit() {/* noop */}
//______________________________________________________________________________
int StEtrHit::section() const		{return (hardwarePosition()      )%100;}
//______________________________________________________________________________
int StEtrHit::layer()   const		{return (hardwarePosition()/100  )%100;}
//______________________________________________________________________________
int StEtrHit::sector()  const		{return (hardwarePosition()/10000)%100;}



