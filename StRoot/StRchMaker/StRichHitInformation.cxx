/***************************************************************************
 *
 * $Id: StRichHitInformation.cxx,v 2.2 2000/09/29 19:05:23 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of Hit Info
 *
 ***************************************************************************
 *
 * $Log: StRichHitInformation.cxx,v $
 * Revision 2.2  2000/09/29 19:05:23  lasiuk
 * number of pads added as well as ostream operator
 *
 * Revision 2.1  2000/09/13 21:01:58  lasiuk
 * Initial Revision
 *
 *
 **************************************************************************/
#include "StRichHitInformation.h"


StRichHitInformation::StRichHitInformation()
    : mPosition(0,0,0), mPositionError(0,0,0),
      mPosition2(0,0,0), mCharge(0), mMaxAdc(0), mClusterNumber(-1)
{ /* no-op */ }

StRichHitInformation::StRichHitInformation(StThreeVector<double> x,
					   StThreeVector<double> dx,
					   StThreeVector<double> x2,
					   float q, float maxAdc, int cno)
    : mPosition(x), mPositionError(dx),
      mPosition2(x2), mCharge(q), mMaxAdc(maxAdc), mClusterNumber(cno)
{ /* no-op */ }

StRichHitInformation::~StRichHitInformation()
{ /* no-op */ }

void StRichHitInformation::setPosition(double x, double y, double z)
{
    mPosition = StThreeVector<double>(x,y,z);
}
void StRichHitInformation::setPositionError(double dx, double dy, double dz)
{
    mPositionError = StThreeVector<double>(dx,dy,dz);
}
void StRichHitInformation::setPosition2(double x2, double y2, double z2)
{
    mPosition2 = StThreeVector<double>(x2,y2,z2);
}

ostream& operator<<(ostream& os, StRichHitInformation& hit)
{
    return (os
	    << " x=" << hit.position() << " q=" << hit.charge()
	    << " q_max=" << hit.maxAdc() << " #=" << hit.clusterNumber());
}
