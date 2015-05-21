/***************************************************************************
 *
 * $Id: StSstHit.cxx,v 2.2 2015/05/21 14:11:43 ullrich Exp $
 *
 * Author: Jonathan Bouchet, Thomas Ullrich, May 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSstHit.cxx,v $
 * Revision 2.2  2015/05/21 14:11:43  ullrich
 * Changed mADC from int to unsigned short.
 *
 * Revision 2.1  2015/05/13 16:50:59  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include "StSstHit.h"
#include "StTrack.h"

static const char rcsid[] = "$Id: StSstHit.cxx,v 2.2 2015/05/21 14:11:43 ullrich Exp $";

StMemoryPool StSstHit::mPool(sizeof(StSstHit));

ClassImp(StSstHit)

StSstHit::StSstHit()
{
    mLocalPosition[0] = 0;
    mLocalPosition[1] = 0;
    mLocalPosition[2] = 0;
}


StSstHit::StSstHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c)
    : StHit(p, e, hw, q, c)
{
    mLocalPosition[0] = 0;
    mLocalPosition[1] = 0;
    mLocalPosition[2] = 0;
}


StSstHit::~StSstHit() {/* no op */}

unsigned int
StSstHit::ladder() const
{
    unsigned long numwaf = (mHardwarePosition>>4) & ~(~0UL<<9);
    return (numwaf/mWaferPerLadder+1);
}

unsigned int
StSstHit::wafer() const
{
    unsigned long numwaf = (mHardwarePosition>>4) & ~(~0UL<<9);
    return (numwaf-(numwaf/mWaferPerLadder)*mWaferPerLadder+1);
}

unsigned int
StSstHit::centralStripNSide() const
{
  return bits(13, 10);     // bits 13-22
}

unsigned int
StSstHit::centralStripPSide() const
{
    return (bits(23, 5)+bits(13,10)-15);      // bits 23-27
}

unsigned int
StSstHit::clusterSizeNSide() const
{
    return bits(28, 2)+1;    // bits 28-29
}

unsigned int
StSstHit::clusterSizePSide() const
{
    return bits(30, 2)+1;    // bits 30-31
}

float
StSstHit::localPosition(unsigned int i) const
{
    if (i<3)
        return mLocalPosition[i];
    else
        return 0;
}

void
StSstHit::setLocalPosition(float u, float v, float w)
{
    mLocalPosition[0] = u;
    mLocalPosition[1] = v;
    mLocalPosition[2] = w;
}

void StSstHit::setADC(unsigned short adcp, unsigned short adcn) 
{ 
    mADC[0] = adcp; 
    mADC[1] = adcn; 
} 
 
int StSstHit::getADC(unsigned int i) const 
{ 
    if (i<2) 
        return mADC[i]; 
    else 
        return 0; 
} 

int
StSstHit::volumeID() const {return 10000 * sector() + 7000 + 100 * wafer() + ladder();}

ostream&  operator<<(ostream& os, const StSstHit& v)
{
    return os << Form("Sst l:%2i w:%2i",v.ladder(), v.wafer())
	          << *((StHit *)&v)
	          << Form(" Luv: %8.3f %8.3f %8.3f",v.localPosition(0),v.localPosition(1),v.localPosition(2));
}

void StSstHit::Print(const Option_t *option) const { cout << *this << endl;}
