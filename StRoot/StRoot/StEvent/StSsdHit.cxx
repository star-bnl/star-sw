/***************************************************************************
 *
 * $Id: StSsdHit.cxx,v 2.14 2009/11/23 22:20:51 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *         Lilian Martin, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHit.cxx,v $
 * Revision 2.14  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.13  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.12  2009/11/10 00:40:17  ullrich
 * Changed print-out format.
 *
 * Revision 2.11  2006/04/27 21:58:53  ullrich
 * Added data member and methods to deal with local positions.
 *
 * Revision 2.10  2005/12/19 19:24:10  ullrich
 * Applied patch by A. Kiesel to fix tp correct decoding of hardware info.
 *
 * Revision 2.9  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2001/04/05 04:00:55  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2001/03/24 03:34:58  perev
 * clone() -> clone() const
 *
 * Revision 2.6  2000/06/01 21:39:00  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.5  2000/01/05 16:05:37  ullrich
 * Updated for actual use in StEvent. Unpacking changed.
 *
 * Revision 2.4  1999/11/09 19:35:15  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.3  1999/11/04 21:40:52  ullrich
 * Added missing default constructor
 *
 * Revision 2.2  1999/10/28 22:26:36  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSsdHit.h"
#include "StTrack.h"

static const char rcsid[] = "$Id: StSsdHit.cxx,v 2.14 2009/11/23 22:20:51 ullrich Exp $";

StMemoryPool StSsdHit::mPool(sizeof(StSsdHit));

ClassImp(StSsdHit)

StSsdHit::StSsdHit()
{
    mLocalPosition[0] = 0;
    mLocalPosition[1] = 0;
}


StSsdHit::StSsdHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c)
    : StHit(p, e, hw, q, c)
{
    mLocalPosition[0] = 0;
    mLocalPosition[1] = 0;
}


StSsdHit::~StSsdHit() {/* noop */}

unsigned int
StSsdHit::ladder() const
{
    unsigned long numwaf = (mHardwarePosition>>4) & ~(~0UL<<9);
    return (numwaf/mWaferPerLadder+1);
}

unsigned int
StSsdHit::wafer() const
{
    unsigned long numwaf = (mHardwarePosition>>4) & ~(~0UL<<9);
    return (numwaf-(numwaf/mWaferPerLadder)*mWaferPerLadder+1);
}

unsigned int
StSsdHit::centralStripNSide() const
{
  return bits(13, 10);     // bits 13-22
}

unsigned int
StSsdHit::centralStripPSide() const
{
    return (bits(23, 5)+bits(13,10)-15);      // bits 23-27
}

unsigned int
StSsdHit::clusterSizeNSide() const
{
    return bits(28, 2)+1;    // bits 28-29
}

unsigned int
StSsdHit::clusterSizePSide() const
{
    return bits(30, 2)+1;    // bits 30-31
}

float
StSsdHit::localPosition(unsigned int i) const
{
    if (i<2)
        return mLocalPosition[i];
    else
        return 0;
}

void
StSsdHit::setLocalPosition(float u, float v)
{
    mLocalPosition[0] = u;
    mLocalPosition[1] = v;
}

int
StSsdHit::volumeID() const {return 10000 * sector() + 7000 + 100 * wafer() + ladder();}

ostream&  operator<<(ostream& os, const StSsdHit& v)
{
    return os << Form("Ssd l:%2i w:%2i",v.ladder(), v.wafer())
	    << *((StHit *)&v)
	    << Form(" Luv: %8.3f %8.3f",v.localPosition(0),v.localPosition(1));
}
void StSsdHit::Print(const Option_t *option) const { cout << *this << endl;}
