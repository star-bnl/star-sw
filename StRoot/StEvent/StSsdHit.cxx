/***************************************************************************
 *
 * $Id: StSsdHit.cxx,v 2.5 2000/01/05 16:05:37 ullrich Exp $
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
 * Revision 2.5  2000/01/05 16:05:37  ullrich
 * Updated for actual use in StEvent. Unpacking changed.
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
#include "tables/St_dst_point_Table.h"

static const char rcsid[] = "$Id: StSsdHit.cxx,v 2.5 2000/01/05 16:05:37 ullrich Exp $";

StMemoryPool StSsdHit::mPool(sizeof(StSsdHit));

ClassImp(StSsdHit)

StSsdHit::StSsdHit() { /* noop */ }

StSsdHit::StSsdHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   ULong_t hw, Float_t q, UChar_t c)
    : StHit(p, e, hw, q, c)
{ /* noop */ }

StSsdHit::StSsdHit(const dst_point_st& pt)
{
    //
    // Unpack charge:
    // The charge is decoded together with its error.
    // Currently only the charge is used but the corresponding
    // error can easily be added.
    //
    const ULong_t ssddq = pt.charge/(1L<<16);
    const ULong_t ssdq  = pt.charge - ssddq*(1L<<16);
    mCharge = Float_t(ssdq)/(1<<21);

    //
    // Unpack position in xyz
    //
    const Float_t maxRange   = 40;
    const Float_t mapFactor  = 16000;
    ULong_t ssdy11 = pt.position[0]/(1L<<20);
    ULong_t ssdz   = pt.position[1]/(1L<<10);
    ULong_t ssdx   = pt.position[0] - (1L<<20)*ssdy11;
    ULong_t ssdy10 = pt.position[1] - (1L<<10)*ssdz;
    ULong_t ssdy   = ssdy11 + (1L<<10)*ssdy10;
    mPosition.setX(Float_t(ssdx)/mapFactor - maxRange);
    mPosition.setY(Float_t(ssdy)/mapFactor - maxRange);
    mPosition.setZ(Float_t(ssdz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    ssdy11 = pt.pos_err[0]/(1L<<20);
    ssdz   = pt.pos_err[1]/(1L<<10);
    ssdx   = pt.pos_err[0] - (1L<<20)*ssdy11;
    ssdy10 = pt.pos_err[1] - (1L<<10)*ssdz;
    ssdy   = ssdy11 + (1L<<10)*ssdy10;
    mPositionError.setX(Float_t(ssdx)/(1L<<26));
    mPositionError.setY(Float_t(ssdy)/(1L<<26));
    mPositionError.setZ(Float_t(ssdz)/(1L<<26));

    //
    // The hardware position stays at it is
    //
    mHardwarePosition = pt.hw_position;
}

StSsdHit::~StSsdHit() {/* noop */}

StObject*
StSsdHit::clone() { return new StSsdHit(*this); }

ULong_t
StSsdHit::ladder() const
{
    unsigned long numwaf = (mHardwarePosition>>4) & ~(~0UL<<9);
    return (numwaf/mWaferPerLadder+1);
}

ULong_t
StSsdHit::wafer() const
{
    unsigned long numwaf = (mHardwarePosition>>4) & ~(~0UL<<9);
    return (numwaf-(numwaf/mWaferPerLadder)*mWaferPerLadder+1);
}

ULong_t 
StSsdHit::centralStripNSide() const
{
  return bits(13, 10);     // bits 13-22
}

ULong_t
StSsdHit::centralStripPSide() const
{
  return bits(23, 5);      // bits 23-27
}

ULong_t
StSsdHit::clusterSizeNSide() const
{
    return bits(28, 2);    // bits 28-29
}

ULong_t
StSsdHit::clusterSizePSide() const
{
    return bits(30, 2);    // bits 30-31
}
