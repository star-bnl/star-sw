/***************************************************************************
 *
 * $Id: StSvtHit.cxx,v 2.0 1999/10/12 18:42:43 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cxx,v $
 * Revision 2.0  1999/10/12 18:42:43  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.5  1999/12/13 20:16:19  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.4  1999/11/11 11:03:55  ullrich
 * Inlined layer(), sector() and ladder().
 *
#include "tables/dst_point.h"
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/11/04 21:40:55  ullrich
 * Added missing default constructor
 *
 * Revision 2.1  1999/10/28 22:26:41  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 * Completely Revised for New Version
 *
#include "StTrack.h"
#include "tables/St_dst_point_Table.h"

static const char rcsid[] = "$Id: StSvtHit.cxx,v 2.0 1999/10/12 18:42:43 ullrich Exp $";

ClassImp(StSvtHit)
    
StMemoryPool StSvtHit::mPool(sizeof(StSvtHit));

StSvtHit::StSvtHit() { /* noop */ }

StSvtHit::StSvtHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   ULong_t hw, Float_t q, UChar_t c)
    : StHit(p, e, hw, q, c)
{ /* noop */ }

StSvtHit::StSvtHit(const dst_point_st& pt)
{
    //
    // Unpack charge:
    // The charge is decoded together with its error.
    // Currently only the charge is used but the corresponding
    // error can easily be added.
    //
    const ULong_t svtdq = pt.charge/(1L<<16);
    const ULong_t svtq  = pt.charge - svtdq*(1L<<16);
    mCharge = Float_t(svtq)/(1<<21);

    //
    // Unpack position in xyz
    //
    const Float_t maxRange   = 22;
    const Float_t mapFactor  = 23800;
    ULong_t svty11 = pt.position[0]/(1L<<20);
    ULong_t svtz   = pt.position[1]/(1L<<10);
    ULong_t svtx   = pt.position[0] - (1L<<20)*svty11;
    ULong_t svty10 = pt.position[1] - (1L<<10)*svtz;
    ULong_t svty   = svty11 + (1L<<10)*svty10;
    mPosition.setX(Float_t(svtx)/mapFactor - maxRange);
    mPosition.setY(Float_t(svty)/mapFactor - maxRange);
    mPosition.setZ(Float_t(svtz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    svty11 = pt.pos_err[0]/(1L<<20);
    svty   = svty11 + (1L<<10)*svty10;
    mPositionError.setX(Float_t(svtx)/(1L<<26));
    mPositionError.setY(Float_t(svty)/(1L<<26));
    mPositionError.setZ(Float_t(svtz)/(1L<<26));

    //
    // The hardware position stays at it is
    //
    mHardwarePosition = pt.hw_position;
}

StSvtHit::layer() const
{
    ULong_t w = mHardwarePosition>>4;
    return w/1000;
}

ULong_t
StSvtHit::ladder() const
{
    ULong_t w = mHardwarePosition>>4;
    return w-1000*layer()-100*wafer();
}

ULong_t
StSvtHit::~StSvtHit() {/* noop */}

StObject*
StSvtHit::barrel() const { return layer()/2; }

ULong_t
StSvtHit::barrel() const { return (layer()+1)/2; }

ULong_t
StSvtHit::hybrid() const { return 0; } // to be implemented

