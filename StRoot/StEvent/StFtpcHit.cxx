/***************************************************************************
 *
 * $Id: StFtpcHit.cxx,v 2.3 1999/11/09 19:35:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.cxx,v $
 * Revision 2.3  1999/11/09 19:35:09  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.5  1999/12/13 20:16:12  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.4  1999/12/06 18:28:21  ullrich
 * Changed method names xxxInCluster to xxxInHit
 *
 * Revision 2.3  1999/11/09 19:35:09  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/11/04 21:40:49  ullrich
 * Added missing default constructor
 *
 * Revision 2.1  1999/10/28 22:25:16  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:02  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StFtpcHit.h"
#include "tables/St_dst_point_Table.h"
#include "StTrack.h"

static const char rcsid[] = "$Id: StFtpcHit.cxx,v 2.3 1999/11/09 19:35:09 ullrich Exp $";

StMemoryPool StFtpcHit::mPool(sizeof(StFtpcHit));

ClassImp(StFtpcHit)

StFtpcHit::StFtpcHit() { /* noop */ }

StFtpcHit::StFtpcHit(const StThreeVectorF& p,
                     const StThreeVectorF& e,
                     ULong_t hw, Float_t q, UChar_t c)
    : StHit(p, e, hw, q, c)
{ /* noop */ }

StFtpcHit::StFtpcHit(const dst_point_st& pt)
{
    //
    // Unpack charge:
    // The charge is decoded together with the max. ADC value.
    // Currently only the charge is used but the corresponding
    // ADC can easily be added.
    //
    const ULong_t maxadc = pt.charge/(1L<<16);
    const ULong_t ftpcq  = pt.charge - maxadc*(1L<<16);
    mCharge = Float_t(ftpcq)/(1<<16);

    //
    // Unpack position in xyz
    //
    const Float_t maxRange   = 270;
    const Float_t mapFactor  = 2380;
    ULong_t ftpcy11 = pt.position[0]/(1L<<20);
    ULong_t ftpcz   = pt.position[1]/(1L<<10);
    ULong_t ftpcx   = pt.position[0] - (1L<<20)*ftpcy11;
    ULong_t ftpcy10 = pt.position[1] - (1L<<10)*ftpcz;
    ULong_t ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;
    mPosition.setX(Float_t(ftpcx)/mapFactor - maxRange);
    mPosition.setY(Float_t(ftpcy)/mapFactor - maxRange);
    mPosition.setZ(Float_t(ftpcz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    ftpcy11 = pt.pos_err[0]/(1L<<20);
    ftpcz   = pt.pos_err[1]/(1L<<10);
    ftpcx   = pt.pos_err[0] - (1L<<20)*ftpcy11;
    ftpcy10 = pt.pos_err[1] - (1L<<10)*ftpcz;
    ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;
    mPositionError.setX(Float_t(ftpcx)/(1L<<17));
    mPositionError.setY(Float_t(ftpcy)/(1L<<17));
    mPositionError.setZ(Float_t(ftpcz)/(1L<<17));

    //
    // The hardware position stays at it is
    //
    mHardwarePosition = pt.hw_position;
}

StFtpcHit::~StFtpcHit() {/* noop */}

StObject*
StFtpcHit::clone() { return new StFtpcHit(*this); }

    return bits(11, 10)-1;   // bits 11-20
StFtpcHit::sector() const
{
    return bits(11, 10);   // bits 11-20
}

    return bits(4, 7)-1;    // bits 4-10
StFtpcHit::padsInCluster() const
{
    return bits(4, 7);    // bits 4-10
}

ULong_t
StFtpcHit::timebinsInCluster() const
{
    return bits(21, 4);   // bits 21-24
}

ULong_t
StFtpcHit::timebinsInHit() const
{
    return bits(25, 7);   // bits 25-31
}
