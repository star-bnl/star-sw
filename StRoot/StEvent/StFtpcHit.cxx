/***************************************************************************
 *
 * $Id: StFtpcHit.cxx,v 2.11 2004/07/15 16:36:24 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.cxx,v $
 * Revision 2.11  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.10  2004/05/07 15:05:28  calderon
 * Adding constructor based on StFtpcPoint from Markus.
 *
 * Revision 2.9  2004/04/08 19:02:33  ullrich
 * Added additional data member and access methods to hold the position in
 * pad and time units including their std deviation. Constructors updated.
 *
 * Revision 2.8  2001/04/05 04:00:50  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2001/03/24 03:34:46  perev
 * clone() -> clone() const
 *
 * Revision 2.6  2000/06/01 21:38:49  ullrich
 * Added member mFlag and access member flag() and setFlag().
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
#include "StFtpcTrackMaker/StFtpcPoint.hh"
#include "StTrack.h"

static const char rcsid[] = "$Id: StFtpcHit.cxx,v 2.11 2004/07/15 16:36:24 ullrich Exp $";

StMemoryPool StFtpcHit::mPool(sizeof(StFtpcHit));

ClassImp(StFtpcHit)

StFtpcHit::StFtpcHit()
{
    mPadPos = 0;      
    mTimePos = 0;     
    mPadPosSigma = 0; 
    mTimePosSigma = 0;
}   

StFtpcHit::StFtpcHit(const StThreeVectorF& p,
                     const StThreeVectorF& e,
                     unsigned int hw, float q, unsigned char c)
    : StHit(p, e, hw, q, c)
{
    mPadPos = 0;      
    mTimePos = 0;     
    mPadPosSigma = 0; 
    mTimePosSigma = 0;
}

StFtpcHit::StFtpcHit(const dst_point_st& pt)
{
    //
    // Unpack charge and status flag
    //
    const unsigned int iflag = pt.charge/(1L<<16);
    const unsigned int ftpcq  = pt.charge - iflag*(1L<<16);
    mCharge = float(ftpcq)/(1<<16);
    mFlag = static_cast<unsigned char>(iflag);

    //
    // Unpack position in xyz
    //
    const float maxRange   = 270;
    const float mapFactor  = 2380;
    unsigned int ftpcy11 = pt.position[0]/(1L<<20);
    unsigned int ftpcz   = pt.position[1]/(1L<<10);
    unsigned int ftpcx   = pt.position[0] - (1L<<20)*ftpcy11;
    unsigned int ftpcy10 = pt.position[1] - (1L<<10)*ftpcz;
    unsigned int ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;
    mPosition.setX(float(ftpcx)/mapFactor - maxRange);
    mPosition.setY(float(ftpcy)/mapFactor - maxRange);
    mPosition.setZ(float(ftpcz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    ftpcy11 = pt.pos_err[0]/(1L<<20);
    ftpcz   = pt.pos_err[1]/(1L<<10);
    ftpcx   = pt.pos_err[0] - (1L<<20)*ftpcy11;
    ftpcy10 = pt.pos_err[1] - (1L<<10)*ftpcz;
    ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;
    mPositionError.setX(float(ftpcx)/(1L<<17));
    mPositionError.setY(float(ftpcy)/(1L<<17));
    mPositionError.setZ(float(ftpcz)/(1L<<17));

    //
    // The hardware position stays as it is
    //
    mHardwarePosition = pt.hw_position;

    mPadPos = 0;      
    mTimePos = 0;     
    mPadPosSigma = 0; 
    mTimePosSigma = 0;
}

StFtpcHit::StFtpcHit(const StFtpcPoint& pt)
{
  update(pt);
}

void StFtpcHit::update(const StFtpcPoint& pt)
{
    //
    // charge and status flag
    //
    const unsigned int iflag = pt.GetFlags();
    const unsigned int ftpcq  = pt.GetCharge();
    mCharge = float(ftpcq)/(1<<16);
    mFlag = static_cast<unsigned char>(iflag);

    //
    // position in xyz
    //
    mPosition.setX(pt.GetX());
    mPosition.setY(pt.GetY());
    mPosition.setZ(pt.GetZ());
       
    //
    // error on position in xyz
    //
    mPositionError.setX(pt.GetXerr());
    mPositionError.setY(pt.GetYerr());
    mPositionError.setZ(pt.GetZerr());

    //
    // The hardware position stays as it is
    //
    mHardwarePosition = pt.GetHardwarePosition();

    mPadPos = pt.GetPadPos();      
    mTimePos = pt.GetTimePos();     
    mPadPosSigma = pt.GetPadPosSigma(); 
    mTimePosSigma = pt.GetTimePosSigma();
}

StFtpcHit::~StFtpcHit() {/* noop */}

unsigned int
StFtpcHit::sector() const
{
    return bits(11, 10);   // bits 11-20
}

unsigned int
StFtpcHit::plane() const
{
    return bits(4, 7);    // bits 4-10
}

unsigned int
StFtpcHit::padsInHit() const
{
    return bits(21, 4);   // bits 21-24
}

unsigned int
StFtpcHit::timebinsInHit() const
{
    return bits(25, 7);   // bits 25-31
}

