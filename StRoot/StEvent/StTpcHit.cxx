/***************************************************************************
 *
 * $Id: StTpcHit.cxx,v 2.15 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cxx,v $
 * Revision 2.15  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.14  2009/11/10 00:40:18  ullrich
 * Changed print-out format.
 *
 * Revision 2.13  2007/10/03 21:47:35  ullrich
 * Added several new member to hold hit length info.
 *
 * Revision 2.12  2004/08/06 15:37:09  fisyak
 * Add clster id
 *
 * Revision 2.11  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.10  2004/01/13 21:01:08  fisyak
 * Add Truth and Quality information from simulation
 *
 * Revision 2.9  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.8  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.7  2000/07/28 23:29:42  calderon
 * Added handling of Fit Flag: use this flag to tell if the point
 * is used in the fit.
 *
 * Revision 2.6  2000/06/01 21:39:07  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.5  1999/12/01 15:56:28  ullrich
 * Renamed xxxInCluster() methods to xxxInHit()
 *
 * Revision 2.4  1999/11/11 10:19:52  ullrich
 * Inlined sector() and padrow().
 *
 * Revision 2.3  1999/11/09 19:35:25  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/11/04 21:41:00  ullrich
 * Added missing default constructor
 *
 * Revision 2.1  1999/10/28 22:27:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:48  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StTpcHit.h"
#include "StTrack.h"
static const char rcsid[] = "$Id: StTpcHit.cxx,v 2.15 2009/11/23 16:34:07 fisyak Exp $";

StMemoryPool StTpcHit::mPool(sizeof(StTpcHit));

ClassImp(StTpcHit)
    
StTpcHit::StTpcHit()
{
    mMinpad = mMaxpad = mMintmbk = mMaxtmbk = 0;
    mMcl_x = mMcl_t = 0;
    mChargeModified = 0;
}

StTpcHit::StTpcHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c,
	         unsigned short idTruth, unsigned short quality, unsigned short id,
	         short mnpad, short mxpad, short mntmbk,
	         short mxtmbk, float cl_x, float cl_t)
    : StHit(p, e, hw, q, c, idTruth, quality)
{
    mMcl_x = static_cast<short>(cl_x*64);
    mMcl_t = static_cast<short>(cl_t*64);
    Short_t pad  = mMcl_x/64;
    Short_t time = mMcl_t/64;
    mMinpad = pad - mnpad;
    mMaxpad = mxpad - pad;
    mMintmbk = time - mntmbk;
    mMaxtmbk = mxtmbk - time;
    mChargeModified = 0;
}
StTpcHit::~StTpcHit() {/* noop */}

ostream&  operator<<(ostream& os, const StTpcHit& v)
{
    return os << Form("Tpc s/r %3i/%3i ",v.sector(),v.padrow())
	    << *((StHit *)&v)
	    << Form(" min/max pad %3i/%3i npad %2i min/max t %3i/%3i ntime %2i time %10.3f pad %10.3f",
                      (int)  v.minPad(), (int)  v.maxPad(),(int) v.padsInHit(), 
                      (int) v.minTmbk(), (int) v.maxTmbk(),(int) v.timeBucketsInHit(),
                      v.timeBucket(),v.pad()); 
}
void   StTpcHit::Print(Option_t *option) const {cout << *this << endl;}
