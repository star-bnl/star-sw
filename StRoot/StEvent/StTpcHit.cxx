/***************************************************************************
 *
 * $Id: StTpcHit.cxx,v 1.6 1999/08/25 12:50:07 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cxx,v $
 * Revision 1.6  1999/08/25 12:50:07  ullrich
 * Buf fixed in unpacking the charge.
 *
 * Revision 1.6  1999/08/25 12:50:07  ullrich
 * Buf fixed in unpacking the charge.
 *
 * Revision 1.5  1999/06/27 22:45:28  fisyak
 * Merge StRootEvent and StEvent
 *
 * Revision 1.4  1999/05/05 22:36:42  fisyak
 * restore relatedTracks
 *
 * Revision 1.3  1999/04/28 22:27:36  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/03/23 21:51:09  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:58  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.5  1999/12/01 15:56:28  ullrich
#include "tables/dst_point.h"
#include "StGlobalTrack.h"
static const Char_t rcsid[] = "$Id: StTpcHit.cxx,v 1.6 1999/08/25 12:50:07 ullrich Exp $";
#include "dst_point.h"
 * Inlined sector() and padrow().
static const Char_t rcsid[] = "$Id: StTpcHit.cxx,v 1.6 1999/08/25 12:50:07 ullrich Exp $";
#include "tables/dst_point.h"
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
StCollectionImp(TpcHit)
 * Revision 2.2  1999/11/04 21:41:00  ullrich
	           const StThreeVectorF& e,
	           Float_t q, UChar_t c)  : StHit(p, e, q, c)
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
StTpcHit::StTpcHit(dst_point_st* pt)
{    
    //
    // Decode position. cf. pams/global/dst/dst_point_filler.F
    //                      pams/global/dst/dst_point_unpack.F
    //   
    if (!pt) {                 // nothing to be done
	mCharge = 0;
	mTrackRefCount = 0;
	return;
    }

#include "tables/St_dst_point_Table.h"

static const char rcsid[] = "$Id: StTpcHit.cxx,v 1.6 1999/08/25 12:50:07 ullrich Exp $";

StMemoryPool StTpcHit::mPool(sizeof(StTpcHit));

    const ULong_t tpcdq = pt->charge/(1L<<16);
    const ULong_t tpcq  = pt->charge - tpcdq*(1L<<16);
StTpcHit::StTpcHit() { /* noop */ }

StTpcHit::StTpcHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   ULong_t hw, Float_t q, UChar_t c)
    : StHit(p, e, hw, q, c)
    const Float_t mapFactor  = 2380;   
    ULong_t tpcy11 = pt->position[0]/(1L<<20);
    ULong_t tpcz   = pt->position[1]/(1L<<10);
    ULong_t tpcx   = pt->position[0] - (1L<<20)*tpcy11;
    ULong_t tpcy10 = pt->position[1] - (1L<<10)*tpcz;
    ULong_t tpcy   = tpcy11 + (1L<<10)*tpcy10;	
    mPosition.setX(Float_t(tpcx)/mapFactor - maxRange); 
    // Currently only the charge is used but the corresponding
    // error can easily be added.
    //
    const ULong_t tpcdq = pt.charge/(1L<<16);
    const ULong_t tpcq  = pt.charge - tpcdq*(1L<<16);
    mCharge = Float_t(tpcq)/(1<<25);
    tpcy11 = pt->pos_err[0]/(1L<<20);
    tpcz   = pt->pos_err[1]/(1L<<10);
    tpcx   = pt->pos_err[0] - (1L<<20)*tpcy11;
    tpcy10 = pt->pos_err[1] - (1L<<10)*tpcz;
    const Float_t maxRange   = 220;
    mPositionError.setX(Float_t(tpcx)/(1L<<17)); 
    ULong_t tpcy11 = pt.position[0]/(1L<<20);
    ULong_t tpcz   = pt.position[1]/(1L<<10);
}

StVecPtrGlobalTrack StTpcHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrTpcHit &hits = track->tpcHits();
	//	if (find(hits.begin(), hits.end(), this) != hits.end())
	if (hits.FindObject(this))
	    result.push_back(track);
    }
    return result;
}

ULong_t
StTpcHit::~StTpcHit() {/* noop */}

StObject*
StTpcHit::padsInCluster() const

ULong_t
StTpcHit::padsInHit() const
{
    return bits(15, 7);   // bits 15-21
StTpcHit::pixelsInCluster() const

ULong_t
StTpcHit::pixelsInHit() const
{
    return bits(22, 10);   // bits 22-31
}
