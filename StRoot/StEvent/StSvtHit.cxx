/***************************************************************************
 *
 * $Id: StSvtHit.cxx,v 1.5 1999/06/27 22:45:28 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cxx,v $
 * Revision 1.5  1999/06/27 22:45:28  fisyak
 * Merge StRootEvent and StEvent
 *
 * Revision 1.5  1999/06/27 22:45:28  fisyak
 * Merge StRootEvent and StEvent
 *
 * Revision 1.4  1999/05/05 22:36:41  fisyak
 * restore relatedTracks
 *
 * Revision 1.3  1999/04/28 22:27:35  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/03/23 21:51:14  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:54  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.5  1999/12/13 20:16:19  ullrich
#include "tables/dst_point.h"
#include "StGlobalTrack.h"
static const Char_t rcsid[] = "$Id: StSvtHit.cxx,v 1.5 1999/06/27 22:45:28 fisyak Exp $";
#include "dst_point.h"
 * Inlined layer(), sector() and ladder().
static const Char_t rcsid[] = "$Id: StSvtHit.cxx,v 1.5 1999/06/27 22:45:28 fisyak Exp $";
#include "tables/dst_point.h"
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
StCollectionImp(SvtHit)
 * Revision 2.2  1999/11/04 21:40:55  ullrich
	           const StThreeVectorF& e,
	           Float_t q, UChar_t c)
    : StHit(p, e, q, c)
 * Adapted new StArray version. First version to compile on Linux and Sun.
 * Completely Revised for New Version
StSvtHit::StSvtHit(dst_point_st* pt)
#include "StTrack.h"
#include "tables/St_dst_point_Table.h"
    // Decode position. cf. pams/global/dst/dst_point_filler.F
    //                      pams/global/dst/dst_point_unpack.F
    //
    if (!pt) {                 // nothing to be done
	mCharge = 0;
	mTrackRefCount = 0;
	return;
    }

    //

static const char rcsid[] = "$Id: StSvtHit.cxx,v 1.5 1999/06/27 22:45:28 fisyak Exp $";

ClassImp(StSvtHit)
    
    const ULong_t svtdq = pt->charge/(1L<<16);
    const ULong_t svtq  = pt->charge - svtdq*(1L<<16);
StSvtHit::StSvtHit() { /* noop */ }

StSvtHit::StSvtHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   ULong_t hw, Float_t q, UChar_t c)
    : StHit(p, e, hw, q, c)
    const Float_t mapFactor  = 23800;   
    ULong_t svty11 = pt->position[0]/(1L<<20);
    ULong_t svtz   = pt->position[1]/(1L<<10);
    ULong_t svtx   = pt->position[0] - (1L<<20)*svty11;
    ULong_t svty10 = pt->position[1] - (1L<<10)*svtz;
    ULong_t svty   = svty11 + (1L<<10)*svty10;	
    mPosition.setX(Float_t(svtx)/mapFactor - maxRange); 
    // Currently only the charge is used but the corresponding
    // error can easily be added.
    //
    const ULong_t svtdq = pt.charge/(1L<<16);
    const ULong_t svtq  = pt.charge - svtdq*(1L<<16);
    mCharge = Float_t(svtq)/(1<<21);
    svty11 = pt->pos_err[0]/(1L<<20);
    svtz   = pt->pos_err[1]/(1L<<10);
    svtx   = pt->pos_err[0] - (1L<<20)*svty11;
    svty10 = pt->pos_err[1] - (1L<<10)*svtz;
    const Float_t maxRange   = 22;
    mPositionError.setX(Float_t(svtx)/(1L<<26)); 
    ULong_t svty11 = pt.position[0]/(1L<<20);
    ULong_t svtz   = pt.position[1]/(1L<<10);
    mPositionError.setZ(Float_t(svtz)/(1L<<26));

StVecPtrGlobalTrack StSvtHit::relatedTracks(const StTrackCollection& c)
    //
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrSvtHit &hits = track->svtHits();
	//	if (find(hits.begin(), hits.end(), this) != hits.end())
	if (hits.FindObject(this))
	  result.push_back(track);
    }
    return result;
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

