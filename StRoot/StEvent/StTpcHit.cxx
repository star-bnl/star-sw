/***************************************************************************
 *
 * $Id: StTpcHit.cxx,v 1.1 1999/01/30 03:58:08 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cxx,v $
 * Revision 1.1  1999/01/30 03:58:08  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/04/28 22:27:36  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/03/23 21:51:09  ullrich
 * Added table-based class specific constructor.
#include "StGlobalTrack.h"
#include "StTrackCollection.h"
 *
static const Char_t rcsid[] = "$Id: StTpcHit.cxx,v 1.1 1999/01/30 03:58:08 fisyak Exp $";
#include "tables/dst_point.h"
#ifdef __ROOT__
#include "StGlobalTrack.h"
static const Char_t rcsid[] = "$Id: StTpcHit.cxx,v 1.1 1999/01/30 03:58:08 fisyak Exp $";
#endif
 * Inlined sector() and padrow().
static const Char_t rcsid[] = "$Id: StTpcHit.cxx,v 1.1 1999/01/30 03:58:08 fisyak Exp $";
#include "tables/dst_point.h"
 * Memory now allocated using StMemoryPool via overloaded new/delete
#if 0
    const Float_t maxRange   = 220;
    mPositionError.setX(Float_t(tpcx)/(1L<<17)); 
    ULong_t tpcy11 = pt.position[0]/(1L<<20);
    ULong_t tpcz   = pt.position[1]/(1L<<10);
}

StVecPtrGlobalTrack StTpcHit::relatedTracks(const StTrackCollection& c)
{
	const StVecPtrTpcHit &hits = track->tpcHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrTpcHit &hits = track->tpcHits();
#endif
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
