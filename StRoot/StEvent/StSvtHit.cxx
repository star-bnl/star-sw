/***************************************************************************
 *
 * $Id: StSvtHit.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cxx,v $
 * Revision 1.1  1999/01/30 03:58:07  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/03/23 21:51:14  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:54  wenaus
 * version with constructors for table-based loading
#include "StGlobalTrack.h"
#include "StTrackCollection.h"
#include "StGlobalTrack.h"
static const Char_t rcsid[] = "$Id: StSvtHit.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $";
#include "dst_point.h"
#ifdef __ROOT__
 * Inlined layer(), sector() and ladder().
static const Char_t rcsid[] = "$Id: StSvtHit.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $";
#endif
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
StCollectionImp(SvtHit)
 * Revision 2.2  1999/11/04 21:40:55  ullrich
	           const StThreeVectorF& e,
#if 0
    ULong_t svtz   = pt.position[1]/(1L<<10);
    mPositionError.setZ(Float_t(svtz)/(1L<<26));

StVecPtrGlobalTrack StSvtHit::relatedTracks(const StTrackCollection& c)
    //
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
	const StVecPtrSvtHit &hits = track->svtHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
	//	if (find(hits.begin(), hits.end(), this) != hits.end())
	if (hits.FindObject(this))
	  result.push_back(track);
#endif
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

