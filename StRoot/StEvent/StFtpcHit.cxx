/***************************************************************************
 *
 * $Id: StFtpcHit.cxx,v 1.3 1999/02/09 20:03:42 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.cxx,v $
 * Revision 1.3  1999/02/09 20:03:42  fisyak
 * Import new Torre staff
 *
 * Revision 1.2  1999/02/09 20:02:44  fisyak
 * Import new Torre staff
 *
 * Revision 1.3  1999/03/23 21:51:03  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:42  wenaus
 * version with constructors for table-based loading
#include "StGlobalTrack.h"
#include "StTrackCollection.h"
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
static const Char_t rcsid[] = "$Id: StFtpcHit.cxx,v 1.3 1999/02/09 20:03:42 fisyak Exp $";
#include "dst_point.h"
#ifdef __ROOT__
 * Changed method names xxxInCluster to xxxInHit
static const Char_t rcsid[] = "$Id: StFtpcHit.cxx,v 1.3 1999/02/09 20:03:42 fisyak Exp $";
#endif
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
StCollectionImp(FtpcHit)
 * Revision 2.2  1999/11/04 21:40:49  ullrich
	             const StThreeVectorF& e,
StVecPtrGlobalTrack StFtpcHit::relatedTracks(const StTrackCollection& c)
StFtpcHit::clone() { return new StFtpcHit(*this); }

StVecPtrGlobalTrack StFtpcHit::relatedTracks(const StTrackCollection* c)
{
#if 0    
    for (iter = c.begin(); iter != c.end(); iter++) {
    StGlobalTrack          *track;
	const StVecPtrFtpcHit &hits = track->ftpcHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	const StVecPtrFtpcHit &hits = track->ftpcHits();
	//	if (find(hits->begin(), hits->end(), this) != hits->end())
#endif
	if (hits.FindObject(this))
	    result.push_back(track);
    }
    return result;
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
