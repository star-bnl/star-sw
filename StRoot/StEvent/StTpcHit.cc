/***************************************************************************
 *
 * $Id: StTpcHit.cc,v 1.1 1999/01/15 20:40:08 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cc,v $
 * Revision 1.1  1999/01/15 20:40:08  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:53:58  wenaus
#include "StTpcHit.hh"
#include "StGlobalTrack.hh"
#include "StTrackCollection.hh"
#include "StEvent/StTrackCollection.hh"

static const char rcsid[] = "$Id: StTpcHit.cc,v 1.1 1999/01/15 20:40:08 wenaus Exp $";

StTpcHit::StTpcHit(const StThreeVector<float>& p,
	           const StThreeVector<float>& e,
	           float q, unsigned char c)  : StHit(p, e, q, c)
{ /* noop */ }

StVecPtrGlobalTrack StTpcHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrTpcHit &hits = track->tpcHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
    }
    return result;
}
