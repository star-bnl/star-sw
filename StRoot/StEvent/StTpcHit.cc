/***************************************************************************
 *
 * $Id: StTpcHit.cc,v 1.2 1999/01/15 22:53:58 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cc,v $
 * Revision 1.2  1999/01/15 22:53:58  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:53:58  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StTpcHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"

static const char rcsid[] = "$Id: StTpcHit.cc,v 1.2 1999/01/15 22:53:58 wenaus Exp $";

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
