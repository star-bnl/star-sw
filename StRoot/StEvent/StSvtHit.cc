/***************************************************************************
 *
 * $Id: StSvtHit.cc,v 1.1 1999/01/15 20:40:02 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cc,v $
 * Revision 1.1  1999/01/15 20:40:02  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:53:54  wenaus
#include "StSvtHit.hh"
#include "StGlobalTrack.hh"
#include "StTrackCollection.hh"
#include "StEvent/StTrackCollection.hh"

static const char rcsid[] = "$Id: StSvtHit.cc,v 1.1 1999/01/15 20:40:02 wenaus Exp $";

StSvtHit::StSvtHit(const StThreeVector<float>& p,
	           const StThreeVector<float>& e,
	           float q, unsigned char c)
    : StHit(p, e, q, c)
{ /* noop */ }

StVecPtrGlobalTrack StSvtHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrSvtHit &hits = track->svtHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
    }
    return result;
}
