/***************************************************************************
 *
 * $Id: StFtpcHit.cc,v 1.2 1999/01/15 22:53:42 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.cc,v $
 * Revision 1.2  1999/01/15 22:53:42  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:53:42  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StFtpcHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"

static const char rcsid[] = "$Id: StFtpcHit.cc,v 1.2 1999/01/15 22:53:42 wenaus Exp $";

StFtpcHit::StFtpcHit(const StThreeVector<float>& p,
	             const StThreeVector<float>& e,
	             float q, unsigned char c) : StHit(p, e, q, c)
{ /* noop */ }

StVecPtrGlobalTrack StFtpcHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack    result;
    StGlobalTrack          *track;
    StTrackConstIterator   iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrFtpcHit &hits = track->ftpcHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
    }
    return result;
}
