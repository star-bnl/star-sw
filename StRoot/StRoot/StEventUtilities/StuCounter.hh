/***************************************************************************
 *
 * $Id: StuCounter.hh,v 1.1 1999/12/15 15:05:20 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 * Set of functions which perform simple counting
 * tasks. All functions are inline function and one
 * only needs to include the header file to use them.
 * No need to link with any library.
 * 
 * Syntax:
 *        unsigned int numberOfTracks(StEvent& evt, StTrackType ttyp,
 *                                    unsigned int minHits = 0);
 * Returns number of tracks in event 'evt' of type 'ttyp' (global/primary)
 * with greater or equal 'minHits' hits. 'minHits' default to 0.
 * 
 * There are two related specialised functions for the two track types:
 *        unsigned int numberOfGlobalTracks(StEvent& evt, unsigned int minHits = 0);
 *        unsigned int numberOfTracks(StEvent& evt, unsigned int minHits = 0);
 * Examples:
 *        int n = numberOfTracks(evt, global);
 *        int m = numberOfGlobalTracks(evt);       // same as above
 *        int k = numberOfPrimaryTracks(evt, 10);  // primary tracks with >= 10 hits
 *        int k = numberOfTracks(evt,primary, 10); // same as above
 *
 ***************************************************************************
 *
 * $Log: StuCounter.hh,v $
 * Revision 1.1  1999/12/15 15:05:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StStuCounter_hh
#define StStuCounter_hh

#include "StEventTypes.h"

inline unsigned int
numberOfTracks(StEvent& evt, StTrackType ttyp, unsigned int minHits = 0)
{
    StSPtrVecTrackNode& nodes = evt.trackNodes();
    unsigned int sum = 0;
    unsigned int i, j, n;
    
    if (minHits) {
	for (i=0; i<nodes.size(); i++) {
	    n = nodes[i]->entries(ttyp);
	    for (j=0; j<n; j++)
		if (nodes[i]->track(ttyp, j)->detectorInfo()->numberOfPoints() >= minHits)
		    sum++;
	}
    }
    else {
	for (i=0; i<nodes.size(); i++)
	    sum += nodes[i]->entries(ttyp);
    }
    return sum;
}

inline unsigned int
numberOfGlobalTracks(StEvent& evt, unsigned int minHits = 0)
{
    return numberOfTracks(evt, global, minHits);
}

inline unsigned int
numberOfPrimaryTracks(StEvent& evt, unsigned int minHits = 0)
{
    return numberOfTracks(evt, primary, minHits);
}

#endif
