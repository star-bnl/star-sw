/***************************************************************************
 *
 * $Id: countPrimaryTracks.cc,v 2.0 1999/11/04 16:10:08 ullrich Exp $
 *
 * Author: Torre Wenaus, BNL,
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  This is an example of a function which performs
 *               some simple analysis using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: countPrimaryTracks.cc,v $
 * Revision 2.0  1999/11/04 16:10:08  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#include "StEventTypes.h"

static const char rcsid[] = "$Id: countPrimaryTracks.cc,v 2.0 1999/11/04 16:10:08 ullrich Exp $";

long countPrimaryTracks(StEvent& event)
{
    //
    //  The number of primary tracks can be obtained
    //  in many different ways. Here we demonstrate
    //  two of them.
    //

    // ******************** Method I *********************
    
    //  All primary tracks are stored/referenced from the
    //  primary vertex. Although not currently implemented
    //  there might be more than one event vertex. 
    //
    long nvtx = event.numberOfPrimaryVertices();

    //
    //  Now we loop over all event vertices and add up the
    //  number of 'primary' tracks.
    //
    long counter1 = 0;
    for (int k=0; k<nvtx; k++) {
	StPrimaryVertex *vtx = event.primaryVertex(k);
	if (vtx) counter1 += vtx->numberOfDaughters();
	// or shorter:
	// counter1 += event.primaryVertex(k)->numberOfDaughters();
    };

    //
    //  That's it. counter1 is the what we have to return and
    //  we are done. There's however another method which
    //  should give us the same result.
    //
    
    // ******************** Method II *********************

    //
    //  We scan the track nodes and count the number of primary
    //  tracks referenced therein. Not the fastest way but that's
    //  not the point here.
    //  Note that 'primary' is defined in StEnumerations.h
    //  
    StSPtrVecTrackNode& theNodes = event.trackNodes();

    long counter2 = 0;
    for (unsigned int i=0; i<theNodes.size(); i++) {
	counter2 += theNodes[i]->entries(primary);   
    }

    //
    //  At last, a check if both are equal (they better be)
    //  and we are done.
    //
    if (counter1 != counter2)
	cerr << "countPrimaryTracks(): strange, different # of primary "
	     << "tracks from different methods." << endl;

    return counter2;
}
