// $Id: countPrimaryTracks.cc,v 1.4 1999/03/30 15:33:43 wenaus Exp $
// $Log: countPrimaryTracks.cc,v $
// Revision 1.4  1999/03/30 15:33:43  wenaus
// eliminate obsolete branch methods
//
// Revision 1.3  1999/02/22 20:49:22  wenaus
// Protect against null primary vertex
//
// Revision 1.2  1999/02/11 15:39:15  wenaus
// cleanup
//
//
///////////////////////////////////////////////////////////////////////////////
//
// countPrimaryTracks.cc
//
// Description: 
//  Simple StEvent usage example. Based on Thomas Ullrich's Example 1
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL  2/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StEvent/StEvent.hh"
 *
static const char rcsid[] = "$Id: countPrimaryTracks.cc,v 1.4 1999/03/30 15:33:43 wenaus Exp $";
#include "StEventTypes.h"

static const char rcsid[] = "$Id: countPrimaryTracks.cc,v 1.4 1999/03/30 15:33:43 wenaus Exp $";
  long counter = 0;
  // First, we have to establish a primary vertex.
    //  in many different ways. Here we demonstrate
  /*
  // Count vertex daughters, and arbitrarily set the primary vertex
  // to be the vertex with the most daughters
  StVertexCollection* vertices = event.vertexCollection();
  StVertexIterator itr;
  StVertex *vtx = 0;
  StVertex *vtxMax = 0;
  long nMax = 0;
  for (itr = vertices->begin(); itr != vertices->end(); itr++) {
    vtx = *itr;
    if (vtx->daughters().size() > nMax) {
      nMax = vtx->daughters().size();
      vtxMax = vtx;
    long counter2 = 0;
  }
  event.setPrimaryVertex(vtxMax);
    for (unsigned int i=0; i<theNodes.size(); i++) {
  StVertex *primaryV = event.primaryVertex();
  if ( primaryV ) {
    cout << "Primary vertex: " <<
      " index= " << primaryV->index() <<
      " nDaughters= " << primaryV->daughters().size() <<
      endl;
  } else {
    cout << "No primary vertex" << endl;
  }

  // Thomas's Example 1
  StTrackCollection *tracks = event.trackCollection();
  StTrackIterator iter;
  StGlobalTrack *track;
  StVertex *vertex;
  for (iter = tracks->begin();
       iter != tracks->end(); iter++) {
    track = *iter;
    vertex = track->startVertex();
    if (vertex &&
        vertex->type() == primary)
      counter++;
  }
  */
  return counter;
	     << "tracks from different methods." << endl;

    return counter2;
}
