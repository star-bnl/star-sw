#include "StEvent/StEvent.hh"
#include "StEventTypes.h"

static const char rcsid[] = "$Id: countPrimaryTracks.cc,v 1.1 1999/02/10 23:59:53 wenaus Exp $";
  // First, we have to establish a primary vertex.
    //  in many different ways. Here we demonstrate
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
  cout << "Primary vertex: " <<
    " index= " << primaryV->index() <<
    " nDaughters= " << primaryV->daughters().size() <<
    endl;

  // Thomas's Example 1
  long counter = 0;
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
  return counter;
	     << "tracks from different methods." << endl;

    return counter2;
}
