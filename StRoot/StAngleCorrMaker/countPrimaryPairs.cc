// $Id: countPrimaryPairs.cc,v 1.3 1999/06/27 22:45:25 fisyak Exp $
// $Log: countPrimaryPairs.cc,v $
// Revision 1.3  1999/06/27 22:45:25  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.2  1999/04/28 15:14:50  ogilvie
// included filling of histogram for pairs of tracks in real events
// i.e. numerator of correlation function
//
// Revision 1.5  1999/03/30 20:37:49  wenaus
// Explicit StGlobalTrack include
//
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
// countPrimarPairs.cc
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
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StAngleDiff.hh"
#include "StTrackForPool.hh"
#include "StThreeVectorD.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TRandom.h"
#include <TOrdCollection.h>

static const char rcsid[] = "$Id: countPrimaryPairs.cc,v 1.3 1999/06/27 22:45:25 fisyak Exp $";

long countPrimaryPairs(StEvent& event,int eventNumber, TOrdCollection* trackPool)
{
  // temporary, replace with a get
  //  long eventNumber = 1; 
  //
  long counter = 0;
  // First, we have to establish a primary vertex.

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
    }
  }
  event.setPrimaryVertex(vtxMax);

  StVertex *primaryV = event.primaryVertex();
  if ( primaryV ) {
    cout << "Primary vertex: " <<
      " index= " << primaryV->index() <<
      " nDaughters= " << primaryV->daughters().size() <<
      endl;
  } else {
    cout << "No primary vertex" << endl;
  }
  

  StTrackCollection *tracks = event.trackCollection();
  StTrackIterator iter1;
  StTrackIterator iter2;
  StGlobalTrack *track1;
  StGlobalTrack *track2;

  StVertex *vertex1;
  StVertex *vertex2;
  StAngleDiff *anglediff = new StAngleDiff ;

  //
  // requires bfield, see Thomas for this !!!
  //  
  const double  bField = 0.5*tesla;

  for (iter1 = tracks->begin();
       iter1 != tracks->end(); iter1++) {
    track1 = *iter1;
    vertex1 = track1->startVertex();
    if (vertex1 &&
        vertex1->type() == primary) {
        StTrackForPool *trackforpool = new StTrackForPool; 
	StThreeVectorD mom1 = track1->helix().momentum(bField);      //
      // put track into pool
      //

         trackforpool->setTrackForPool(mom1,eventNumber) ;
         trackPool->AddLast(trackforpool); 
	 //
	 // find pairs in this event
	 //
	 for (iter2 = iter1, iter2++;
	      iter2 != tracks->end(); iter2++) {
	     track2 = *iter2;
	     vertex2 = track2->startVertex();
	     if (vertex2 &&
		 vertex2->type() == primary) {

		 StThreeVectorD mom2 = track2->helix().momentum(bField);
	         double diff = anglediff->alphaDiff(mom1,mom2);

	       // fill histrograms here

	       counter++;
	     }	   
      }
    }
  }

  StTrackForPool *trackfrompool;
  //  TObject *trackfrompool = trackPool->Last();
  trackfrompool = (StTrackForPool* ) trackPool->Last();
  Int_t poolCounter = trackPool->IndexOf(trackfrompool);

  // quick test for information
  cout << "event number test " <<  trackfrompool->mEventNumber << endl;

  //  long poolCounter= 0;	   
  //  check that the pool has entries
  //  for (iterpool = trackPool->begin();
  //   iterpool != trackpool.end(); iterpool++) {
  //  poolCounter++;
  // }
  
  cout << "tracks in pool " << poolCounter+1 << endl;

  Float_t r = gRandom->Rndm();
  cout << "random " << r << endl ;

  return counter;
}
