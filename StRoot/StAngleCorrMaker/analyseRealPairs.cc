#include "StAngleCorrMaker.h"
#include "StEvent/StEvent.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StAngleDiff.hh"
#include "StTrackForPool.hh"
#include "StThreeVector.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TRandom.h"
#include <TOrdCollection.h>

static const char rcsid[] = "$Id: analyseRealPairs.cc,v 1.1 1999/04/28 15:13:25 ogilvie Exp $";

void StAngleCorrMaker::analyseRealPairs(StEvent& event,int eventNumber)
{
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
	StThreeVector<double> mom1 = track1->helix().momentum(bField);      //
      // put track into pool
      //
         trackforpool->setTrackForPool(mom1,eventNumber) ;
         mCollectionOfTracks->AddLast(trackforpool); 
	 //
	 // find pairs in this event
	 //
	 for (iter2 = iter1+1;
	      iter2 != tracks->end(); iter2++) {
	     track2 = *iter2;
	     vertex2 = track2->startVertex();
	     if (vertex2 &&
		 vertex2->type() == primary) {

		 StThreeVector<double> mom2 = track2->helix().momentum(bField);
	         double diff = anglediff->phiDiff(mom1,mom2);
                 double weight = anglediff->weightPhiDiff(mom1,mom2);
		 // convert radians to degrees
		 //
                 diff = diff/degree; 
	       // fill histrograms here
		   mHistPhiNumerator->Fill(float(diff),weight);
	       counter++;
	     }	   
      }
    }
  }

  StTrackForPool *trackfrompool;
  trackfrompool = (StTrackForPool* ) mCollectionOfTracks->Last();
  Int_t poolCounter = mCollectionOfTracks->IndexOf(trackfrompool);

  // quick test for information
  cout << "event number test " <<  trackfrompool->mEventNumber << endl;
  
  cout << "tracks in pool " << poolCounter+1 << endl;

  Float_t r = gRandom->Rndm();
  cout << "random " << r << endl ;

  return ;
}
