#include "StAngleCorrMaker.h"
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StAngleDiff.hh"
#include "StTrackForPool.hh"
#include "StThreeVectorD.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TRandom.h"
#include <TOrdCollection.h>

static const char rcsid[] = "$Id: analyseRealPairs.cc,v 1.3 1999/06/27 22:45:25 fisyak Exp $";

void StAngleCorrMaker::analyseRealPairs(StEvent& event,int eventNumber)
{
  cout << "in real pairs " << endl;
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
      // cut on quality of tracks
      // 

      if (track1->tpcHits().size() < 20) continue;
        float degreesOfFreedom = track1->fitTraits().degreesOfFreedom();
        if ((track1->fitTraits().chiSquaredInXY()/degreesOfFreedom > 1.5 ) ||
              (track1->fitTraits().chiSquaredInPlaneZ()/degreesOfFreedom > 1.5)) 
	       continue;

        StTrackForPool *trackforpool = new StTrackForPool; 
	StThreeVectorD mom1 = track1->helix().momentum(bField);      //
	if (mom1.perp() > 1.5*GeV ) {
      // put track into pool
      //
         trackforpool->setTrackForPool(mom1,eventNumber) ;
         if (mNumberTracksInPool > 0) { 
	   mCollectionOfTracks->AddLast(trackforpool); }
	 else {
	   mCollectionOfTracks->AddFirst(trackforpool); }
	 mNumberTracksInPool++;	   
	  
	 //
	 // find pairs in this event
	 //
	 for (iter2 = iter1, iter2++;
	      iter2 != tracks->end(); iter2++) {
	     track2 = *iter2;
	     vertex2 = track2->startVertex();
	     if (vertex2 &&
		 vertex2->type() == primary) {
	       if (track2->tpcHits().size() < 20) continue;
	       float degreesOfFreedom = track2->fitTraits().degreesOfFreedom();
	       if ((track2->fitTraits().chiSquaredInXY()/degreesOfFreedom > 1.5 ) ||
                (track2->fitTraits().chiSquaredInPlaneZ()/degreesOfFreedom > 1.5)) 
	       continue;
	       StThreeVectorD mom2 = track2->helix().momentum(bField);
	       if (mom2.perp() > 1.5*GeV ) {
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
    }
  }
  StTrackForPool *trackFromPool;
  trackFromPool = (StTrackForPool* ) mCollectionOfTracks->Last();
  Int_t poolCounter = mCollectionOfTracks->IndexOf(trackFromPool);

  if ((poolCounter+1) > 1 ) {
  // quick test for information
  cout << "event number test " <<  trackFromPool->mEventNumber << endl;
  }
  cout << "tracks in pool " << poolCounter+1 << endl;

  Float_t r = gRandom->Rndm();
  cout << "random " << r << endl ;

  return ;
}
