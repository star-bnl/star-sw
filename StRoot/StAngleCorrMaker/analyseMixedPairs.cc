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

static const char rcsid[] = "$Id: analyseMixedPairs.cc,v 1.3 1999/08/25 14:19:35 ogilvie Exp $";

void StAngleCorrMaker::analyseMixedPairs()
{
  cout << "in pair mixer" << endl;
  StTrackForPool *trackfrompool;
  trackfrompool = (StTrackForPool* ) mCollectionOfTracksA->Last();
  Int_t poolCounterA = mCollectionOfTracksA->IndexOf(trackfrompool);
  cout << "tracks in pool A" << poolCounterA+1 << endl;

  trackfrompool = (StTrackForPool* ) mCollectionOfTracksB->Last();
  Int_t poolCounterB = mCollectionOfTracksB->IndexOf(trackfrompool);
  cout << "tracks in pool B" << poolCounterB+1 << endl;

  trackfrompool = (StTrackForPool* ) mCollectionOfHighestPt->Last();
  Int_t poolCounterHighestPt = mCollectionOfHighestPt->IndexOf(trackfrompool);
  cout << "tracks in highest pt pool " << poolCounterHighestPt+1 << endl;

  // find random pairs in pool A and B

  StAngleDiff *anglediff = new StAngleDiff ;
  int loop = 0;
  //
  // estimate number of mixed pairs from likely number of random combinations
  //
  double aveTracksAPerEvent = 
    float(( poolCounterA + 1)) / float( mNumberEventsInPool);
  double aveTracksBPerEvent = 
    float(( poolCounterB + 1)) / float( mNumberEventsInPool);
  int numPossiblePairs = (poolCounterA - aveTracksAPerEvent)*
    (poolCounterB - aveTracksBPerEvent);
  //
  // reduce  by factor of 10 to avoid superstatistical correlations
  //
  int numMixedPairs = numPossiblePairs/10.;
  cout << "attempting to find " << numMixedPairs << " mixed pairs" << endl;

  while (loop < numMixedPairs ){
  
    StTrackForPool *trackfrompool1;
    StTrackForPool *trackfrompool2;
    Float_t r1 = gRandom->Rndm();
    int randPosition1 = r1*(poolCounterA+1);  
    trackfrompool1 = (StTrackForPool*) mCollectionOfTracksA->At(randPosition1);
    Float_t r2 = gRandom->Rndm();
    int randPosition2 = r2*(poolCounterB+1);  
    trackfrompool2 = (StTrackForPool*) mCollectionOfTracksB->At(randPosition2);
    // 
    // check that tracks come from different events
    //
    if ((trackfrompool1->mEventNumber) != (trackfrompool2->mEventNumber)) {  
      loop++;
      StThreeVectorD mom1 = trackfrompool1->mMomentum  ;
      StThreeVectorD mom2 = trackfrompool2->mMomentum  ;
      double diff = anglediff->alphaDiff(mom1,mom2);
      double weight = anglediff->weightAlpha(diff);
      // convert radians to degrees
      //
      diff = diff/degree; 
      // fill histrograms here
      mPhiDenominatorPtThresh->Fill(float(diff),weight);
    }
  }
  // loop over all tracks in highest pt pool

  for (int i = 0; i!= poolCounterHighestPt; i++) {
    StTrackForPool *trackfrompool1;
    trackfrompool1 = (StTrackForPool* ) mCollectionOfHighestPt->At(i);
    StThreeVectorD mom1 = trackfrompool1->mMomentum  ; 
    for (int j = 0; j!= poolCounterB; j++) {
      StTrackForPool *trackfrompool2;
      Float_t r2 = gRandom->Rndm();
      int randPosition2 = r2*(poolCounterB+1);  
      trackfrompool2=(StTrackForPool*) mCollectionOfTracksB->At(randPosition2);
    // 
    // check that tracks come from different events
    //
      if ((trackfrompool1->mEventNumber) != (trackfrompool2->mEventNumber)) {  
	StThreeVectorD mom2 = trackfrompool2->mMomentum  ;
	double diff = anglediff->alphaDiff(mom1,mom2);
	double weight = anglediff->weightAlpha(diff);
      // convert radians to degrees
      //
	diff = diff/degree; 
      // fill histrograms here
	mPhiDenominatorPtHigh->Fill(float(diff),weight);
      }
    }
  }
  return ;
}
