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

static const char rcsid[] = "$Id: analyseMixedPairs.cc,v 1.2 1999/06/27 22:45:24 fisyak Exp $";

void StAngleCorrMaker::analyseMixedPairs()
{
  cout << "in pair mixer" << endl;
  StTrackForPool *trackfrompool;
  trackfrompool = (StTrackForPool* ) mCollectionOfTracks->Last();
  Int_t poolCounter = mCollectionOfTracks->IndexOf(trackfrompool);
  cout << "tracks in pool " << poolCounter+1 << endl;

  // find random pairs in pool

  StAngleDiff *anglediff = new StAngleDiff ;
  int loop = 0;
  //
  // estimate number of mixed pairs from likely number of random combinations
  //
  double aveTracksPerEvent = 
    float(( poolCounter + 1)) / float( mNumberEventsInPool);
  int numPossiblePairs = pow((poolCounter - aveTracksPerEvent),2);
  //
  // reduce  by factor of 100 to avoid superstatistical correlations
  //
  int numMixedPairs = numPossiblePairs/100.;
  cout << "attempting to find " << numMixedPairs << " mixed pairs" << endl;

  while (loop < numMixedPairs ){
    StTrackForPool *trackfrompool1;
    StTrackForPool *trackfrompool2;
    Float_t r1 = gRandom->Rndm();
    int randPosition1 = r1*(poolCounter+1);  
    trackfrompool1 = (StTrackForPool* ) mCollectionOfTracks->At(randPosition1);
    Float_t r2 = gRandom->Rndm();
    int randPosition2 = r2*(poolCounter+1);  
    trackfrompool2 = (StTrackForPool* ) mCollectionOfTracks->At(randPosition2);
    // 
    // check that tracks come from different events
    //
    if ((trackfrompool1->mEventNumber) != (trackfrompool2->mEventNumber)) {  
      loop++;
      StThreeVectorD mom1 = trackfrompool1->mMomentum  ;
      StThreeVectorD mom2 = trackfrompool2->mMomentum  ;
      double diff = anglediff->phiDiff(mom1,mom2);
      double weight = anglediff->weightPhiDiff(mom1,mom2);
      // convert radians to degrees
      //
      diff = diff/degree; 
      // fill histrograms here
      mHistPhiDenominator->Fill(float(diff),weight);
    }
  }
  return ;
}
