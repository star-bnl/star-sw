#include "StAngleCorrMaker.h"
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StEvent/StTpcDedxPid.h"
#include "StAngleDiff.hh"
#include "StTrackForPool.hh"
#include "StThreeVectorD.hh"

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TRandom.h"
#include <TOrdCollection.h>

static const char rcsid[] = "$Id: analyseRealPairs.cc,v 1.4 1999/08/25 14:19:35 ogilvie Exp $";

void StAngleCorrMaker::analyseRealPairs(StEvent& event,int eventNumber)
{
  cout << "in real pairs " << endl;
  long counter = 0;

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
  
  double pperpMax = 0;
  StTrackIterator iterMax ;

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

      int numHits1 = track1->numberOfTpcHits();    
      if (numHits1 < 15) continue;

        float degreesOfFreedom = track1->fitTraits().degreesOfFreedom();
        if ((track1->fitTraits().chiSquaredInXY()/degreesOfFreedom > 1.5 ) ||
              (track1->fitTraits().chiSquaredInPlaneZ()/degreesOfFreedom > 1.5)) 
	       continue;

        StTrackForPool *trackforpool = new StTrackForPool; 
	StThreeVectorD mom1 = track1->helix().momentum(bField);      //


      // put track into poolA
      //
	if (mom1.perp() > 4.0*GeV ) {
          trackforpool->setTrackForPool(mom1,eventNumber) ; 
	  mCollectionOfTracksA->AddLast(trackforpool); 
	}
      // put track into poolB
      //
	if (mom1.perp() < 2.5*GeV && mom1.perp() > 2.0*GeV ) {
          trackforpool->setTrackForPool(mom1,eventNumber) ; 
	  mCollectionOfTracksB->AddLast(trackforpool); 
	}
      // check if largest in event
        if (mom1.perp() > pperpMax) {
	    iterMax = iter1; 
	    pperpMax = mom1.perp() ;
	}	
      // form numerator pairs
      //
	if (mom1.perp() > 4.0*GeV ) {
	 //
	 // find pairs in this event
	 //
	  for (iter2 = tracks->begin();
	      iter2 != tracks->end(); iter2++) {
	     track2 = *iter2;
	     vertex2 = track2->startVertex();
	     if (vertex2 &&
		 vertex2->type() == primary) {
	       int numHits2 = track2->numberOfTpcHits();    
	       if (numHits2 < 15) continue;
	       float degreesOfFreedom = track2->fitTraits().degreesOfFreedom();
	       if ((track2->fitTraits().chiSquaredInXY()/degreesOfFreedom > 1.5 ) ||
                (track2->fitTraits().chiSquaredInPlaneZ()/degreesOfFreedom > 1.5)) 
	       continue;
	       StThreeVectorD mom2 = track2->helix().momentum(bField);
	       if (mom2.perp() < 2.5*GeV && mom2.perp() > 2.0*GeV) {
	         double diff = anglediff->alphaDiff(mom1,mom2);
                 double weight = anglediff->weightAlpha(diff);
		 // convert radians to degrees
		 //
                 diff = diff/degree; 
	       // fill histrograms here
		   mPhiNumeratorPtThresh->Fill(float(diff),weight);
	       counter++;
	       }
	     }	
	  }   
	}  
    }
  }

  StTrackForPool *trackFromPool;
  trackFromPool = (StTrackForPool* ) mCollectionOfTracksA->Last();
  Int_t poolCounterA = mCollectionOfTracksA->IndexOf(trackFromPool);
  cout << "tracks in poolA " << poolCounterA+1 << endl;
  trackFromPool = (StTrackForPool* ) mCollectionOfTracksB->Last();
  Int_t poolCounterB = mCollectionOfTracksB->IndexOf(trackFromPool);
  cout << "tracks in poolB " << poolCounterB+1 << endl;

  //
  // fill histogram of angle diff wrt largest pt track
  //

  if (pperpMax > 0. ){
      StTrackForPool *trackforpool = new StTrackForPool; 
      track1 = *iterMax;
      StThreeVectorD momMax = track1->helix().momentum(bField);      
	cout << momMax.perp() << " maximum transverse momentum" << endl;
      // put track into pool
      //
      trackforpool->setTrackForPool(momMax,eventNumber) ; 
      mCollectionOfHighestPt->AddLast(trackforpool);
	 // loop through all tracks again and fill histograms 
      for (iter1 = tracks->begin();
	      iter1 != tracks->end(); iter1++) {
	// don't use maximum pperp track!!
	if (iter1 != iterMax) {
	 track1 = *iter1;
	 vertex1 = track1->startVertex();
	 if (vertex1 &&
	   vertex1->type() == primary) {
      // cut on quality of tracks
      // 
          int numHits1 = track1->numberOfTpcHits();    
          if (numHits1 < 15) continue;
	  float degreesOfFreedom = track1->fitTraits().degreesOfFreedom();
	  if ((track1->fitTraits().chiSquaredInXY()/degreesOfFreedom > 1.5 ) ||
	     (track1->fitTraits().chiSquaredInPlaneZ()/degreesOfFreedom > 1.5)) 
	      continue;
	  StThreeVectorD mom1 = track1->helix().momentum(bField);
	  if (mom1.perp() > 1.5*GeV ) {
	      double diff = anglediff->alphaDiff(mom1,momMax);
              double weight = anglediff->weightAlpha(diff);
		 // convert radians to degrees
		 //
              diff = diff/degree; 
	       // fill histrograms here
	      mPhiNumeratorPtHigh->Fill(float(diff),weight);
	  }
	 }
	}
      }
  }  
  return ;
}




