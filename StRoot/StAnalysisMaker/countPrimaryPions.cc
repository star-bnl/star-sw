/***************************************************************************
 *
 * $Id: countPrimaryPions.cc,v 2.2 2000/01/25 03:11:58 ogilvie Exp $
 *
 * Author: Craig Ogilvie, MIT, Dec 199
 ***************************************************************************
 *
 * Description:  Demonstrates a simple use of PID information in StEvent
 *
 ***************************************************************************

 **************************************************************************/
#include "StEventTypes.h"
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StTpcDedxPidAlgorithm.h"

static const char rcsid[] = "$Id: countPrimaryPions.cc,v 2.2 2000/01/25 03:11:58 ogilvie Exp $";

long countPrimaryPions(StEvent& event)
{
    long counter = 0;
    StParticleDefinition* particle;  
    particle =  StParticleTable::instance()->findParticle("pi+") ;

    // return if no primary vertex
    if (event.primaryVertex()==0) {
       return counter;
    }
    const StSPtrVecPrimaryTrack& tracks = event.primaryVertex()->daughters();
    // return if no primary tracks
    if (tracks.size() < 0) {
      return counter;
    }

    // collection of primary tracks
    StPrimaryTrackIterator iter;
    StPrimaryTrack *track;

    for (iter = tracks.begin();
       iter != tracks.end(); iter++) {
       track = *iter;
       if (track==0) continue;
      
       StTpcDedxPidAlgorithm tpcDedx;
       StDedxPidTraits* dedxPidTr;
       //
       // now get collection of pid traits of track, dedx, tof etc.
       //
       StSPtrVecTrackPidTraits& traits=track->pidTraits();

       for (int itrait = 0; itrait < traits.size(); itrait++){
           dedxPidTr = 0;
	   if (traits[itrait]->detector() == kTpcId) {
	     //
	     // tpc pid trait
	     //
             StTrackPidTraits* thisTrait = traits[itrait];
	     //
	     // perform cast to make the pid trait a dedx trait
	     //
	     dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
	   }
	   // container of traits contains several possible de/dx methods
           if (dedxPidTr &&  dedxPidTr->method() == kTruncatedMeanId) {
	     // next line is necessary to allow tpcDedx functions
	       const StParticleDefinition *guess = track->pidTraits(tpcDedx);
	       double deviant = tpcDedx.numberOfSigma(particle); 
	       if (fabs(deviant) < 3) counter++;
	       // counts for both charge signs of particle
	   }
       }  
    }
    return counter;
}


