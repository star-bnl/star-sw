/***************************************************************************
 *
 * $Id: countPrimaryPions.cc,v 2.3 2000/02/27 20:50:39 ogilvie Exp $
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

static const char rcsid[] = "$Id: countPrimaryPions.cc,v 2.3 2000/02/27 20:50:39 ogilvie Exp $";

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
      
       StTpcDedxPidAlgorithm tpcDedxAlgorithm;
       //
       // now apply pid algorithm tpcDedxAlgorithm, which finds the tpc dedx
       // object in the collection of pidTraits, and initializes
       // the data members of the algorithm
       // which can then be accessed through member functions
       // of the algorithm e.g. numberOfSigma
       // guess is currently not used
       //
       const StParticleDefinition *guess = track->pidTraits(tpcDedxAlgorithm);
       double deviant = tpcDedxAlgorithm.numberOfSigma(particle); 
       if (fabs(deviant) < 3) counter++;
	       // counts for both charge signs of particle          
    }
    return counter;
}


