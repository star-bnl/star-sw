/***************************************************************************
 *
 * $Id: countPrimaryPions.cc,v 2.4 2000/03/02 13:13:19 ullrich Exp $
 *
 * Author: Craig Ogilvie, MIT, Dec 199
 ***************************************************************************
 *
 * Description:  Demonstrates a simple use of PID information in StEvent
 *
 ***************************************************************************
 *
 * $Log: countPrimaryPions.cc,v $
 * Revision 2.4  2000/03/02 13:13:19  ullrich
 * Modified such that code compiles on Linux without warnings.
 *
 **************************************************************************/
#include "StEventTypes.h"    // includes all basic StEvent classes
#include "StPionPlus.hh"
#include "StTpcDedxPidAlgorithm.h"

static const char rcsid[] = "$Id: countPrimaryPions.cc,v 2.4 2000/03/02 13:13:19 ullrich Exp $";

long countPrimaryPions(StEvent& event)
{
    long counter = 0;

    // return if no primary vertex
    if (!event.primaryVertex()) return counter;

    const StSPtrVecPrimaryTrack& tracks = event.primaryVertex()->daughters();
    
    // return if no primary tracks
    if (!tracks.size()) return counter;

    // collection of primary tracks
    StPrimaryTrack         *track;

    for (StPrimaryTrackIterator iter = tracks.begin();
	 iter != tracks.end(); iter++) {

	track = *iter;
	if (track==0) continue;
      
	StTpcDedxPidAlgorithm tpcDedxAlgorithm;

	//
	// Now apply pid algorithm tpcDedxAlgorithm, which finds the tpc dedx
	// object in the collection of pidTraits, and initializes
	// the data members of the algorithm
	// which can then be accessed through member functions
	// of the algorithm e.g. numberOfSigma
	// guess is currently not used
	//
	track->pidTraits(tpcDedxAlgorithm);	// ignore return value
	double deviant = tpcDedxAlgorithm.numberOfSigma(StPionPlus::instance()); 
	if (fabs(deviant) < 3) counter++;
	// counts for both charge signs of particle          
    }
    return counter;
}


