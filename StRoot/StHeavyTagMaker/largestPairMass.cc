//
// $Id: largestPairMass.cc,v 1.2 2004/07/29 23:06:13 calderon Exp $
//
// Author: Manuel Calderon de la Barca Sanchez
//
// Calculates the largest invariant mass of track pairs
// from StEvent primary tracks.
// 
// For use in the creation of the Heavy Flavor Tags
// Track cuts are:
// flag>0
// tpc fit points >=15
// |eta|<1.5
// p>1 GeV/c
//
// $Log: largestPairMass.cc,v $
// Revision 1.2  2004/07/29 23:06:13  calderon
// Changed adc cut to match towers to 360 ADC counts,
// and documented the origin.
// Added Description to cxx file.
// Removed unnecessary static_cast for StDetectorId
//
//

#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StContainers.h"
#include "StPrimaryTrack.h"
#include "StTrackGeometry.h"
#include "StTrackFitTraits.h"

#include "StLorentzVectorF.hh"

float largestPairMass(StEvent* event) {

    // first, protect against funny business..
    if (!event) return -9999;
    if (!(event->primaryVertex())) return -9999;

    
    // Set up the parameters we'll need
    // This just takes one constant, the
    // mass of the electron in GeV/c^2
    //
    const float eMass = 0.00051099906; // 

    // Set the initial Invariant Mass Seed:
    float mostGargantuanMass = 0.0;
    
    // Get primary track container
    const StSPtrVecPrimaryTrack& trackArray = event->primaryVertex()->daughters();    

    // Construct pairs of tracks:
    //
    // 
    for (unsigned int ipr1=0; ipr1<trackArray.size(); ++ipr1) {
	StPrimaryTrack* const ptrack1 = trackArray[ipr1];

	// check track 1 for cuts:
	if (!ptrack1) continue; // valid pointer
	if (ptrack1->flag()<=0) continue; // valid flag
	if (ptrack1->fitTraits().numberOfFitPoints(kTpcId)<15) continue; // enough fit points
	StThreeVectorF mom1 = ptrack1->geometry()->momentum();
	if (mom1.mag()<1.) continue; //use tracks with p>1
	if (fabs(mom1.pseudoRapidity())>1.5) continue; // use tracks with |eta|<1.5

	// at this point, track passed cuts,
	// make pairs with the rest of the
	// tracks in the container
	// Note: loop MUST start with ipr1+1 to avoid double counting
	//
	for (unsigned int ipr2=ipr1+1; ipr2<trackArray.size(); ++ipr2) {
	    StPrimaryTrack* const ptrack2 = trackArray[ipr2];

	    // same as for track 1, check track 2 for cuts:
	    if (!ptrack2) continue;
	    if (ptrack2->flag()<=0) continue;
	    if (ptrack2->fitTraits().numberOfFitPoints(kTpcId)<15) continue;
	    StThreeVectorF mom2 = ptrack2->geometry()->momentum();
	    if (mom2.mag()<1.) continue;
	    if (fabs(mom2.pseudoRapidity())>1.5) continue;

	    // Ok, at this point we have a pair.
	    // There is no further selection on the charge.
	    // Now we calculate the invariant mass.
	    // This takes 3 lines of code using Lorentz Vectors
	    StLorentzVectorF fmom1(mom1,mom1.massHypothesis(eMass));
	    StLorentzVectorF fmom2(mom2,mom2.massHypothesis(eMass));	    
	    float mass = (fmom1 + fmom2).m();
	    if (mass>mostGargantuanMass) {
		mostGargantuanMass = mass;
	    }
	}// 2nd track loop
    }// first track loop
    return mostGargantuanMass;
}
