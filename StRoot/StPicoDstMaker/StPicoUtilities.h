/**
 * \brief Utility that performs calculations
 *
 * The StPicoUtilities allows one to calculate some usefule
 * quantities on the flight and then store them in PicoDst
 */

#ifndef StPicoUtilities_h
#define StPicoUtilities_h

// C++ headers
#include <array>
#include <string>
#include <cmath>

// MuDst headers
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

//_________________
namespace StPicoUtilities {

  enum Charge_t  { Neg=0, Pos=1 };
  enum TpcHalf_t { East=0, West=2 };
  enum Mult_t    { refMult2=0, refMult3=4, refMult4=8, refMultHalf=12 };

  enum RefMult_t {
     RefMult2NegEast    = refMult2|Neg|East,
     RefMult2NegWest    = refMult2|Neg|West,
     RefMult2PosEast    = refMult2|Pos|East,
     RefMult2PosWest    = refMult2|Pos|West,
     RefMult3NegEast    = refMult3|Neg|East,
     RefMult3NegWest    = refMult3|Neg|West,
     RefMult3PosEast    = refMult3|Pos|East,
     RefMult3PosWest    = refMult3|Pos|West,
     RefMult4NegEast    = refMult4|Neg|East,
     RefMult4NegWest    = refMult4|Neg|West,
     RefMult4PosEast    = refMult4|Pos|East,
     RefMult4PosWest    = refMult4|Pos|West,
     RefMultHalfNegEast = refMultHalf|Neg|East,
     RefMultHalfNegWest = refMultHalf|Neg|West,
     RefMultHalfPosEast = refMultHalf|Pos|East,
     RefMultHalfPosWest = refMultHalf|Pos|West
  };

  std::array<int, 16> calculateRefMult(const StMuDst& muDst) {

    std::array<int, 16> custom_refMult = {};

    // Loop over all primary tracks
    for (Int_t iTrk = 0; iTrk < muDst.primaryTracks()->GetEntries(); ++iTrk) {

      // Retrieve track
      StMuTrack* track = muDst.primaryTracks(iTrk);

      // Track must exist
      if (!track) continue;

      // These first 3 checks are used for all refMult
      if( track->flag() < 0 ||
	  fabs(track->momentum().mag()) < 1.e-10 ||
	  track->dca().mag() > 3. ||
	  fabs(track->momentum().pseudoRapidity()) > 1. ) continue;

      double const eta = track->momentum().pseudoRapidity() ;
      Charge_t  chargeName = (track->charge() > 0) ? Pos : Neg;
      TpcHalf_t tpcHalfName = (eta > 0) ? West : East;
      
      double const beta = track->btofPidTraits().beta();
      double const massSqr = (beta <= 1.e-5) ? -999. : track->momentum().mag2() * (std::pow(1. / beta, 2) - 1.);

      // Define refMultHalf, refMult2 and refMult3
      if (track->nHitsFit(kTpcId) >= 10) {
	
        // refMultHalf definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) < 1
        custom_refMult[refMultHalf | chargeName | tpcHalfName] += 1;
	
        // refMult2 definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) > 0.5 && abs(eta) < 1
        if (fabs(eta) > 0.5) {
	  custom_refMult[refMult2 | chargeName | tpcHalfName] += 1;
	}
	
        // refMult3 definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) < 1 && Exclude protons
        if (track->nSigmaProton() < -3. && massSqr < 0.4) {
	  custom_refMult[refMult3 | chargeName | tpcHalfName] += 1;
	}
      } //if (track->nHitsFit(kTpcId) >= 10)

      // Define refMult4
      if (track->nHitsFit(kTpcId) >= 15) {
	
        // refMult4 definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 15 && abs(eta) < 1 && Exclude kaons
        if( (massSqr <= -990. && fabs(track->nSigmaKaon()) > 3) ||    // tof is not available
            (massSqr >  -990. && (massSqr > 0.6 || massSqr < 0.1)) ) {    // tof is available
          custom_refMult[refMult4 | chargeName | tpcHalfName] += 1;
	}
      } // if (track->nHitsFit(kTpcId) >= 15)
    } //for (Int_t iTrk = 0; iTrk < muDst.primaryTracks()->GetEntries(); ++iTrk)
    
    return custom_refMult;
    
  } //std::array<int, 16> calculateRefMult(const StMuDst& muDst)
}

#endif // #define StPicoUtilities_h
