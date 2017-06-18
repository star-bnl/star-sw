#ifndef StPicoUtilities_h
#define StPicoUtilities_h
#include <array>
#include <string>
#include <cmath>
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

namespace StPicoUtilities
{
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


  std::array<int, 16> calculateRefMult(const StMuDst& muDst)
  {

    std::array<int, 16> custom_refMult = {};

    for (Int_t itrk = 0; itrk < muDst.primaryTracks()->GetEntries(); ++itrk)
    {
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if (!track) continue;

      // these first 3 checks are used for all refMult
      if (track->flag() < 0 || fabs(track->momentum().mag()) < 1.e-10
          || track->dca().mag() > 3 || fabs(track->momentum().pseudoRapidity()) > 1) continue;

      double const eta = track->momentum().pseudoRapidity() ;
      Charge_t  chargeName = track->charge() > 0 ? Pos : Neg;
      TpcHalf_t tpcHalfName = eta > 0 ? West : East;

      double const beta = track->btofPidTraits().beta();
      double const mass2 = beta <= 1.e-5 ? -999. : track->momentum().mag2() * (std::pow(1. / beta, 2) - 1);

      if (track->nHitsFit(kTpcId) >= 10)
      {
        // refMultHalf definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) < 1
        custom_refMult[refMultHalf | chargeName | tpcHalfName] += 1;

        // refMult2 definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) > 0.5 && abs(eta) < 1
        if (fabs(eta) > 0.5) custom_refMult[refMult2 | chargeName | tpcHalfName] += 1;

        // refMult3 definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) < 1 && Exclude protons
        if (track->nSigmaProton() < -3. && mass2 < 0.4) custom_refMult[refMult3 | chargeName | tpcHalfName] += 1;
      }

      if (track->nHitsFit(kTpcId) >= 15)
      {
        // refMult4 definition: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 15 && abs(eta) < 1 && Exclude kaons
        if ((mass2 <= -990. && fabs(track->nSigmaKaon()) > 3) || // tof is not available
            (mass2 >  -990. && (mass2 > 0.6 || mass2 < 0.1)))    // tof is available
          custom_refMult[refMult4 | chargeName | tpcHalfName] += 1;
      }
    }
    return custom_refMult;
  }
}
#endif
