#ifndef StPicoUtilities_h
#define StPicoUtilities_h

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

namespace StPicoUtilities
{

  //______________________________________________________________________________
  // Reference multiplicity |eta|<0.5
  inline UInt_t refMult(const UInt_t charge, const StMuDst& muDst)
  {
    // charge   0:negative, 1:positive

    size_t countedTracks = 0;
    for (Int_t itrk = 0; itrk < muDst.primaryTracks()->GetEntries(); ++itrk)
    {
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if (!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge == 0 && track->charge() < 0) || (charge == 1 && track->charge() > 0);
      if (track->flag() < 0 || !isChargeOk || track->nHitsFit(kTpcId) < 10) continue;

      // check eta, a bit more elaborate
      if (fabs(track->momentum().mag()) < 1.e-10)     continue;
      if (fabs(track->momentum().pseudoRapidity()) > 0.5)   continue;
      // finally, check dca, if a track satisfies gets inside the if, count it.
      if (track->dca().mag() < 3) ++countedTracks;
    }
    return countedTracks;
  }

  //______________________________________________________________________________
  // Reference multiplicity2 |eta|>0.5
  inline UInt_t refMult2(const UInt_t charge, const UInt_t etaId, const StMuDst& muDst)
  {
    // charge   0:negative, 1:positive
    // etaId    0:East(eta<-0.5), 1:West(eta>0.5)

    size_t countedTracks = 0;
    for (Int_t itrk = 0; itrk < muDst.primaryTracks()->GetEntries(); ++itrk)
    {
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if (!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge == 0 && track->charge() < 0) || (charge == 1 && track->charge() > 0);
      if (track->flag() < 0 || !isChargeOk || track->nHitsFit(kTpcId) < 10) continue;

      // check eta, a bit more elaborate
      if (fabs(track->momentum().mag()) < 1.e-10) continue;
      const Double_t eta = track->momentum().pseudoRapidity() ;
      const Bool_t isEtaOk = (etaId == 0 && (eta > -1.0 && eta < -0.5)) || (etaId == 1 && (eta > 0.5 && eta < 1.0));
      if (!isEtaOk) continue;
      // finally, check dca, if a track satisfies gets inside the if, count it.
      if (track->dca().mag() < 3) ++countedTracks;
    }
    return countedTracks;
  }

  //______________________________________________________________________________
  // Reference multiplicity half eta<0 or eta>0
  inline UInt_t refMultHalf(const UInt_t charge, const UInt_t etaId, const StMuDst& muDst)
  {
    // charge   0:negative, 1:positive
    // etaId    0:East(eta<0), 1:West(eta>0)

    size_t countedTracks = 0;
    for (Int_t itrk = 0; itrk < muDst.primaryTracks()->GetEntries(); ++itrk)
    {
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if (!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge == 0 && track->charge() < 0) || (charge == 1 && track->charge() > 0);
      if (track->flag() < 0 || !isChargeOk || track->nHitsFit(kTpcId) < 10 || fabs(track->momentum().mag()) < 1.e-10) continue;

      // check eta, a bit more elaborate
      const Double_t eta = track->momentum().pseudoRapidity() ;
      if (fabs(eta) > 1.0) continue;

      const Bool_t isEtaOk = (etaId == 0 && eta < 0) || (etaId == 1 && eta > 0) ;
      if (!isEtaOk) continue ;
      // finally, check dca, if a track satisfies gets inside the if, count it.
      if (track->dca().mag() < 3) ++countedTracks;
    }
    return countedTracks;
  }

  //______________________________________________________________________________
  // Reference multiplicity3 |eta|<1 (Pions and Kaons)
  inline UInt_t refMult3(const UInt_t charge, const UInt_t etaId, const StMuDst& muDst)
  {
    // charge   0:negative, 1:positive
    // etaId    0:East(-1<eta<0), 1:West(1>eta>0)

    size_t countedTracks = 0;
    for (Int_t itrk = 0; itrk < muDst.primaryTracks()->GetEntries(); ++itrk)
    {
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if (!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge == 0 && track->charge() < 0) || (charge == 1 && track->charge() > 0);
      if (track->flag() < 0 || !isChargeOk || track->nHitsFit(kTpcId) < 10 || fabs(track->momentum().mag()) < 1.e-10 || track->dca().mag() > 3) continue;

      // check eta, a bit more elaborate
      const Double_t eta = track->momentum().pseudoRapidity() ;
      const Bool_t isEtaOk = (etaId == 0 && (eta > -1.0 && eta < 0)) || (etaId == 1 && (eta > 0 && eta < 1.0));
      if (!isEtaOk) continue;

      // finally, nsigmaproton,mass square, if a track satisfies gets inside the if, count it
      const Double_t p = track->momentum().mag();
      const Double_t beta = track->btofPidTraits().beta();
      Double_t  M2;
      if (beta <= 1.e-5) M2 = -999.;
      else M2 = p * p * (pow(1. / beta, 2) - 1);
      if (track->nSigmaProton() < (-3.0) && M2 < 0.4) ++countedTracks;
    }
    return countedTracks;
  }

  //______________________________________________________________________________
  // Reference multiplicity4 |eta|<1 (Pions and Protons)
  inline UInt_t refMult4(const UInt_t charge, const UInt_t etaId, const StMuDst& muDst)
  {
    // charge   0:negative, 1:positive
    // etaId    0:East(-1<eta<0), 1:West(1>eta>0)

    size_t countedTracks = 0;
    for (Int_t itrk = 0; itrk < muDst.primaryTracks()->GetEntries(); ++itrk)
    {
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if (!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge == 0 && track->charge() < 0) || (charge == 1 && track->charge() > 0);
      if (track->flag() < 0 || !isChargeOk || track->nHitsFit(kTpcId) < 15 || fabs(track->momentum().mag()) < 1.e-10 || track->dca().mag() > 3) continue;

      // check eta, a bit more elaborate
      const Double_t eta = track->momentum().pseudoRapidity() ;
      const Bool_t isEtaOk = (etaId == 0 && (eta > -1.0 && eta < 0)) || (etaId == 1 && (eta > 0 && eta < 1.0));
      if (!isEtaOk) continue;

      // finally, check nsigmaproton, mass square, if a track satisfies gets inside the if, count it
      const Double_t beta = track->btofPidTraits().beta();
      if (beta <= 1.e-5)
      {
        if (track->nSigmaKaon() > 3 || track->nSigmaKaon() < -3) ++countedTracks;
      }
      else
      {
        const Double_t p = track->momentum().mag();
        const Double_t M2 = p * p * (pow(1. / beta, 2) - 1);
        if (M2 > 0.6 || M2 < 0.1) ++countedTracks;
      }
    }
    return countedTracks;
  }
}
#endif
