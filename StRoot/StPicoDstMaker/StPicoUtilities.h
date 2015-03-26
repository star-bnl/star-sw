
#ifndef StPicoUtilities_h
#define StPicoUtilities_h

#include "StMuDSTMaker/COMMON/StMuTrack.h" 
#include "StMuDSTMaker/COMMON/StMuDst.h" 

namespace StPicoUtilities {

  //______________________________________________________________________________
  // Reference multiplicity |eta|<0.5
  inline UInt_t refMult(const UInt_t charge, const StMuDst& muDst)
  {
    // charge   0:negative, 1:positive

    size_t countedTracks = 0;
    for (Int_t itrk=0; itrk<muDst.primaryTracks()->GetEntries(); itrk++){
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if(!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge==0&&track->charge()<0)||(charge==1&&track->charge()>0);
      if (track->flag()<0 || !isChargeOk || track->nHitsFit(kTpcId)<10 ) continue; 

      // check eta, a bit more elaborate
      if (fabs(track->momentum().mag())<1.e-10) 		continue;
      if (fabs(track->momentum().pseudoRapidity())>0.5) 	continue;
      // finally, check dca, if a track satisfies gets inside the if, count it.
      if (track->dca().mag()<3) ++countedTracks;
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
    for (Int_t itrk=0; itrk<muDst.primaryTracks()->GetEntries(); itrk++){
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if(!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge==0&&track->charge()<0)||(charge==1&&track->charge()>0);
      if (track->flag()<0 || !isChargeOk || track->nHitsFit(kTpcId)<10 ) continue; 

      // check eta, a bit more elaborate
      if (fabs(track->momentum().mag())<1.e-10) continue;
      const Double_t eta = track->momentum().pseudoRapidity() ;
      const Bool_t isEtaOk = (etaId==0&&(eta>-1.0&&eta<-0.5)) || (etaId==1&&(eta>0.5&&eta<1.0));
      if (!isEtaOk) continue;
      // finally, check dca, if a track satisfies gets inside the if, count it.
      if (track->dca().mag()<3) ++countedTracks;
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
    for (Int_t itrk=0; itrk<muDst.primaryTracks()->GetEntries(); itrk++){
      StMuTrack* track = muDst.primaryTracks(itrk) ;
      if(!track) continue;

      // these first 3 checks are easy, save time
      const Bool_t isChargeOk = (charge==0&&track->charge()<0)||(charge==1&&track->charge()>0);
      if (track->flag()<0 || !isChargeOk || track->nHitsFit(kTpcId)<10 ) continue; 

      // check eta, a bit more elaborate
      if (fabs(track->momentum().mag())<1.e-10) continue;
      const Double_t eta = track->momentum().pseudoRapidity() ;
      if (fabs(eta)>1.0) continue;

      const Bool_t isEtaOk = (etaId==0&&eta<0)||(etaId==1&&eta>0) ;
      if (!isEtaOk) continue ;
      // finally, check dca, if a track satisfies gets inside the if, count it.
      if (track->dca().mag()<3) ++countedTracks;
    }
    return countedTracks;
  }

}

#endif

