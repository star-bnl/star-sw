//
// $id$
//
// $Log: StBsmdePreCluster.cxx,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

//////////////////////////////////////////////////////////////////////////
//                                   
// StBsmdePreCluster
//
// StBsmdePreCluster differs from the base class StEmcPreCluster only one 
// function => calcMeanAndRms(StEmcHitCollection *emc_hits).
//
//////////////////////////////////////////////////////////////////////////

#include "StBsmdePreCluster.h"
ClassImp(StBsmdePreCluster)

//__________________________________________________________________________
StBsmdePreCluster::StBsmdePreCluster(TArrayI *hits) : StEmcPreCluster(hits) { /* Nobody */ }
//_____________________________________________________________________________
void StBsmdePreCluster::calcMeanAndRms(StEmcHitCollection *emc_hits)
{
// The cluster size in eta direction equal zero for Bsmde.
  Float_t etah, phih;
  Int_t id, check, m, e, s;

  if(mNhits == 1){
    id    = emc_hits->HitId(mHitsID[0]);
    check = emc_hits->getBin(id, m, e, s);

    if(check == 0 && s == 1){
      emc_hits->getEta(m,e, etah);      
      emc_hits->getPhi(m,s, phih);
      mEta      = etah;
      mPhi      = phih;
      mSigmaEta = 0.0;
      mSigmaPhi = 0.0;
      mEnergy   = emc_hits->HitEnergy(mHitsID[0]);
    }
    else printf("Ctor for StBsmdePreCluster => bad id %i m,e,s %i %i %i \n",id, m, e, s);
  }
  else{
    Float_t E;
    for(int i=0; i<mNhits; i++){
      id    = emc_hits->HitId(mHitsID[i]);
      check = emc_hits->getBin(id, m, e, s);

      if(check == 0 && s == 1){
        if(i ==0) {
          emc_hits->getPhi(m,s, phih);
          mPhi      = phih;
          mSigmaPhi = 0.0;
        }

        E = emc_hits->HitEnergy(mHitsID[i]);
        emc_hits->getEta(m,e, etah);      

        mEnergy   += E;
        mEta      += etah*E;
        mSigmaEta += etah*etah*E;
      }
      else printf("Ctor for StBsmdePreCluster => bad id %i m,e,s %i %i %i \n",id, m, e, s); 
    }

    mEta     /= mEnergy;
    mSigmaEta = sqrt(mSigmaEta/mEnergy - mEta*mEta);
  }
}

