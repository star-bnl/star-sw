//
// $id$
//
// $Log: StBsmdpPreCluster.cxx,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhya,
//         Aleksei Pavlinov , July 1999
//

//////////////////////////////////////////////////////////////////////////
//                                   
// StBsmdpPreCluster
//
// StBsmdpPreCluster differs from the base class StEmcPreCluster only one 
// function => calcMeanAndRms(StEmcHitCollection *emc_hits).
//
//////////////////////////////////////////////////////////////////////////

#include "StBsmdpPreCluster.h"
ClassImp(StBsmdpPreCluster)

//__________________________________________________________________________
StBsmdpPreCluster::StBsmdpPreCluster(TArrayI *hits) : StEmcPreCluster(hits) { /* Nobody */ }
//_____________________________________________________________________________
void StBsmdpPreCluster::calcMeanAndRms(StEmcHitCollection *emc_hits)
{
// The cluster size in phi direction equal zero for Bsmdp.
  Float_t etah, phih;
  Int_t id, check, m, e, s;

  if(mNhits == 1){
    id    = emc_hits->HitId(mHitsID[0]);
    check = emc_hits->getBin(id, m, e, s);

    if(check == 0){
      emc_hits->getEta(m,e, etah);      
      emc_hits->getPhi(m,s, phih);
      mEta      = etah;
      mPhi      = phih;
      mSigmaEta = 0.0;
      mSigmaPhi = 0.0;
      mEnergy   = emc_hits->HitEnergy(mHitsID[0]);
    }
    else printf("Ctor for StBsmdpPreCluster => bad id %i m,e,s %i %i %i \n",id, m, e, s);
  }
  else{
    Float_t E;
    Int_t sCheck;
    for(int i=0; i<mNhits; i++){
      id    = emc_hits->HitId(mHitsID[i]);
      check = emc_hits->getBin(id, m, e, s);
      if(i == 0) {
        emc_hits->getEta(m,e, etah);
        mEta      = etah;
        mSigmaEta = 0.0;
        sCheck=s;
      }
      if(sCheck != s) cout<<" Bad cluster for Bsmdp "<<endl; // Only for testing

      if(check == 0){
        E = emc_hits->HitEnergy(mHitsID[i]);
        emc_hits->getPhi(m,s, phih);      

        mEnergy   += E;
        mPhi      += phih*E;
        mSigmaPhi += phih*phih*E;
      }
      else printf("Ctor for StBsmdpPreCluster => bad id %i m,e,s %i %i %i \n",id, m, e, s); 
    }

    mPhi     /= mEnergy;
    mSigmaPhi = sqrt(mSigmaPhi/mEnergy - mPhi*mPhi);
  }
}
