//
// $id$
//
// $Log: StEmcPreCluster.cxx,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//          initial version from Akio Ogawa    
//

//////////////////////////////////////////////////////////////////////////
//                                   
// StEmcPreCluster
//
// StEmcPreCluster is base class for electromagnetic cluster. Used by
// StBemcPreCluster, StBsmdePreCluster and StBsmdePreCluster.
//
//////////////////////////////////////////////////////////////////////////

#include "StEmcPreCluster.h"
ClassImp(StEmcPreCluster)
//_____________________________________________________________________________
StEmcPreCluster::StEmcPreCluster(TArrayI *hits) : TObject() 
{
  mEnergy = 0.0; mEta = 0.0; mPhi = 0.0; 
  mSigmaEta =0.0; mSigmaPhi=0.0;

  mNhits = hits->GetSize();
  if(mNhits>0) {
    mHitsID.Set(mNhits,hits->GetArray());
  }
  else printf(" <E> Ctor StEmcPreCluster => mNhits = %i \n", mNhits);
}
//_____________________________________________________________________________
void StEmcPreCluster::calcMeanAndRms(StEmcHitCollection *emc_hits)
{
// For caclulation of cluster's characteristics.
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
    else printf("Ctor for StEmcPreCluster => bad id %i m,e,s %i %i %i \n",id, m, e, s);
  }
  else{
    Float_t E;
    for(int i=0; i<mNhits; i++){
      id    = emc_hits->HitId(mHitsID[i]);
      check = emc_hits->getBin(id, m, e, s);

      if(check == 0){
        E = emc_hits->HitEnergy(mHitsID[i]);
        emc_hits->getEta(m,e, etah);      
        emc_hits->getPhi(m,s, phih);      

        mEnergy   += E;
        mEta      += etah*E;
        mPhi      += phih*E;
        mSigmaEta += etah*etah*E;
        mSigmaPhi += phih*phih*E;
      }
      else printf("Ctor for StEmcPreCluster => bad id %i m,e,s %i %i %i \n",id, m, e, s); 
    }

    mEta /= mEnergy;
    mSigmaEta = mSigmaEta/mEnergy - mEta*mEta;
    if(mSigmaEta<=1.e-7) mSigmaEta = 0.0;
    else mSigmaEta = sqrt(mSigmaEta);

    mPhi /= mEnergy;
    mSigmaPhi = mSigmaPhi/mEnergy - mPhi*mPhi;
    if(mSigmaPhi<=1.0e-7) mSigmaPhi = 0.0;
    else mSigmaPhi = sqrt(mSigmaPhi);
  }
}
//_____________________________________________________________________________
void StEmcPreCluster::print(ostream *os)
{
// Printing member function.
  *os << " Energy " << mEnergy << " #hits " << mNhits;
  *os <<" HitsId => ";
  for(Int_t i=0; i<mNhits; i++){*os <<mHitsID[i]<<" ";}
  *os <<endl;

  *os << " eta " << mEta << "+/-" << mSigmaEta;
  *os <<"   phi " << mPhi << "+/-" << mSigmaPhi << endl;
}
//_____________________________________________________________________________
ostream &operator<<(ostream &os, StEmcPreCluster &cl)
{
// Global operator << for StEmcPreCluster. 
  cl.print(&os); return os;
}
