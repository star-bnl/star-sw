// $Id: StEmcPreCluster.cxx,v 1.5 2000/09/08 22:55:05 suaide Exp $
//
// $Log: StEmcPreCluster.cxx,v $
// Revision 1.5  2000/09/08 22:55:05  suaide
// some modifications to compile on Solaris
//
// Revision 1.4  2000/08/24 22:11:34  suaide
//
//
// restored some files for background compatibility
//
// Revision 1.3  2000/08/24 19:45:36  suaide
//
//
// small modifications: some cout has been removed
//
// Revision 1.2  2000/08/24 11:26:48  suaide
//
//
//
// by A. A. P. Suaide - 2000/08/24 07:25:00
//
// Notes:
//
// 1. Full StEvent Compatible
// 2. Read hits from StEvent object
// 3. Write clusters in StEvent format and old format to keep background
//    compatibility
// 4. Do clustering in bemc, bprs, bsmde, bsmdp
// 5. Included method StPreEclMaker::SetClusterCollection
//
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Alexandre A. P. Suaide (version 2.0)
//         Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//         initial version from Akio Ogawa    
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
#include "StEvent/StEvent.h" 
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/StEmcGeom.h"

ClassImp(StEmcPreCluster)


StEmcGeom* emcgeo;

//_____________________________________________________________________________
StEmcPreCluster::StEmcPreCluster(TArrayI *hits) : TObject() 
{
}
//_____________________________________________________________________________
StEmcPreCluster::StEmcPreCluster(Int_t mod,TArrayI *hits,Int_t detector) : TObject() 
{
  mEnergy = 0.0; mEta = 0.0; mPhi = 0.0; 
  mSigmaEta =0.0; mSigmaPhi=0.0;
  
  mModule=mod;
  
  emcgeo=new StEmcGeom(detector);
    
  mNhits = hits->GetSize();
  if(mNhits>0) 
  {
    mHitsID.Set(mNhits,hits->GetArray());
  }
  else printf(" <E> Ctor StEmcPreCluster => mNhits = %i \n", mNhits);
}
//_____________________________________________________________________________
void StEmcPreCluster::calcMeanAndRms(StEmcDetector* mDet,Int_t mod)
{
// For caclulation of cluster's characteristics.
  StSPtrVecEmcRawHit& mStEventHits=mDet->module(mod)->hits();
  Float_t etah, phih;
  Int_t m, e, s;
  if(mNhits == 1){
    m=(Int_t)mStEventHits[mHitsID[0]]->module();
    e=(Int_t)mStEventHits[mHitsID[0]]->eta();
    s=abs(mStEventHits[mHitsID[0]]->sub());
    emcgeo->getEta(m,e, etah);      
    emcgeo->getPhi(m,s, phih);
    mEta      = etah;
    mPhi      = phih;
    mSigmaEta = 0.0;
    mSigmaPhi = 0.0;
    mEnergy   = mStEventHits[mHitsID[0]]->energy();
    
  }
  else{
    Float_t E;
    for(int i=0; i<mNhits; i++)
    {
      m=mStEventHits[1]->module();
      e=mStEventHits[mHitsID[i]]->eta();
      s=abs(mStEventHits[mHitsID[i]]->sub());
      
      E = mStEventHits[mHitsID[i]]->energy();
      emcgeo->getEta(m,e, etah);      
      emcgeo->getPhi(m,s, phih);      

      mEnergy   += E;
      mEta      += etah*E;
      mPhi      += phih*E;
      mSigmaEta += etah*etah*E;
      mSigmaPhi += phih*phih*E;
      
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
