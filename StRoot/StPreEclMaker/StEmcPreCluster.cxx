// $Id: StEmcPreCluster.cxx,v 1.12 2003/01/23 03:49:59 jeromel Exp $
//
// $Log: StEmcPreCluster.cxx,v $
// Revision 1.12  2003/01/23 03:49:59  jeromel
// Include changed
//
// Revision 1.11  2001/10/01 15:36:19  pavlinov
// cleanup
//
// Revision 1.10  2001/09/22 00:30:08  pavlinov
// No public constructor for StEmcGeom
//
// Revision 1.9  2001/04/17 23:51:22  pavlinov
// Clean up before MDC4
//
// Revision 1.8  2001/02/01 22:23:09  suaide
// Fixed some memory leaks
//
// Revision 1.7  2001/01/26 21:54:23  suaide
// fixed a small bug in the phi calculation for a given cluster
// CVt: ----------------------------------------------------------------------
//
// Revision 1.6  2000/12/01 21:15:40  suaide
//
//
// Small fixes in StPreEclMaker::Make()
//       if some detector fails to find clusters it was aborting the chain
//
// Small fixes in StEmcPreClusterCollection::findClustersInModule()
// Small fixes in StEmcPreClusterCollection::testOnNeighbor()
//
// Small fixes in StEmcPreCluster::calcMeanAndRms()
//
// Revision 1.5  2000/09/08 22:55:05  suaide
//
//
//
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
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/StEmcMath.h"

ClassImp(StEmcPreCluster)


StEmcGeom* emcgeo;

//_____________________________________________________________________________
StEmcPreCluster::StEmcPreCluster(TArrayI *hits) : StObject() 
{
}
//_____________________________________________________________________________
StEmcPreCluster::StEmcPreCluster(Int_t mod,TArrayI *hits,Int_t detector) : StObject() 
{
  mEnergy = 0.0; mEta = 0.0; mPhi = 0.0; 
  mSigmaEta =0.0; mSigmaPhi=0.0;
  
  mModule=mod;
  
  mDetector=detector;
      
  mNhits = hits->GetSize();
  if(mNhits>0) 
  {
    mHitsID.Set(mNhits,hits->GetArray());
  }
  else printf(" <E> Ctor StEmcPreCluster => mNhits = %i \n", mNhits);
}
//_____________________________________________________________________________
StEmcPreCluster::StEmcPreCluster(Int_t mod,TArrayI *hits,Int_t detector,StEmcDetector* mDet)  
{
  mEnergy = 0.0; mEta = 0.0; mPhi = 0.0; 
  mSigmaEta =0.0; mSigmaPhi=0.0;
  
  mModule=mod;
  
  mDetector=detector;
    
  mNhits = hits->GetSize();
  if(mNhits>0) 
  {
    mHitsID.Set(mNhits,hits->GetArray());
  }
  else printf(" <E> Ctor StEmcPreCluster => mNhits = %i \n", mNhits);
  calcMeanAndRms(mDet,mod);
}
//_____________________________________________________________________________
void StEmcPreCluster::calcMeanAndRms(StEmcDetector* mDet,Int_t mod)
{
// For caclulation of cluster's characteristics.
  emcgeo=StEmcGeom::getEmcGeom(mDetector);
  StSPtrVecEmcRawHit& mStEventHits=mDet->module(mod)->hits();
  Float_t etah, phih;
  Int_t m, e, s, indh;
  if(mNhits == 1){
    indh = mHitsID[0];
    m=(Int_t)mStEventHits[indh]->module();
    e=(Int_t)mStEventHits[indh]->eta();
    s=abs(mStEventHits[indh]->sub());
    emcgeo->getEta(m,e, etah);      
    emcgeo->getPhi(m,s, phih);
    mEta      = etah;
    mPhi      = phih;
    mSigmaEta = 0.0;
    mSigmaPhi = 0.0;
    mEnergy   = mStEventHits[indh]->energy();
  }
  else{
    Float_t E, phi0;
    for(int i=0; i<mNhits; i++)
    {
      indh = mHitsID[i];
      m=mStEventHits[indh]->module();
      e=mStEventHits[indh]->eta();
      s=abs(mStEventHits[indh]->sub());
      
      E = mStEventHits[indh]->energy();
      emcgeo->getEta(m,e, etah);      
      emcgeo->getPhi(m,s, phih);      
      
      if(i == 0) {phi0 =  phih; phih = 0.0;}
      else        phih -= phi0;           // Rotate to the system of first hit 
      phih = StEmcMath::getPhiPlusMinusPi(phih);

      mEnergy   += E;
      mEta      += etah*E;
      mPhi      += phih*E;
      mSigmaEta += etah*etah*E;
      mSigmaPhi += phih*phih*E;      
    }

    mEta /= mEnergy;
    mSigmaEta = mSigmaEta/mEnergy - mEta*mEta;
    if(mNhits==2 && (mStEventHits[mHitsID[0]]->eta()==mStEventHits[mHitsID[1]]->eta())) 
      mSigmaEta = 0.0; // Same eta
    else {
      if(mSigmaEta <= 0.0) mSigmaEta = 0.0;
      else mSigmaEta = sqrt(mSigmaEta);
    }

    mPhi /= mEnergy;
    mSigmaPhi = mSigmaPhi/mEnergy - mPhi*mPhi;
    if(mNhits==2 && (mStEventHits[mHitsID[0]]->sub()==mStEventHits[mHitsID[1]]->sub()))
       mSigmaPhi = 0.0;  // Same phi
    else {
      if(mSigmaPhi <= 0.0) mSigmaPhi = 0.0;
      else mSigmaPhi = sqrt(mSigmaPhi);
    }
    mPhi += phi0;                 // Rotate to STAR system
    mPhi  = StEmcMath::getPhiPlusMinusPi(mPhi);
  }
  //  delete emcgeo;
}

void 
StEmcPreCluster::print(ostream *os)
{
// Printing member function.
  *os << " m " << Module();
  *os << " Energy " << mEnergy << " #hits " << mNhits;
  *os <<" HitsId => ";
  for(Int_t i=0; i<mNhits; i++){*os <<mHitsID[i]<<" ";}
  *os <<endl;

  *os << " eta " << mEta << "+/-" << mSigmaEta;
  *os <<"   phi " << mPhi << "+/-" << mSigmaPhi << endl;
}

ostream &operator<<(ostream &os, StEmcPreCluster &cl)
{
// Global operator << for StEmcPreCluster. 
  cl.print(&os); return os;
}

void 
StEmcPreCluster::Browse(TBrowser *b)
{
  cout << (*this) << endl;
  StObject::Browse(b);
}
