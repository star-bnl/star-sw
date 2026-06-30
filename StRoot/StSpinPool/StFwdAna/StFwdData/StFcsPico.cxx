#include "StFcsPico.h"

ClassImp(StFcsPicoHit)
/*
StFcsPicoHit::Set(StFcsHit* hit, Double_t xpos, Double_t ypos, Double_t zpos )
{
  if( hit==0 ){ return; }

  mDetId  = hit->detectorId();
  mChId   = hit->id();
  mAdcSum = hit->adcSum();
  mEnergy = hit->energy();

  mXstar = xpos;
  mYstar = ypos;
  mZstar = zpos;  
}
*/
ClassImp(StFcsPicoCluster)

/*
StFcsPicoCluster::Set(StFcsCluster* clus, Double_t px, Double_t py, Double_t pz, Double_t e)
{
  if( clust==0 ){ return; }

  mId             = clus->id();
  mDetId          = clus->detectorId();
  mCategory       = clus->category();
  mNTowers        = clus->nTowers();
  mNNeighbor      = clus->nNeighbor();
  mNPoints        = clus->nPoints();
  mEnergy         = clus->energy();
  mX              = clus->x();
  mY              = clus->y();
  mSigmaMin       = clus->sigmaMin();
  mSigmaMax       = clus->sigmaMax();
  mTheta          = clus->theta();
  mChi2Ndf1Photon = clus->chi2Ndf1Photon();
  mChi2Ndf2Phoron = clus->chi2Ndf2Photon();

  //StLorentzVectorD clusp = clus->fourMomentum();
  mPx = px;//clusp.px();
  mPy = py;//clusp.py();
  mPz = pz;//clusp.pz();
  mE  = e;//clusp.e();
}
*/
ClassImp(StFcsPicoPoint)
/*
StFcsPicoPoint::Set(StFcsPoint* point, Double_t x, Double_t y, Double_t z, Double_t px, Double_t py, Double_t pz, Double_t e)
{
  if( point==0 ){ return; }

  mDetId                 = point->detectorId();
  mEnergy                = point->energy();
  mXlocal                = point->x();
  mYlocal                = point->y();
  mNParentClusterPhotons = point->nParentClusterPhotons();

  //StThreeVectorF pointxyz = point->xyz();
  mXstar = x;//pointxyz.x();
  mYstar = y;//pointxyz.y();
  mZstar = z;//pointxyz.z();

  //StLorentzVectorD pointp = point->fourMomentum();
  mPx = px;//pointp.px();
  mPy = py;//pointp.py();
  mPz = pz;//pointp.pz();
  mE  = e;//pointp.e();
  
}

*/

