/**
 * \class StEEmcCluster
 * \brief A base class for describing clusters of EEMC towers
 *
 * This class is designed to represent EEMC tower clusters.  
 * By "tower cluster" I mean a cluster of StEEmcTower objects, which
 * can in principle be clusters of tower, preshower or postshower
 * elements.
 *
 * \author Jason C. Webb
 * $Date: 2010/08/26 22:49:25 $
 * $Revision: 1.8 $
 *
 * \section steemccluster_conventions Conventions
 *
 * By convention, we assume that the first tower added to the cluster
 * is the "seed" tower.
 *
 * Each cluster should be assigned its own unique "key" by the maker
 * which produces it.
 *
 * This class makes no assumtion about the size and/or shape of the
 * cluster.  
 *
 */

#include "StEEmcCluster.h"
#include "StEvent/StEmcCluster.h"

#include <iostream>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

ClassImp(StEEmcCluster);

// ----------------------------------------------------------------------------
StEEmcCluster::StEEmcCluster() 
    : StEEmcBaseCluster()
{
  mEmcCluster=0;
  mEnergy=0.;
  mfPhibin=0.0;
  mfEtabin=0.0;
  mSumEta2W=0.;
  mSumEtaW=0.;
  mSumPhi2W=0.;
  mSumPhiW=0.;
  mMomentum=TVector3(0,0,0);
}

// ----------------------------------------------------------------------------
StEEmcCluster::StEEmcCluster( const StEEmcCluster &other )
    : StEEmcBaseCluster()
{
  mEmcCluster=other.mEmcCluster;
  mKey=other.mKey;

  mEnergy=other.mEnergy;
  mfPhibin=other.mfPhibin;
  mfEtabin=other.mfEtabin;
  mSumEta2W=other.mSumEta2W;
  mSumEtaW=other.mSumEtaW;
  mSumPhi2W=other.mSumPhi2W;
  mSumPhiW=other.mSumPhi2W;

  mWeights=other.mWeights;

  mEnergy=other.mEnergy;
  mMomentum=other.mMomentum;
  mPosition=other.mPosition;
  mMatched=other.mMatched;

  for ( UInt_t ii=0;ii<other.mTowers.size();ii++ )
    mTowers.push_back( other.mTowers[ii] );
}

// ----------------------------------------------------------------------------
StEEmcCluster::~StEEmcCluster(){
  /// If we have created an StEmcCluster, delete it
  //$$$  if ( mEmcCluster != 0 ) delete mEmcCluster;
  mTowers.clear();
  mWeights.clear();
}

// ----------------------------------------------------------------------------
void StEEmcCluster::add(const StEEmcTower &tower, Float_t weight)
{
  
  if ( weight * tower.energy() <= 0. ) return;

  mWeights.push_back( weight );
  mTowers.push_back( tower );
  
  Float_t energy = weight * tower.energy();
  mEnergy+=energy;

  mfEtabin+=energy*weight*tower.etabin();
  int myphi = (tower.phibin() - mTowers[0].phibin());
  mfPhibin+=energy*weight*myphi;

  mSumEta2W = (tower.etabin()*tower.etabin())*tower.energy()*weight;
  mSumPhi2W = (myphi*myphi)*tower.energy()*weight;
  mSumEtaW  = (tower.etabin())*tower.energy()*weight;
  mSumPhiW  = (myphi)*tower.energy()*weight;

  //$$$  static EEmcGeomSimple geom=EEmcGeomSimple::Instance();

  EEmcGeomSimple *geom=new EEmcGeomSimple();
  mMomentum += energy * ( geom->getTowerCenter( (UInt_t)tower.sector(), (UInt_t)tower.subsector(), (UInt_t)tower.etabin() ).Unit() );
  delete geom;

  mPosition = mMomentum.Unit();
  mPosition *= ( kEEmcZSMD / mMomentum.CosTheta() );

  if ( TMath::Abs(mPosition.Eta())>100.0 ) {
    Warning("add","Cluster eta is crazy!");
    print();
  }

  mNumberOfElements=mTowers.size();

}

// ----------------------------------------------------------------------------
Float_t StEEmcCluster::fracEtabin() const
{
  return mfEtabin/mEnergy;
}

// ----------------------------------------------------------------------------
Float_t StEEmcCluster::fracPhibin() const
{
  return mfPhibin/mEnergy+mTowers[0].phibin();
}

// ----------------------------------------------------------------------------
StEmcCluster *StEEmcCluster::stemc()
{

  if ( mEmcCluster ) return mEmcCluster;
  mEmcCluster=new StEmcCluster();

  mEmcCluster->setEta( momentum().Eta() );
  mEmcCluster->setPhi( momentum().Phi() );
  mEmcCluster->setSigmaEta(-1.);
  mEmcCluster->setSigmaPhi(-1.);
  mEmcCluster->setEnergy( energy() );
  mEmcCluster->SetUniqueID( mKey );
#if 1
  for ( Int_t i=0; i< numberOfTowers(); i++ ) 
    {
      StEmcRawHit *hit=mTowers[i].stemc();
      if (hit) mEmcCluster->addHit( hit );
    }
#endif
  
  return mEmcCluster;
}

// ----------------------------------------------------------------------------
void StEEmcCluster::print() const
{
  std::cout << "cluster key: " << mKey << std::endl;
  std::cout << "seed tower:  " << mTowers[0].name() << std::endl;
  //  std::cout << "ntowers:     " << mTowers.size() << std::endl;
  //  std::cout << "feta:        " << fracEtabin() << std::endl;
  std::cout << "fphi:        " << fracPhibin() << std::endl;
  std::cout << "energy:      " << mEnergy << std::endl;
  std::cout << "pt:          " << mMomentum.Perp() << std::endl;
  std::cout << "eta:         " << mMomentum.Eta() << std::endl;
  std::cout << "phi:         " << mMomentum.Phi() << std::endl;

  for ( UInt_t i=0;i<mTowers.size();i++ )
    {
      mTowers[i].printLine(); std::cout << " W=" << mWeights[i] << std::endl;
    }
}

// ----------------------------------------------------------------------------
void StEEmcCluster::printLine(Bool_t Endl) const
{
  std::cout << "key="<<mKey<<" seed="<<mTowers[0].name()<<" ntow="<<mTowers.size()<<" pt=" <<mMomentum.Perp()<<" eta="<<mMomentum.Eta()<<" phi="<<mMomentum.Phi();
  if ( Endl ) std::cout << std::endl;
}

// ----------------------------------------------------------------------------
Bool_t StEEmcCluster::isNeighbor(const StEEmcTower &tower) const
{

  for ( UInt_t i=0;i<mTowers.size();i++ )
    {
      StEEmcTower myTower=mTowers[i];
      if ( myTower.isNeighbor(tower) ) return true;
    }

  return false;

}

// ----------------------------------------------------------------------------
Bool_t StEEmcCluster::hasTower(const StEEmcTower &tower) const
{

  for ( UInt_t i=0;i<mTowers.size();i++ )
    {
      if ( tower.index() == mTowers[i].index() ) return true;
    }
  return false;
}

// ----------------------------------------------------------------------------
Float_t StEEmcCluster::sigmaE() const
{
  Float_t sumE=0.;
  Float_t sumE2=0.;
  Float_t sumw=0.;
  for ( UInt_t i=0;i<mTowers.size();i++ )
    {
      StEEmcTower t=mTowers[i];
      Float_t     w=mWeights[i];
      sumE += t.energy()*w;
      sumE2 += t.energy()*t.energy()*w;
      sumw+=w;
    }
  Float_t mean=sumE/sumw;
  Float_t var=sumE2/sumw-mean*mean;
  return TMath::Sqrt(var);  
}

// ----------------------------------------------------------------------------
Int_t StEEmcCluster::numberOfEtabins() const
{
  Int_t etabins[12];for ( Int_t ii=0;ii<12;ii++ ) etabins[ii]=0;
  for ( UInt_t ii=0;ii<mTowers.size();ii++ ) etabins[ mTowers[ii].etabin() ]++;
  Int_t count=0;
  for ( Int_t ii=0;ii<12;ii++ ) if ( etabins[ii] ) count++;
  return count;
}

// ----------------------------------------------------------------------------
Int_t StEEmcCluster::numberOfPhibins() const
{
  Int_t phibins[60];for ( Int_t ii=0;ii<12;ii++ ) phibins[ii]=0;
  for ( UInt_t ii=0;ii<mTowers.size();ii++ ) phibins[ mTowers[ii].phibin() ]++;
  Int_t count=0;
  for ( Int_t ii=0;ii<12;ii++ ) if ( phibins[ii] ) count++;
  return count;
}
