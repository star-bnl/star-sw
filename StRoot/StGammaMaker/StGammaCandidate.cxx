#include "StGammaCandidate.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StGammaEEmcLeakage.h"

ClassImp(StGammaCandidate);

// ---------------------------------------------------------------- utilities -
//
// Define some useful functions and structures for sorting towers by distance
//
struct Tower {
  StGammaTower *tower;
  Float_t dR;
};
Bool_t SortDistance ( const Tower& t1, const Tower& t2 )
{
  return (t1.dR < t2.dR);
}



// ------------------------------------------------------- class constructor --
StGammaCandidate::StGammaCandidate()
{
  SetId(0);
  SetTowerId(0);
  SetTowerClusterId(0);
  SetSmduClusterId(0);
  SetSmdvClusterId(0);
  SetDetectorId(0);

  SetMomentum( TVector3(0.,0.,0.) );
  SetPosition( TVector3(0.,0.,0.) );
  SetEnergy(0.);
  SetSeedEnergy(0.);
  SetPre1Energy(0.);
  SetPre2Energy(0.);
  SetPostEnergy(0.);
  SetSmduEnergy(0.);
  SetSmdvEnergy(0.);
}

// -------------------------------------------------------- class destructor --
StGammaCandidate::~StGammaCandidate()
{
}

// --------------------------------------------------------------- isolation --
Float_t StGammaCandidate::sumPt( Float_t radius )
{
  Float_t sum_tracks = sumTrackPt( radius );
  Float_t sum_towers = sumTowerPt( radius );
  return sum_tracks + sum_towers;
}

Float_t StGammaCandidate::sumTrackPt( Float_t radius )
{
  Float_t sum = 0.;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfTracks();i++ )
    {
      StGammaTrack *t = track(i);
      Float_t deta = my_eta - t->eta();
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi());
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius )
	sum += t->pt();
    }
  return sum;
}

Float_t StGammaCandidate::sumTowerPt( Float_t radius )
{
  Float_t sum = 0.;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfTowers();i++ )
    {
      StGammaTower *t = tower(i);
      if ( t->fail ) continue;
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius )
	sum += t->energy / TMath::CosH(t->eta);
    }
  return sum;
}


Float_t StGammaCandidate::sumPreshower1( Float_t radius )
{
  Float_t sum = 0.;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfPreshower1();i++ )
    {
      StGammaTower *t = preshower1(i);
      if ( t->fail ) continue;
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius )
	sum += t->energy / TMath::CosH(t->eta);
    }
  return sum;
}

Float_t StGammaCandidate::sumPreshower2( Float_t radius )
{
  Float_t sum = 0.;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfPreshower2();i++ )
    {
      StGammaTower *t = preshower2(i);
      if ( t->fail ) continue;
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius )
	sum += t->energy / TMath::CosH(t->eta);
    }
  return sum;
}

Float_t StGammaCandidate::sumPostshower( Float_t radius )
{
  Float_t sum = 0.;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfPostshower();i++ )
    {
      StGammaTower *t = postshower(i);
      if ( t->fail ) continue;
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius )
	sum += t->energy / TMath::CosH(t->eta);
    }
  return sum;
}


Int_t StGammaCandidate::numberOfTracks( Float_t radius, Float_t minpt )
{
  Int_t count=0;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfTracks();i++ )
    {
      StGammaTrack *t = track(i);
      Float_t deta = my_eta - t->eta();
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi());
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius && t->pt() >= minpt )
	count++;
    }
  return count;
}





Int_t StGammaCandidate::numberOfTowers( Float_t radius, Float_t minpt )
{
  Int_t count=0;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfTowers();i++ )
    {
      StGammaTower *t = tower(i);
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius && t->pt() >= minpt )
	count++;
    }
  return count;
}

Int_t StGammaCandidate::numberOfPreshower1( Float_t radius, Float_t thresh )
{
  Int_t count=0;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfPreshower1();i++ )
    {
      StGammaTower *t = preshower1(i);
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius && t->energy >= thresh )
	count++;
    }
  return count;
}



Int_t StGammaCandidate::numberOfPreshower2( Float_t radius, Float_t thresh )
{
  Int_t count=0;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfPreshower2();i++ )
    {
      StGammaTower *t = preshower2(i);
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius && t->energy >= thresh )
	count++;
    }
  return count;
}



Int_t StGammaCandidate::numberOfPostshower( Float_t radius, Float_t thresh )
{
  Int_t count=0;
  Float_t my_eta = mMomentum.Eta();
  Float_t my_phi = mMomentum.Phi();
  for ( Int_t i=0;i<numberOfPostshower();i++ )
    {
      StGammaTower *t = postshower(i);
      Float_t deta = my_eta - t->eta;
      Float_t dphi = TVector2::Phi_mpi_pi(my_phi - t->phi);
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius && t->energy >= thresh )
	count++;
    }
  return count;
}


TVector3 StGammaCandidate::momentum1x1()
{

  return mMomentum.Unit() * mSeedEnergy;

}

TVector3 StGammaCandidate::momentum1x1c()
{

  //
  // Perform light leakage correction w/in the EEmc
  //
  if ( detectorId() == kEEmc ) 
    {
      static StGammaEEmcLeakage *shape = StGammaEEmcLeakage::instance();
      static EEmcGeomSimple    &geom  = EEmcGeomSimple::Instance();
      
      Int_t sec,sub,eta;
      if ( !geom.getTower( position(), sec, sub, eta ) )
	{
	  return TVector3(0.,0.,-999.0);
	}
      
      Int_t mysec,mysub,myeta;
      myeta=mTowerId%12;
      mysec=( mTowerId/12 )/5;
      mysub=( mTowerId/12 )%5;
      
      TVector3 tower = geom.getTowerCenter( (UInt_t)sec, (UInt_t)sub, (UInt_t)eta );
      
      Float_t frac = shape->expectation( position() );
      TVector3 p=momentum1x1();
      p*=1.0/frac;
      
      return p;

    }
  else
    {
      // to be implemented
    }
  
  return momentum1x1();
      
}



TVector3 StGammaCandidate::momentum2x1()
{
  Float_t eta = mMomentum.Eta();
  Float_t phi = mMomentum.Phi();
  std::vector<Tower> listOfTowers;
  for ( Int_t i=0;i<numberOfMyTowers(); i++ ) {
    StGammaTower *t = mytower(i);
    Float_t deta = eta - t->eta;
    Float_t dphi = TVector2::Phi_mpi_pi(phi - t->phi);
    Float_t dR   = TMath::Sqrt(deta*deta+dphi*dphi);
    Tower T      = { t, dR };
    listOfTowers.push_back(T);
  }
  std::sort( listOfTowers.begin(), listOfTowers.end(), SortDistance );
  std::vector<Tower>::iterator iter;
  Float_t energy_sum=0.;
  Int_t count=0;
  //std::cout << "==========================" << std::endl;                     
  for ( iter=listOfTowers.begin(); iter!=listOfTowers.end();iter++ )
    {
      if ( count<2 ) {
        energy_sum += (*iter).tower->energy;
      }
      count++;
    }
  TVector3 p=mMomentum;
  p.SetMag(energy_sum);
  return p;
}


