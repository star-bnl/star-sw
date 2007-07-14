#include "StGammaCandidate.h"

ClassImp(StGammaCandidate);


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
      Float_t dphi = my_phi - t->phi();
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi();
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi;
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
      Float_t dphi = my_phi - t->phi;
      Float_t r = TMath::Sqrt(deta*deta + dphi*dphi);
      if ( r <= radius && t->energy >= thresh )
	count++;
    }
  return count;
}


