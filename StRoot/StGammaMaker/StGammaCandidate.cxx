////////////////////////////////////////////////////////////
//                                                        //
//  Note that, when calculating the isolation sums,       //
//  the candidate momentum, tower eta and track eta       //
//  have been corrected for vertex so that the            //
//  dR comparisons are in fact consistent                 //
//                                                        //
////////////////////////////////////////////////////////////

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StGammaEEmcLeakage.h"

#include "StGammaCandidate.h"

ClassImp(StGammaCandidate);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
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

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaCandidate::~StGammaCandidate() 
{}

//////////////////////////////////////////////////
//    Return the total track pT and tower eT    //
//   within a cone of the given radius around   //
//                 the candidate                //
//////////////////////////////////////////////////
Float_t StGammaCandidate::sumPt(Float_t radius)
{
    Float_t sumTracks = sumTrackPt(radius);
    Float_t sumTowers = sumTowerPt(radius);
    return sumTracks + sumTowers;
}

//////////////////////////////////////////////////
//    Return the total track pT within a cone   //
//   of the given radius around the candidate   //
//////////////////////////////////////////////////
Float_t StGammaCandidate::sumTrackPt(Float_t radius)
{

    Float_t sum = 0.0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
  
    for(Int_t i = 0; i < numberOfTracks(); i++)
    {
    
        StGammaTrack *t = track(i);
        
        Float_t dEta = myEta - t->eta();
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi());
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if(r <= radius) sum += t->pt();
    
    }

    return sum;
  
}

//////////////////////////////////////////////////
//    Return the total tower Et within a cone   //
//   of the given radius around the candidate   //
//////////////////////////////////////////////////
Float_t StGammaCandidate::sumTowerPt(Float_t radius)
{

    Float_t sum = 0.0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
  
    for(Int_t i = 0; i < numberOfTowers(); i++)
    {
    
        StGammaTower *t = tower(i);
        if(t->fail) continue;
        
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if(r <= radius) sum += t->energy / TMath::CosH(t->eta);
    
    }
    
    return sum;
  
}

//////////////////////////////////////////////////
//  Return the total preshower Et within a cone //
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Float_t StGammaCandidate::sumPreshower1(Float_t radius)
{

    Float_t sum = 0.;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
    
    for(Int_t i = 0; i < numberOfPreshower1(); i++)
    {
    
        StGammaTower *t = preshower1(i);
        if(t->fail) continue;
        
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if(r <= radius) sum += t->energy / TMath::CosH(t->eta);
        
    }
    
    return sum;
  
}

//////////////////////////////////////////////////
//  Return the total preshower Et within a cone //
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Float_t StGammaCandidate::sumPreshower2(Float_t radius)
{

  Float_t sum = 0.0;
  Float_t myEta = mMomentum.Eta();
  Float_t myPhi = mMomentum.Phi();
  
  for(Int_t i = 0; i < numberOfPreshower2(); i++)
    {
    
      StGammaTower *t = preshower2(i);
      if(t->fail) continue;
      
      Float_t dEta = myEta - t->eta;
      Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
      Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
      
      if(r <= radius) sum += t->energy / TMath::CosH(t->eta);
	
    }
    
  return sum;
  
}

//////////////////////////////////////////////////
// Return the total postshower Et within a cone //
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Float_t StGammaCandidate::sumPostshower(Float_t radius)
{

    Float_t sum = 0.0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
    
    for(Int_t i = 0; i < numberOfPostshower(); i++)
    {
    
        StGammaTower *t = postshower(i);
        if (t->fail) continue;
        
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if (r <= radius) sum += t->energy / TMath::CosH(t->eta);
      
    }
    
    return sum;
  
}

//////////////////////////////////////////////////
//   Return the number of tracks within a cone  //
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Int_t StGammaCandidate::numberOfTracks(Float_t radius, Float_t minPt)
{

    Int_t count = 0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
    
    for (Int_t i = 0; i < numberOfTracks(); i++)
    {
    
      StGammaTrack *t = track(i);
      
      Float_t dEta = myEta - t->eta();
      Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi());
      Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
      
      if (r <= radius && t->pt() >= minPt) count++;
      
    }
    
    return count;
  
}

//////////////////////////////////////////////////
//   Return the number of towers within a cone  //
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Int_t StGammaCandidate::numberOfTowers(Float_t radius, Float_t minPt)
{

    Int_t count = 0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
    
    for(Int_t i = 0; i < numberOfTowers(); i++)
    {
    
        StGammaTower *t = tower(i);
        if(t->fail) continue;
        
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if (r <= radius && t->pt() >= minPt) count++;
        
    }
    
    return count;
  
}

//////////////////////////////////////////////////
// Return the number of preshowers within a cone//
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Int_t StGammaCandidate::numberOfPreshower1(Float_t radius, Float_t thresh)
{

    Int_t count = 0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
  
    for(Int_t i = 0; i < numberOfPreshower1(); i++)
    {
    
        StGammaTower *t = preshower1(i);
      
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if(r <= radius && t->energy >= thresh) count++;
      
    }
    
  return count;
  
}

//////////////////////////////////////////////////
// Return the number of preshowers within a cone//
//    of the given radius around the candidate  //
//////////////////////////////////////////////////
Int_t StGammaCandidate::numberOfPreshower2(Float_t radius, Float_t thresh)
{

    Int_t count = 0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
    
    for(Int_t i = 0; i < numberOfPreshower2(); i++)
    {
    
        StGammaTower *t = preshower2(i);
      
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
      
        if (r <= radius && t->energy >= thresh) count++;
        
    }
    
    return count;
    
}

//////////////////////////////////////////////////
//  Return the number of postshowers within a   //
// cone of the given radius around the candidate//
//////////////////////////////////////////////////
Int_t StGammaCandidate::numberOfPostshower(Float_t radius, Float_t thresh)
{

    Int_t count = 0;
    Float_t myEta = mMomentum.Eta();
    Float_t myPhi = mMomentum.Phi();
    
    for(Int_t i = 0; i < numberOfPostshower(); i++)
    {
    
        StGammaTower *t = postshower(i);
      
        Float_t dEta = myEta - t->eta;
        Float_t dPhi = TVector2::Phi_mpi_pi(myPhi - t->phi);
        Float_t r = TMath::Sqrt(dEta * dEta + dPhi * dPhi);
        
        if (r <= radius && t->energy >= thresh) count++;
    }
    
    return count;
  
}

//////////////////////////////////////////////////
//       Auxiliary struct and function for      //
//           sorting towers by distance         //
//////////////////////////////////////////////////
struct Tower 
{
    StGammaTower *tower;
    Float_t dR;
};

Bool_t SortDistance(const Tower& t1, const Tower& t2)
{
    return (t1.dR < t2.dR);
}

//////////////////////////////////////////////////
//   Return momentum using the seed tower only  //
//////////////////////////////////////////////////
TVector3 StGammaCandidate::momentum1x1()
{
    return mMomentum.Unit() * mSeedEnergy;
}

//////////////////////////////////////////////////
//     Return momentum using the seed tower     //
//      with leakage correction (EEMC only)     //
//////////////////////////////////////////////////
TVector3 StGammaCandidate::momentum1x1c()
{

    // Perform light leakage correction w/in the EEmc
    if(detectorId() == kEEmc) 
    {
    
        static StGammaEEmcLeakage *shape = StGammaEEmcLeakage::instance();
        static EEmcGeomSimple    &geom  = EEmcGeomSimple::Instance();
        
        Int_t sec,sub,eta;
        if(!geom.getTower( position(), sec, sub, eta )) return TVector3(0.0, 0.0, -999.0);
        
        Int_t mysec,mysub,myeta;
        myeta = mTowerId % 12;
        mysec = (mTowerId / 12) / 5;
        mysub = (mTowerId / 12) % 5;
        
        TVector3 tower = geom.getTowerCenter( (UInt_t)sec, (UInt_t)sub, (UInt_t)eta );
        
        Float_t frac = shape->expectation( position() );
        TVector3 p=momentum1x1();
        p *= 1.0 / frac;
        
        return p;
    
    }
    else
    {
        // to be implemented
    }
  
    return momentum1x1();
      
}

//////////////////////////////////////////////////
//     Return momentum using the seed tower     //
//     with leakage correction plus the most    //
//    energetic neighboring tower(EEMC only)    //
//////////////////////////////////////////////////
TVector3 StGammaCandidate::momentum2x1()
{

    Float_t eta = mMomentum.Eta();
    Float_t phi = mMomentum.Phi();
    
    // Create Tower structs for each cluster tower
    std::vector<Tower> listOfTowers;
    for(Int_t i = 0; i < numberOfMyTowers(); i++) 
    {
    
        StGammaTower *t = mytower(i);
        Float_t deta = eta - t->eta;
        Float_t dphi = TVector2::Phi_mpi_pi(phi - t->phi);
        Float_t dR   = TMath::Sqrt(deta * deta + dphi * dphi);
        Tower T      = { t, dR };
        listOfTowers.push_back(T);
        
    }
    
    // Sort Tower structs by energy
    std::sort(listOfTowers.begin(), listOfTowers.end(), SortDistance);
    std::vector<Tower>::iterator iter;
    
    Float_t energy_sum = 0.0;
    Int_t count = 0;
        
    // Sum the energy from the two highest towers          
    for(iter=listOfTowers.begin(); iter != listOfTowers.end(); iter++)
    {
        if(count < 2) energy_sum += (*iter).tower->energy;
        count++;
    }
    
    TVector3 p = mMomentum;
    p.SetMag(energy_sum);
    
    return p;
  
}

