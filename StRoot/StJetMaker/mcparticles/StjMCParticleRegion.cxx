//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Lab
// 10 August 2015
//

#include "StjMCParticleRegion.h"


ClassImp(StjMCParticleRegion);

StjMCParticleList StjMCParticleRegion::Do( const StjMCParticleList& mcParticleList, const StJetCandidate* leadingjet, const TString bname)
{
  StjMCParticleList elist;
  // Track loop
  for (StjMCParticleList::const_iterator iParticle = mcParticleList.begin(); iParticle != mcParticleList.end(); ++iParticle) {
    StjMCParticle particle = *iParticle;
    if(bname == "toward" ){
      if(TMath::Abs(TVector2::Phi_mpi_pi( leadingjet->phi() - particle.phi) < (mphiplus * TMath::DegToRad())) && TMath::Abs(particle.eta) <  mdeta ){
	elist.push_back(particle);
      }
    }
    if(bname == "away"){
      if(TMath::Abs(TVector2::Phi_mpi_pi( leadingjet->phi() - particle.phi) > (mphiplus * TMath::DegToRad())) && TMath::Abs(particle.eta) <  mdeta ){
	elist.push_back(particle);
      }
    }
    if(bname == "transP" || bname == "transM"){
      if(TVector2::Phi_mpi_pi( leadingjet->phi() - particle.phi) < (mphiplus * TMath::DegToRad()) &&  TVector2::Phi_mpi_pi( leadingjet->phi() - particle.phi) > (mphiminus * TMath::DegToRad()) && TMath::Abs(particle.eta) <  mdeta ){
      elist.push_back(particle);
      }
    }
  } // End track loop  
  return elist;
}
