//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Lab
// 3 August 2015
//

#include "StjTrackRegion.h"


ClassImp(StjTrackRegion);

StjTrackList StjTrackRegion::Do( const StjTrackList& trackList, const StJetCandidate* leadingjet, const TString bname)
{
  StjTrackList elist;
  // Track loop

  for (StjTrackList::const_iterator iTrack = trackList.begin(); iTrack != trackList.end(); ++iTrack) {
    StjTrack track = *iTrack;
    
    if(bname == "toward" ){
      if(TMath::Abs(TVector2::Phi_mpi_pi( leadingjet->phi() - track.phi)) < (mphiplus * TMath::DegToRad()) && TMath::Abs(track.eta) <  mdeta ){
	elist.push_back(track);
      }
    }
    if(bname == "away"){
      if(TMath::Abs(TVector2::Phi_mpi_pi( leadingjet->phi() - track.phi)) > (mphiplus * TMath::DegToRad()) && TMath::Abs(track.eta) <  mdeta ){
	elist.push_back(track);
      }
    }
    if(bname == "transP" || bname == "transM"){
      if(TVector2::Phi_mpi_pi( leadingjet->phi() - track.phi) < (mphiplus * TMath::DegToRad()) &&  TVector2::Phi_mpi_pi( leadingjet->phi() - track.phi) > (mphiminus * TMath::DegToRad()) && TMath::Abs(track.eta) <  mdeta ){
	elist.push_back(track);
      }
    }
  } // End track loop  
  return elist;
}
