// $Id: TrackListToFourList.cxx,v 1.3 2008/07/13 06:04:42 tai Exp $
#include "TrackListToFourList.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackFourVec.h"

namespace StSpinJet {

FourList TrackListToFourList::operator()(const TrackList& trackList)
{
  FourList ret;

  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {

    TVector3 momentum;
    momentum.SetPtEtaPhi((*track).pt, (*track).eta, (*track).phi);
    double pionMass = 0.1395700;
    float energy = sqrt(pionMass*pionMass + momentum.Mag()*momentum.Mag());

    TLorentzVector p4(momentum, energy);

    StMuTrackEmu *trackEmu = createTrackEmu(*track);

    StMuTrackFourVec* pmu = new StMuTrackFourVec(trackEmu, p4, (*track).charge, (*track).trackIndex, kTpcId);
    ret.push_back(pmu);
  }

  return ret;
}

StMuTrackEmu* TrackListToFourList::createTrackEmu(const Track& track)
{
  StMuTrackEmu *ret = new StMuTrackEmu();

  TVector3 momentum;
  momentum.SetPtEtaPhi(track.pt, track.eta, track.phi);

  ret->_px             =  momentum.Px()	     ;
  ret->_py	       =  momentum.Py()	     ;
  ret->_pz	       =  momentum.Pz()	     ;
  ret->_flag	       =  track.flag	     ;
  ret->_nHits	       =  track.nHits	     ;
  ret->_charge	       =  track.charge	     ;
  ret->_nHitsPoss      =  track.nHitsPoss    ;
  ret->_nHitsDedx      =  track.nHitsDedx    ;
  ret->_nHitsFit       =  track.nHitsFit     ;
  ret->_nSigmaPion     =  track.nSigmaPion   ;
  ret->_Tdca	       =  track.Tdca	     ;
  ret->_dcaZ	       =  track.dcaZ	     ;
  ret->_dcaD	       =  track.dcaD	     ;
  ret->_BField	       =  track.BField	     ;
  ret->_bemcRadius     =  track.bemcRadius   ;
  ret->_etaext	       =  track.etaext	     ;
  ret->_phiext	       =  track.phiext	     ;
  ret->_dEdx	       =  track.dEdx	     ;
  ret->_trackIndex     =  track.trackIndex   ;
  ret->_id             =  track.id           ;

  return ret;
}

}
