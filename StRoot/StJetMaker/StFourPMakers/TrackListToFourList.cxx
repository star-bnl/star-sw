// $Id: TrackListToFourList.cxx,v 1.4 2008/07/15 04:19:53 tai Exp $
#include "TrackListToFourList.h"

#include "TrackToTLorentzVector.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackFourVec.h"

namespace StSpinJet {

TrackListToFourList::TrackListToFourList() 
  : _trackTo4p(*(new TrackToTLorentzVector)) { }


FourList TrackListToFourList::operator()(const TrackList& trackList)
{
  FourList ret;

  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {

    StMuTrackEmu *trackEmu = createTrackEmu(*track);

    TLorentzVector p4 = _trackTo4p(*track);

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
