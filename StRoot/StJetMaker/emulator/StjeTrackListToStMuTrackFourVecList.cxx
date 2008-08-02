// $Id: StjeTrackListToStMuTrackFourVecList.cxx,v 1.1 2008/08/02 23:10:21 tai Exp $
#include "StjeTrackListToStMuTrackFourVecList.h"

#include "StjTrackToTLorentzVector.h"

#include "../emulator/StMuTrackEmu.h"
#include "../emulator/StMuTrackFourVec.h"

namespace StSpinJet {

StjeTrackListToStMuTrackFourVecList::StjeTrackListToStMuTrackFourVecList() 
  : _trackTo4p(*(new StjTrackToTLorentzVector)) { }


FourList StjeTrackListToStMuTrackFourVecList::operator()(const StjTrackList& trackList)
{
  FourList ret;

  for(StjTrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {

    StMuTrackEmu *trackEmu = createTrackEmu(*track);

    TLorentzVector p4 = _trackTo4p(*track);

    StMuTrackFourVec* pmu = new StMuTrackFourVec(trackEmu, p4, (*track).charge, (*track).trackIndex, kTpcId);
    ret.push_back(pmu);
  }

  return ret;
}

StMuTrackEmu* StjeTrackListToStMuTrackFourVecList::createTrackEmu(const StjTrack& track)
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
  ret->_etaext	       =  track.exitEta      ;
  ret->_phiext	       =  track.exitPhi      ;
  ret->_dEdx	       =  track.dEdx	     ;
  ret->_trackIndex     =  track.trackIndex   ;
  ret->_id             =  track.id           ;

  return ret;
}

}
