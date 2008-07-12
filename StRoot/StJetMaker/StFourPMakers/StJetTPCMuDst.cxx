// $Id: StJetTPCMuDst.cxx,v 1.3 2008/07/12 01:32:07 tai Exp $
#include "StJetTPCMuDst.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include <StFourPMakers/StMuEmcPosition.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

namespace StSpinJet {

StJetTPCMuDst::StJetTPCMuDst(StMuDstMaker* uDstMaker)
  : _uDstMaker(uDstMaker)
{

}

TrackList StJetTPCMuDst::getTrackList()
{
  TrackList ret;

  StMuDst* uDst = _uDstMaker->muDst();

  long nTracks = uDst->numberOfPrimaryTracks();

  double magneticField = uDst->event()->magneticField()/10.0; // Tesla
  for(int i = 0; i < nTracks; ++i) {
    const StMuTrack* mutrack = uDst->primaryTracks(i);

    if(mutrack->flag() < 0) continue;

    if(mutrack->topologyMap().trackFtpcEast() || mutrack->topologyMap().trackFtpcWest()) continue;

    //    StMuTrackEmu* trackEmu = StMuTrackEmuFactory::createStMuTrackEmu(track, i, magneticField);
    Track track;
    track.runNumber = _uDstMaker->muDst()->event()->runId();
    track.eventId = _uDstMaker->muDst()->event()->eventId();

    track.px         = mutrack->momentum().x();
    track.py         = mutrack->momentum().y();
    track.pz         = mutrack->momentum().z();
    track.flag       = mutrack->flag();
    track.nHits      = mutrack->nHits(); 
    track.charge     = mutrack->charge();
    track.nHitsPoss  = mutrack->nHitsPoss();
    track.nHitsDedx  = mutrack->nHitsDedx();
    track.nHitsFit   = mutrack->nHitsFit();
    track.nSigmaPion = mutrack->nSigmaPion();
    track.Tdca       = mutrack->dcaGlobal().mag();
    track.dcaZ       = mutrack->dcaZ();
    track.dcaD       = mutrack->dcaD();

    track.BField      = magneticField;
    track.bemcRadius = StEmcGeom::instance("bemc")->Radius() + 5;

    StThreeVectorD momentumAt, positionAt;
    StMuEmcPosition EmcPosition;
    if (EmcPosition.trackOnEmc(&positionAt, &momentumAt, mutrack, track.BField, track.bemcRadius) ||
	EmcPosition.trackOnEEmc(&positionAt, &momentumAt, mutrack))
      {
	track.etaext = positionAt.pseudoRapidity();
	track.phiext = positionAt.phi();
      }
    else
      {
	track.etaext = -999;
	track.phiext = -999;
      }


    track.dEdx = mutrack->dEdx();

    track.trackIndex = i;

    track.id = mutrack->id();


    ret.push_back(track);
  }

  return ret;
}


}
