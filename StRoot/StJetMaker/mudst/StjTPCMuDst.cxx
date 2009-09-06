// $Id: StjTPCMuDst.cxx,v 1.5 2009/09/06 15:32:29 pibero Exp $
#include "StjTPCMuDst.h"

#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <mudst/StMuEmcPosition.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

#include <TVector3.h>

StjTPCMuDst::StjTPCMuDst(StMuDstMaker* uDstMaker)
  : _uDstMaker(uDstMaker)
{

}

StjTrackList StjTPCMuDst::getTrackList()
{
  StjTrackList ret;

  StMuDst* uDst = _uDstMaker->muDst();

  long nTracks = uDst->numberOfPrimaryTracks();

  double magneticField = uDst->event()->magneticField()/10.0; // Tesla
  for(int i = 0; i < nTracks; ++i) {
    const StMuTrack* mutrack = uDst->primaryTracks(i);

    if(mutrack->flag() < 0) continue;

    if(mutrack->topologyMap().trackFtpcEast() || mutrack->topologyMap().trackFtpcWest()) continue;

    StjTrack track = createTrack(mutrack, i, magneticField);

    ret.push_back(track);
  }

  return ret;
}

StjTrack StjTPCMuDst::createTrack(const StMuTrack* mutrack, int i, double magneticField)
{
  StjTrack track;

  track.runNumber = _uDstMaker->muDst()->event()->runId();
  track.eventId = _uDstMaker->muDst()->event()->eventId();
  track.detectorId = 1;

  TVector3 p(mutrack->momentum().x(), mutrack->momentum().y(), mutrack->momentum().z());

  track.pt         = p.Pt();
  track.eta        = p.Eta();
  track.phi        = p.Phi();
  track.flag       = mutrack->flag();
  track.nHits      = mutrack->nHits(); 
  track.charge     = mutrack->charge();
  track.nHitsPoss  = mutrack->nHitsPoss();
  track.nHitsDedx  = mutrack->nHitsDedx();
  track.nHitsFit   = mutrack->nHitsFit();
  track.nSigmaPion = mutrack->nSigmaPion();
  track.nSigmaKaon = mutrack->nSigmaKaon();
  track.nSigmaProton = mutrack->nSigmaProton();
  track.nSigmaElectron = mutrack->nSigmaElectron();
  track.Tdca       = mutrack->dcaGlobal().mag();
  track.dcaZ       = mutrack->dcaZ();
  track.dcaD       = mutrack->dcaD();

  track.BField      = magneticField;
  //track.bemcRadius = StEmcGeom::instance("bemc")->Radius() + 5;

  // The optimum BEMC radius to use in extrapolating the track was determined to be 238.6 cm
  // (slightly behind the shower max plane) in Murad Sarsour's electron jets analysis.
  // http://cyclotron.tamu.edu/star/2006Jets/nov27_2007/details.html
  track.bemcRadius = 238.6;	// cm

  StThreeVectorF vertex = _uDstMaker->muDst()->event()->primaryVertexPosition();
  track.vertexZ = vertex.z(); 

  StThreeVectorD momentumAt, positionAt;
  StMuEmcPosition EmcPosition;
  if (EmcPosition.trackOnEmc(&positionAt, &momentumAt, mutrack, track.BField, track.bemcRadius))
    {
      track.exitDetectorId = 9;
      track.exitEta = positionAt.pseudoRapidity();
      track.exitPhi = positionAt.phi();
      int id(0);
      StEmcGeom::instance("bemc")->getId(track.exitPhi, track.exitEta, id);
      track.exitTowerId = id;
    }
  else if(EmcPosition.trackOnEEmc(&positionAt, &momentumAt, mutrack))
    {
      track.exitDetectorId = 13;
      track.exitEta = positionAt.pseudoRapidity();
      track.exitPhi = positionAt.phi();
      track.exitTowerId = 0; // todo 
    }
  else
    {
      track.exitDetectorId = 0;
      track.exitEta = -999;
      track.exitPhi = -999;
      track.exitTowerId = 0;
    }


  track.dEdx = mutrack->dEdx();

  track.trackIndex = i;

  track.id = mutrack->id();

  return track;
}
