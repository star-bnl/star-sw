// $Id: StjTPCMuDst.cxx,v 1.10 2012/12/23 16:48:18 pibero Exp $
#include "StjTPCMuDst.h"

#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include <mudst/StMuEmcPosition.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include <TVector3.h>

int StjTPCMuDst::currentVertexIndex() const
{
  return StMuDst::currentVertexIndex();
}

void StjTPCMuDst::setVertexIndex(int i)
{
  StMuDst::setVertexIndex(i);
}

int StjTPCMuDst::numberOfVertices() const
{
  return StMuDst::numberOfPrimaryVertices();
}

StjPrimaryVertex StjTPCMuDst::getVertex() const
{
  StjPrimaryVertex vertex;

  if (StMuPrimaryVertex* muVertex = StMuDst::primaryVertex()) {
    vertex.mPosition              = muVertex->position().xyz();
    vertex.mPosError              = muVertex->posError().xyz();
    vertex.mVertexFinderId        = muVertex->vertexFinderId();
    vertex.mRanking               = muVertex->ranking();
    vertex.mNTracksUsed           = muVertex->nTracksUsed();
    vertex.mNBTOFMatch            = muVertex->nBTOFMatch();
    vertex.mNCTBMatch             = muVertex->nCTBMatch();
    vertex.mNBEMCMatch            = muVertex->nBEMCMatch();
    vertex.mNEEMCMatch            = muVertex->nEEMCMatch();
    vertex.mNCrossCentralMembrane = muVertex->nCrossCentralMembrane();
    vertex.mSumTrackPt            = muVertex->sumTrackPt();
    vertex.mMeanDip               = muVertex->meanDip();
    vertex.mChiSquared            = muVertex->chiSquared();
    vertex.mRefMultPos            = muVertex->refMultPos();
    vertex.mRefMultNeg            = muVertex->refMultNeg();
    vertex.mRefMultFtpcEast       = muVertex->refMultFtpcEast();
    vertex.mRefMultFtpcWest       = muVertex->refMultFtpcWest();
  }

  return vertex;
}

StjTrackList StjTPCMuDst::getTrackList()
{
  StjTrackList ret;

  int nTracks = StMuDst::numberOfPrimaryTracks();

  double magneticField = StMuDst::event()->magneticField()/10.0; // Tesla
  for(int i = 0; i < nTracks; ++i) {
    const StMuTrack* mutrack = StMuDst::primaryTracks(i);

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

  track.runNumber = StMuDst::event()->runId();
  track.eventId = StMuDst::event()->eventId();
  track.detectorId = kTpcId;

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
  track.dcaX       = mutrack->dcaGlobal().x();
  track.dcaY       = mutrack->dcaGlobal().y();
  track.dcaZ       = mutrack->dcaZ();
  track.dcaD       = mutrack->dcaD();
  track.chi2       = mutrack->chi2();
  track.chi2prob   = mutrack->chi2prob();
  track.BField     = magneticField;

  // The optimum BEMC radius to use in extrapolating the track was determined to be 238.6 cm
  // (slightly behind the shower max plane) in Murad Sarsour's electron jets analysis.
  // http://cyclotron.tamu.edu/star/2006Jets/nov27_2007/details.html

  track.bemcRadius = 238.6;	// cm

  StThreeVectorF vertex = StMuDst::primaryVertex()->position();
  track.vertexZ = vertex.z(); 

  StThreeVectorD momentumAt, positionAt;
  StMuEmcPosition EmcPosition;

  if (EmcPosition.trackOnEmc(&positionAt, &momentumAt, mutrack, magneticField, track.bemcRadius))
    {
      track.exitDetectorId = kBarrelEmcTowerId;
      track.exitEta = positionAt.pseudoRapidity();
      track.exitPhi = positionAt.phi();
      StEmcGeom::instance("bemc")->getId(track.exitPhi, track.exitEta, track.exitTowerId);
    }
  else if (EmcPosition.trackOnEEmc(&positionAt, &momentumAt, mutrack, magneticField, kEEmcZSMD))
    {
      track.exitDetectorId = kEndcapEmcTowerId;
      track.exitEta = positionAt.pseudoRapidity();
      track.exitPhi = positionAt.phi();
      int sector, subsector, etabin;
      EEmcGeomSimple::Instance().getTower(positionAt.xyz(),sector,subsector,etabin);
      track.exitTowerId = sector*60+subsector*12+etabin;
    }
  else
    {
      track.exitDetectorId = -999;
      track.exitEta = -999;
      track.exitPhi = -999;
      track.exitTowerId = -999;
    }

  track.dEdx = mutrack->dEdx();
  track.beta = mutrack->globalTrack() ? mutrack->globalTrack()->btofPidTraits().beta() : 0;
  track.trackIndex = i;
  track.id = mutrack->id();
  track.firstPoint = mutrack->firstPoint().xyz();
  track.lastPoint  = mutrack->lastPoint().xyz();

  return track;
}
