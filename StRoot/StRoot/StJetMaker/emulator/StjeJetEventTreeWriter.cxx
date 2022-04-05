//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 September 2009
//

class StJets;

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuTrackFourVec.h"

#include "StMuTrackEmu.h"
#include "StMuTowerEmu.h"
#include "StMcTrackEmu.h"
#include "StSpinPool/StJetEvent/StJetEventTypes.h"

#include "StJetFinder/StProtoJet.h"

#include "StFourPMaker.h"
#include "StBET4pMaker.h"

#include "TFile.h"
#include "TTree.h"

#include "StjeJetEventTreeWriter.h"

ClassImp(StjeJetEventTreeWriter);

StjeJetEventTreeWriter::StjeJetEventTreeWriter(const char* outFileName)
  : _OutFileName(outFileName)
  , _jetTree(0)
  , _outFile(0)
{
}

void StjeJetEventTreeWriter::addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name, StJets*)
{
  AnalyzerCtl anaCtl;

  anaCtl._branchName = name;
  anaCtl._fourPMaker = fourPMaker;
  anaCtl._protoJetList = protoJetList;
  anaCtl._jetEvent = new StJetEvent;

  _analyzerCtlList.push_back(anaCtl);
}

void StjeJetEventTreeWriter::Init()
{
  _outFile = new TFile(_OutFileName, "recreate");
  _jetTree = new TTree("jet", "jetTree");

  for (vector<AnalyzerCtl>::iterator iAnalyzer = _analyzerCtlList.begin(); iAnalyzer != _analyzerCtlList.end(); ++iAnalyzer)
    _jetTree->Branch(iAnalyzer->_branchName.c_str(), "StJetEvent", &iAnalyzer->_jetEvent);

  _jetTree->BranchRef();
}

void StjeJetEventTreeWriter::Finish()
{
  _outFile->Write();
  _outFile->Close();
  delete _outFile;
  _outFile = 0;
}

void StjeJetEventTreeWriter::fillJetTreeHeader(int iAnalyzer)
{
  StJetEvent* jetEvent = _analyzerCtlList[iAnalyzer]._jetEvent;
  StFourPMaker* fourPMaker = _analyzerCtlList[iAnalyzer]._fourPMaker;
  jetEvent->Clear();
  jetEvent->mRunId   = fourPMaker->GetRunNumber();
  jetEvent->mEventId = fourPMaker->GetEventNumber();
  jetEvent->mDatime  = fourPMaker->GetDateTime();
}

void StjeJetEventTreeWriter::fillJetTree(int iAnalyzer, int iVertex)
{
  AnalyzerCtl& analyzerCtl = _analyzerCtlList[iAnalyzer];
  fillJetTreeForOneVertex(analyzerCtl._jetEvent,analyzerCtl._protoJetList,analyzerCtl._fourPMaker,iVertex);
}

void StjeJetEventTreeWriter::fillJetTreeForOneVertex(StJetEvent* jetEvent, list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker, int iVertex)
{
  StJetVertex* jetVertex = jetEvent->newVertex();
  copyVertex(fourPMaker->getVertexNodes()[iVertex].vertex,jetVertex);
  for (list<StProtoJet>::iterator protojet = protoJetList->begin(); protojet != protoJetList->end(); ++protojet)
    jetVertex->addJet(fillJet(jetEvent,jetVertex,*protojet));
}

StJetCandidate* StjeJetEventTreeWriter::fillJet(StJetEvent* jetEvent, StJetVertex* jetVertex, StProtoJet& protojet)
{
  StJetCandidate* jet = jetEvent->newJet(jetVertex->position(),TLorentzVector(protojet.px(),protojet.py(),protojet.pz(),protojet.e()));

  // Loop over jet particles
  const StProtoJet::FourVecList& particleList = protojet.list();
  for (StProtoJet::FourVecList::const_iterator iParticle = particleList.begin(); iParticle != particleList.end(); ++iParticle) {
    const StMuTrackFourVec* particle = dynamic_cast<const StMuTrackFourVec*>(*iParticle);

    if (StMuTrackEmu* t = particle->track()) {
      StJetTrack* track = jetEvent->newTrack();
      track->mId             = t->id();
      track->mDetectorId     = t->detectorId();
      track->mFlag           = t->flag();
      track->mCharge         = t->charge();
      track->mNHits          = t->nHits();
      track->mNHitsFit       = t->nHitsFit();
      track->mNHitsPoss      = t->nHitsPoss();
      track->mNHitsDedx      = t->nHitsDedx();
      track->mDedx           = t->dEdx();
      track->mBeta           = t->beta();
      track->mFirstPoint     = t->firstPoint();
      track->mLastPoint      = t->lastPoint();
      track->mExitTowerId    = t->exitTowerId();
      track->mExitDetectorId = t->exitDetectorId();
      track->mExitPoint.SetPtEtaPhi(t->bemcRadius(),t->etaext(),t->phiext());
      track->mDca.SetXYZ(t->dcaX(),t->dcaY(),t->dcaZ());
      track->mDcaD           = t->dcaD();
      track->mChi2           = t->chi2();
      track->mChi2Prob       = t->chi2prob();
      TVector3 mom(t->px(),t->py(),t->pz());
      track->mPt             = mom.Pt();
      track->mEta            = mom.Eta();
      track->mPhi            = mom.Phi();
      track->mNSigmaPion     = t->nSigmaPion();
      track->mNSigmaKaon     = t->nSigmaKaon();
      track->mNSigmaProton   = t->nSigmaProton();
      track->mNSigmaElectron = t->nSigmaElectron();
      jet->addTrack(track)->setJet(jet);
    }

    if (StMuTowerEmu* t = particle->tower()) {
      StJetTower* tower = jetEvent->newTower();
      tower->mId         = t->id();
      tower->mDetectorId = t->detectorId();
      tower->mAdc        = t->adc();
      tower->mPedestal   = t->pedestal();
      tower->mRms        = t->rms();
      tower->mStatus     = t->status();

      // StMuTowerEmu has the tower momentum from
      // the origin. Here we correct for vertex.

      TVector3 mom(t->px(),t->py(),t->pz());
      float energy = mom.Mag();

      switch (t->detectorId()) {
      case kBarrelEmcTowerId:
	mom.SetPtEtaPhi(StEmcGeom::instance("bemc")->Radius(),mom.Eta(),mom.Phi());
	break;
      case kEndcapEmcTowerId:
	mom.SetMag(EEmcGeomSimple::Instance().getZMean()/mom.Unit().z());
	break;
      }

      mom -= jetVertex->position();
      mom.SetMag(energy);

      tower->mPt  = mom.Pt();
      tower->mEta = mom.Eta();
      tower->mPhi = mom.Phi();
      jet->addTower(tower)->setJet(jet);
    }

    if (StMcTrackEmu* t = particle->mctrack()) {
      StJetParticle* part = jetEvent->newParticle();
      part->mId     = t->id();
      part->mPt     = t->pt();
      part->mEta    = t->eta();
      part->mPhi    = t->phi();
      part->mM      = t->m();
      part->mE      = t->e();
      part->mPdg    = t->pdg();
      part->mStatus = t->status();
      jet->addParticle(part)->setJet(jet);
    }
  } // End loop over jet particles

  float sumTowerPt = jet->sumTowerPt();
  float sumTrackPt = jet->sumTrackPt();

  jet->mRt = sumTowerPt/(sumTowerPt+sumTrackPt);

  jet->setVertex(jetVertex);

  return jet;
}

void StjeJetEventTreeWriter::copyVertex(const StMuPrimaryVertex* muVertex, StJetVertex* jetVertex)
{
  jetVertex->mPosition = muVertex->position().xyz();
  jetVertex->mPosError = muVertex->posError().xyz();
  jetVertex->mVertexFinderId = muVertex->vertexFinderId();
  jetVertex->mRanking = muVertex->ranking();
  jetVertex->mNTracksUsed = muVertex->nTracksUsed();
  jetVertex->mNBTOFMatch = muVertex->nBTOFMatch();
  jetVertex->mNCTBMatch = muVertex->nCTBMatch();
  jetVertex->mNBEMCMatch = muVertex->nBEMCMatch();
  jetVertex->mNEEMCMatch = muVertex->nEEMCMatch();
  jetVertex->mNCrossCentralMembrane = muVertex->nCrossCentralMembrane();
  jetVertex->mSumTrackPt = muVertex->sumTrackPt();
  jetVertex->mMeanDip = muVertex->meanDip();
  jetVertex->mChiSquared = muVertex->chiSquared();
  jetVertex->mRefMultPos = muVertex->refMultPos();
  jetVertex->mRefMultNeg = muVertex->refMultNeg();
  jetVertex->mRefMultFtpcWest = muVertex->refMultFtpcWest();
  jetVertex->mRefMultFtpcEast = muVertex->refMultFtpcEast();
}
