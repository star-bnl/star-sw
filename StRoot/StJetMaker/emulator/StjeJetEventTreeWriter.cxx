//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 September 2009
//

class StJets;

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
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

void StjeJetEventTreeWriter::addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name, StJets* stjets)
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

void StjeJetEventTreeWriter::fillJetTree()
{
  for(vector<AnalyzerCtl>::iterator iAnalyzer = _analyzerCtlList.begin(); iAnalyzer != _analyzerCtlList.end(); ++iAnalyzer) {
    StFourPMaker* fourPMaker = iAnalyzer->_fourPMaker;
    list<StProtoJet>* protoJetList = iAnalyzer->_protoJetList;
    fillJetTreeForOneJetFindingAlgorithm(*iAnalyzer->_jetEvent, protoJetList, fourPMaker);
  }
  _jetTree->Fill();
}

void StjeJetEventTreeWriter::fillJetTreeForOneJetFindingAlgorithm(StJetEvent& jetEvent, list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker)
{
  jetEvent.Clear();
  jetEvent.setRunId(fourPMaker->GetRunNumber());
  jetEvent.setEventId(fourPMaker->GetEventNumber());
  for (list<StProtoJet>::iterator iJet = protoJetList->begin(); iJet != protoJetList->end(); ++iJet)
    fillJet(jetEvent, *iJet, fourPMaker);
}

void StjeJetEventTreeWriter::fillJet(StJetEvent& jetEvent, StProtoJet& pj, StFourPMaker* fourPMaker)
{
  StJetCandidate* jet = jetEvent.addJet(new StJetCandidate(fourPMaker->getVertex().xyz(), pj.pt(), pj.eta(), pj.phi(), pj.e()));

  // Loop over jet particles
  StProtoJet::FourVecList& particleList = pj.list();
  for (StProtoJet::FourVecList::const_iterator iParticle = particleList.begin(); iParticle != particleList.end(); ++iParticle) {
    const StMuTrackFourVec* particle = dynamic_cast<const StMuTrackFourVec*>(*iParticle);

    if (StMuTrackEmu* t = particle->track()) {
      StJetTrack* track = jetEvent.newTrack();
      track->mId             = t->id();
      track->mDetectorId     = t->detectorId();
      track->mFlag           = t->flag();
      track->mCharge         = t->charge();
      track->mNHits          = t->nHits();
      track->mNHitsFit       = t->nHitsFit();
      track->mNHitsPoss      = t->nHitsPoss();
      track->mNHitsDedx      = t->nHitsDedx();
      track->mDedx           = t->dEdx();
      track->mNSigmaPion     = t->nSigmaPion();
      track->mNSigmaKaon     = t->nSigmaKaon();
      track->mNSigmaProton   = t->nSigmaProton();
      track->mNSigmaElectron = t->nSigmaElectron();
      track->mExitTowerId    = t->exitTowerId();
      track->mExitDetectorId = t->exitDetectorId();
      track->mExitPoint.SetPtEtaPhi(t->bemcRadius(),t->etaext(),t->phiext());
      track->mDca            = t->Tdca();
      track->mDcaZ           = t->dcaZ();
      track->mDcaD           = t->dcaD();
      TVector3 mom(t->px(),t->py(),t->pz());
      track->mPt             = mom.Pt();
      track->mEta            = mom.Eta();
      track->mPhi            = mom.Phi();
      jet->addTrack(track)->setJet(jet);
    }

    if (StMuTowerEmu* t = particle->tower()) {
      StJetTower* tower = jetEvent.newTower();
      tower->mId         = t->id();
      tower->mDetectorId = t->detectorId();
      tower->mAdc        = t->adc();
      tower->mPedestal   = t->pedestal();
      tower->mRms        = t->rms();
      tower->mStatus     = t->status();

      // StMuTowerEmu has the tower momentum from
      // the origin. Here we correct for vertex.

      TVector3 vertex(fourPMaker->getVertex().xyz());
      TVector3 mom(t->px(),t->py(),t->pz());
      float energy = mom.Mag();

      switch (t->detectorId()) {
      case kBarrelEmcTowerId:
	mom.SetPtEtaPhi(StEmcGeom::instance("bemc")->Radius(),mom.Eta(),mom.Phi());
	mom -= vertex;
	mom.SetMag(energy);
	break;
      case kEndcapEmcTowerId:
	mom.SetMag(EEmcGeomSimple::Instance().getZMean()/mom.Unit().z());
	mom -= vertex;
	mom.SetMag(energy);
	break;
      }

      tower->mPt  = mom.Pt();
      tower->mEta = mom.Eta();
      tower->mPhi = mom.Phi();
      jet->addTower(tower)->setJet(jet);
    }

    if (StMcTrackEmu* t = particle->mctrack()) {
      StJetParticle* part = jetEvent.newParticle();
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
}
