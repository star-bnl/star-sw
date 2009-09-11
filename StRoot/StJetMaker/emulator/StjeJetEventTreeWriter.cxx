//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 September 2009
//

#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StJets/StJet.h"
#include "StMuTrackFourVec.h"

#include "StMuTrackEmu.h"
#include "StMuTowerEmu.h"
#include "StSpinPool/StJetEvent/StJetEventTypes.h"

#include <StJetFinder/StProtoJet.h>

#include <StFourPMaker.h>
#include <StBET4pMaker.h>

#include <TTree.h>

#include "StjeJetEventTreeWriter.h"

ClassImp(StjeJetEventTreeWriter);

StjeJetEventTreeWriter::StjeJetEventTreeWriter(const string& outFileName)
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
  _outFile = new TFile(_OutFileName.c_str(), "recreate");
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
  static const StThreeVectorF noVertex(-999,-999,-999);
  if (StMuDst::event()->primaryVertexPosition() == noVertex) return;
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
  jetEvent.setRunId(StMuDst::event()->runId());
  jetEvent.setEventId(StMuDst::event()->eventId());
  for (list<StProtoJet>::iterator iJet = protoJetList->begin(); iJet != protoJetList->end(); ++iJet) fillJet(jetEvent, *iJet);
}

void StjeJetEventTreeWriter::fillJet(StJetEvent& jetEvent, StProtoJet& pj)
{
  StJetCandidate* jet = jetEvent.addJet(new StJetCandidate(StMuDst::event()->primaryVertexPosition().xyz(), pj.pt(), pj.eta(), pj.phi(), pj.e()));

  // Loop over jet particles
  StProtoJet::FourVecList& particleList = pj.list();
  for (StProtoJet::FourVecList::const_iterator iParticle = particleList.begin(); iParticle != particleList.end(); ++iParticle)  {
    const StMuTrackFourVec* particle = dynamic_cast<const StMuTrackFourVec*>(*iParticle);

    assert(particle && particle->getIndex() >= 0);

    if (StMuTrackEmu* t = particle->track()) {
      StJetTrack* track = jetEvent.addTrack(t);
      jet->addTrack(track)->setJet(jet);
    }

    if (StMuTowerEmu* t = particle->tower()) {
      StJetTower* tower = jetEvent.addTower(t);
      jet->addTower(tower)->setJet(jet);
    }
  } // End loop over jet particles
}
