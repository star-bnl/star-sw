// $Id: StDefaultJetTreeWriter.cxx,v 1.4 2008/08/02 21:26:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeDefaultJetTreeWriter.h"

#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

#include "StJets.h"
#include "StJet.h"
#include "StMuTrackFourVec.h"

#include "StMuTrackEmu.h"

#include <StJetFinder/StProtoJet.h>


#include <StFourPMaker.h>
#include <StBET4pMaker.h>

#include <TTree.h>

using namespace std;
using namespace StSpinJet;

StDefaultJetTreeWriter::StDefaultJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName)
  : _uDstMaker(uDstMaker)
  , _OutFileName(outFileName)
  , _jetTree(0)
  , _outFile(0)
{

}

StDefaultJetTreeWriter::~StDefaultJetTreeWriter()
{

}

void StDefaultJetTreeWriter::addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name, StJets* stjets)
{
  AnalyzerCtl anaCtl;
  anaCtl._branchName = name;
  anaCtl._fourPMaker = fourPMaker;
  anaCtl._protoJetList = protoJetList;
  anaCtl._jets = stjets;

  _analyzerCtlList.push_back(anaCtl);
}

void StDefaultJetTreeWriter::Init()
{
  _outFile = new TFile(_OutFileName.c_str(), "recreate");
  _jetTree  = new TTree("jet", "jetTree");

  for(vector<AnalyzerCtl>::iterator it = _analyzerCtlList.begin(); it != _analyzerCtlList.end(); ++it) {
    _jetTree->Branch((*it)._branchName.c_str(), "StJets", &((*it)._jets));
  }
}

void StDefaultJetTreeWriter::Finish()
{
  _outFile->Write();
  _outFile->Close();
  delete _outFile;
  _outFile = 0;
}

void StDefaultJetTreeWriter::fillJetTree()
{
  for(vector<AnalyzerCtl>::iterator it = _analyzerCtlList.begin(); it != _analyzerCtlList.end(); ++it) {
    StFourPMaker* fourPMaker = (*it)._fourPMaker;
    std::list<StProtoJet>* protoJetList = (*it)._protoJetList;
    fillJetTreeForOneJetFindingAlgorithm(*(*it)._jets, protoJetList, fourPMaker);
  }
  _jetTree->Fill();
}

void StDefaultJetTreeWriter::fillJetTreeForOneJetFindingAlgorithm(StJets& jets, std::list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker)
{
  jets.Clear();
  jets.setBemcCorrupt(fourPMaker->bemcCorrupt());

  StMuEvent* event = _uDstMaker.muDst()->event();
  jets.seteventId(event->eventId());
  jets.seteventNumber(event->eventNumber());
  jets.setrunId(event->runId());
  jets.setrunNumber(event->runNumber());

  StBET4pMaker* bet4p = dynamic_cast<StBET4pMaker*>(fourPMaker);
  if (bet4p) {
    jets.setDylanPoints( bet4p->nDylanPoints() );
    jets.setSumEmcE( bet4p->sumEmcEt() );
  }
	
  for(list<StProtoJet>::iterator it = protoJetList->begin(); it != protoJetList->end(); ++it) {
    fillJet(jets, *it);
  }
}

void StDefaultJetTreeWriter::fillJet(StJets &jets, StProtoJet& pj)
{
  StJet aJet(pj.e(), pj.px(), pj.py(), pj.pz(), 0, 0);
  aJet.zVertex = _uDstMaker.muDst()->event()->primaryVertexPosition().z();

  StProtoJet::FourVecList &particleList = pj.list();
  for(StProtoJet::FourVecList::iterator it2 = particleList.begin(); it2 != particleList.end(); ++it2)  {
    const StMuTrackFourVec *particle = dynamic_cast<const StMuTrackFourVec*>(*it2);
    if (!particle) {
      cout <<"StJets::addProtoJet(). ERROR:\tcast to StMuTrackFourVecFailed.  no action"<<endl;
      return;
    }
    int muTrackIndex = particle->getIndex();
    if (muTrackIndex <0) {
      cout <<"Error, muTrackIndex<0. abort()"<<endl;
      abort();
    }

    StDetectorId detectorId;
    int mDetId = particle->detectorId();
    if (mDetId==kTpcIdentifier)
      detectorId = kTpcId;
    else if (mDetId==kBarrelEmcTowerIdentifier)
      detectorId = kBarrelEmcTowerId;
    else if (mDetId==kEndcapEmcTowerIdentifier)
      detectorId = kEndcapEmcTowerId;
    else
      detectorId = kUnknownId;
      
    TrackToJetIndex t2j( jets.nJets(), muTrackIndex, detectorId );
    t2j.SetPxPyPzE(particle->px(), particle->py(), particle->pz(), particle->e() );
      
    StMuTrackEmu* track = particle->track();
    if (track) {
      t2j.setCharge( track->charge() );
      t2j.setNhits( track->nHits() );
      t2j.setNhitsPoss( track->nHitsPoss() );
      t2j.setNhitsDedx( track->nHitsDedx() );
      t2j.setNhitsFit( track->nHitsFit() );
      t2j.setNsigmaPion( track->nSigmaPion() );
      t2j.setTdca ( track->Tdca() );
      t2j.setTdcaz ( track->dcaZ() );
      t2j.setTdcaxy ( track->dcaD() );
      t2j.setetaext ( track->etaext() );
      t2j.setphiext ( track->phiext() );
      t2j.setdEdx ( track->dEdx() );
      t2j.setTrackID( track->id() );
    } else {
      t2j.setTowerID(muTrackIndex);
    }
     
    jets.addTrackToIndex(t2j);

    if (mDetId==kTpcIdentifier) {
      aJet.nTracks++;
      aJet.tpcEtSum += particle->eT();
    }
    else if (mDetId==kBarrelEmcTowerIdentifier) {
      aJet.nBtowers++;
      aJet.btowEtSum += particle->eT();
    }
    else if (mDetId==kEndcapEmcTowerIdentifier) {
      aJet.nEtowers++;
      aJet.etowEtSum += particle->eT();
    }
  }

  jets.addJet(aJet);

}

