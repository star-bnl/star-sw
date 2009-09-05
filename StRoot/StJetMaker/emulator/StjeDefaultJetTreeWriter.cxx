// $Id: StjeDefaultJetTreeWriter.cxx,v 1.7 2009/09/05 22:16:14 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeDefaultJetTreeWriter.h"

#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StJets/StJet.h"
#include "StMuTrackFourVec.h"

#include "StMuTrackEmu.h"
#include "StMuTowerEmu.h"

#include <StJetFinder/StProtoJet.h>


#include <StFourPMaker.h>
#include <StBET4pMaker.h>

#include <TTree.h>

using namespace std;

StjeDefaultJetTreeWriter::StjeDefaultJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName)
  : _uDstMaker(uDstMaker)
  , _OutFileName(outFileName)
  , _jetTree(0)
  , _outFile(0)
{

}

StjeDefaultJetTreeWriter::~StjeDefaultJetTreeWriter()
{

}

void StjeDefaultJetTreeWriter::addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name, StJets* stjets)
{
  AnalyzerCtl anaCtl;
  anaCtl._branchName = name;
  anaCtl._fourPMaker = fourPMaker;
  anaCtl._protoJetList = protoJetList;
  anaCtl._jets = stjets;

  _analyzerCtlList.push_back(anaCtl);
}

void StjeDefaultJetTreeWriter::Init()
{
  _outFile = new TFile(_OutFileName.c_str(), "recreate");
  _jetTree  = new TTree("jet", "jetTree");

  for(vector<AnalyzerCtl>::iterator it = _analyzerCtlList.begin(); it != _analyzerCtlList.end(); ++it) {
    _jetTree->Branch((*it)._branchName.c_str(), "StJets", &((*it)._jets));
  }

  _jetTree->BranchRef();
}

void StjeDefaultJetTreeWriter::Finish()
{
  _outFile->Write();
  _outFile->Close();
  delete _outFile;
  _outFile = 0;
}

void StjeDefaultJetTreeWriter::fillJetTree()
{
  static const StThreeVectorF noVertex(-999,-999,-999);
  if (StMuDst::event()->primaryVertexPosition() == noVertex) return;
  for(vector<AnalyzerCtl>::iterator it = _analyzerCtlList.begin(); it != _analyzerCtlList.end(); ++it) {
    StFourPMaker* fourPMaker = (*it)._fourPMaker;
    std::list<StProtoJet>* protoJetList = (*it)._protoJetList;
    fillJetTreeForOneJetFindingAlgorithm(*(*it)._jets, protoJetList, fourPMaker);
  }
  _jetTree->Fill();
}

void StjeDefaultJetTreeWriter::fillJetTreeForOneJetFindingAlgorithm(StJets& jets, std::list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker)
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

void StjeDefaultJetTreeWriter::fillJet(StJets &stjets, StProtoJet& pj)
{
  StJet aJet(StJet(pj.e(), pj.px(), pj.py(), pj.pz(), 0, 0));
  stjets.addJet(aJet);
  TClonesArray* jets = stjets.jets();
  StJet* jet = (StJet*)jets->Last();
  jet->zVertex = _uDstMaker.muDst()->event()->primaryVertexPosition().z();

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
      
    StMuTrackEmu* track = particle->track();
    if (track) {
      TrackToJetIndex t2j( jets->GetLast(), muTrackIndex, detectorId, jet );

      t2j.SetPxPyPzE(particle->px(), particle->py(), particle->pz(), particle->e() );
      t2j.setTrackId( track->id() );
      t2j.setFlag( track->flag() );
      t2j.setCharge( track->charge() );
      t2j.setNhits( track->nHits() );
      t2j.setNhitsPoss( track->nHitsPoss() );
      t2j.setNhitsDedx( track->nHitsDedx() );
      t2j.setNhitsFit( track->nHitsFit() );
      t2j.setNsigmaPion( track->nSigmaPion() );
      t2j.setNsigmaElectron( track->nSigmaElectron() );
      t2j.setNsigmaKaon( track->nSigmaKaon() );
      t2j.setNsigmaProton( track->nSigmaProton() );
      t2j.setTdca ( track->Tdca() );
      t2j.setTdcaz ( track->dcaZ() );
      t2j.setTdcaxy ( track->dcaD() );
      t2j.setetaext ( track->etaext() );
      t2j.setphiext ( track->phiext() );
      t2j.setdEdx ( track->dEdx() );
      t2j.setTrackId( track->id() );

      stjets.addTrackToIndex(t2j); // for backward compatibility
      jet->addTrack((TrackToJetIndex*)stjets.tracks()->Last());
    }

    StMuTowerEmu* tower = particle->tower();
    if (tower) {
      TowerToJetIndex t2j(jets->GetLast());

      t2j.SetPxPyPzE(particle->px(), particle->py(), particle->pz(), particle->e());
      t2j.setTowerId(tower->id());
      t2j.setDetectorId(tower->detectorId());
      t2j.setAdc(tower->adc());
      t2j.setPedestal(tower->pedestal());
      t2j.setRms(tower->rms());
      t2j.setStatus(tower->status());

      stjets.addTowerToIndex(t2j); // for backward compatibility
      jet->addTower((TowerToJetIndex*)stjets.towers()->Last());
    }

    if (mDetId==kTpcIdentifier) {
      jet->nTracks++;
      jet->tpcEtSum += particle->eT();
    }
    else if (mDetId==kBarrelEmcTowerIdentifier) {
      jet->nBtowers++;
      jet->btowEtSum += particle->eT();
    }
    else if (mDetId==kEndcapEmcTowerIdentifier) {
      jet->nEtowers++;
      jet->etowEtSum += particle->eT();
    }
  }
}
