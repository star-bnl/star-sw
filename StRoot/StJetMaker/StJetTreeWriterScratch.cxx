// $Id: StJetTreeWriterScratch.cxx,v 1.2 2008/07/14 19:59:44 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetTreeWriterScratch.h"

#include "StJets.h"
#include "StJet.h"
#include "StMuTrackFourVec.h"

#include "StMuTrackEmu.h"

#include <StJetFinder/StProtoJet.h>

#include "./StFourPMakers/StFourPMaker.h"
#include "./StFourPMakers/StBET4pMaker.h"

#include <TTree.h>

using namespace std;
using namespace StSpinJet;

ClassImp(StJetTreeWriterScratch)

StJetTreeWriterScratch::StJetTreeWriterScratch()
{

}

StJetTreeWriterScratch::~StJetTreeWriterScratch()
{

}

void StJetTreeWriterScratch::addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name)
{
  AnalyzerCtl anaCtl;
  anaCtl._branchName = name;
  anaCtl._fourPMaker = fourPMaker;
  anaCtl._protoJetList = protoJetList;
  anaCtl._jets = new StJets();

  _analyzerCtlList.push_back(anaCtl);
}

void StJetTreeWriterScratch::Init()
{

}

void StJetTreeWriterScratch::Finish()
{

}

void StJetTreeWriterScratch::fillJetTree()
{
  for(vector<AnalyzerCtl>::iterator it = _analyzerCtlList.begin(); it != _analyzerCtlList.end(); ++it) {
    StFourPMaker* fourPMaker = (*it)._fourPMaker;
    std::list<StProtoJet>* protoJetList = (*it)._protoJetList;
    fillJetTreeForOneJetFindingAlgorithm(*(*it)._jets, protoJetList, fourPMaker);
  }
}

void StJetTreeWriterScratch::fillJetTreeForOneJetFindingAlgorithm(StJets& jets, std::list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker)
{
  jets.Clear();
  jets.setBemcCorrupt(fourPMaker->bemcCorrupt());

  for(list<StProtoJet>::iterator it = protoJetList->begin(); it != protoJetList->end(); ++it) {
    fillJet(jets, *it);
  }
}

void StJetTreeWriterScratch::fillJet(StJets &jets, StProtoJet& pj)
{
  StJet aJet(pj.e(), pj.px(), pj.py(), pj.pz(), 0, 0);

  cout << pj.e() << endl;

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

