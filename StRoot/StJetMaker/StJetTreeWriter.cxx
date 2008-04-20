// $Id: StJetTreeWriter.cxx,v 1.2 2008/04/20 21:38:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
# include "StJetTreeWriter.h"

#include "StJets.h"
#include "StJet.h"
#include "StMuTrackFourVec.h"
#include "StppJetAnalyzer.h"

#include <StJetFinder/StProtoJet.h>

#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

#include <StFourPMakers/StMuEmcPosition.h>
#include <StFourPMakers/StFourPMaker.h>
#include "StFourPMakers/StBET4pMaker.h"

#include <TTree.h>

namespace StSpinJet {

  StJetTreeWriter::StJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName)
  : _uDstMaker(uDstMaker)
  , _OutFileName(outFileName)
  , _outFile(0)
{

}

StJetTreeWriter::~StJetTreeWriter()
{

}

void StJetTreeWriter::Init()
{
  _outFile = new TFile(_OutFileName.c_str(), "recreate");
}

void StJetTreeWriter::Finish()
{
  _outFile->Write();
  _outFile->Close();
}

void StJetTreeWriter::fillJetTree(std::vector<AnalyzerCtl> &mAnalyzerCtl, TTree *mJetTree)
{
  for(std::vector<AnalyzerCtl>::iterator it = mAnalyzerCtl.begin(); it != mAnalyzerCtl.end(); ++it) {
    StppJetAnalyzer* analyzer = (*it).mAnalyzer;
    fillJetTreeForOneJetFindingAlgorithm(*(*it).mJets, analyzer);
  }
  mJetTree->Fill();
}

void StJetTreeWriter::fillJetTreeForOneJetFindingAlgorithm(StJets& jets, StppJetAnalyzer* analyzer)
{
  StFourPMaker* fourPMaker = analyzer->fourPMaker();

  jets.Clear();
  jets.setBemcCorrupt(fourPMaker->bemcCorrupt() );

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
	
  StppJetAnalyzer::JetList &cJets = analyzer->getJets();
  for(StppJetAnalyzer::JetList::iterator it = cJets.begin(); it != cJets.end(); ++it) {
    fillJet(jets, *it);
  }
}

void StJetTreeWriter::fillJet(StJets &jets, StProtoJet& pj)
{
  StJet aJet(pj.e(), pj.px(), pj.py(), pj.pz(), 0, 0);
  aJet.zVertex = _uDstMaker.muDst()->event()->primaryVertexPosition().z();

  StProtoJet::FourVecList &trackList = pj.list();
  for(StProtoJet::FourVecList::iterator it2 = trackList.begin(); it2 != trackList.end(); ++it2)  {
    StMuTrackFourVec *track = dynamic_cast<StMuTrackFourVec*>(*it2);
    if (!track) {
      cout <<"StJets::addProtoJet(). ERROR:\tcast to StMuTrackFourVecFailed.  no action"<<endl;
      return;
    }
    int muTrackIndex = track->getIndex();
    if (muTrackIndex <0) {
      cout <<"Error, muTrackIndex<0. abort()"<<endl;
      abort();
    }
      
    TrackToJetIndex t2j( jets.nJets(), muTrackIndex, track->detectorId() );
    t2j.SetPxPyPzE(track->px(), track->py(), track->pz(), track->e() );
      
    //and cache some properties if it really came from a StMuTrack:
    StMuTrack* muTrack = track->particle();
    if (muTrack) {  //this will fail for calorimeter towers

      double bField(0.5); // to put it in Tesla
      double rad(238.6);// geom->Radius()+5.;

      StThreeVectorD momentumAt, positionAt;
      StMuEmcPosition mMuPosition;
      mMuPosition.trackOnEmc(&positionAt, &momentumAt, muTrack, bField, rad );

      t2j.setCharge( muTrack->charge() );
      t2j.setNhits( muTrack->nHits() );
      t2j.setNhitsPoss( muTrack->nHitsPoss() );
      t2j.setNhitsDedx( muTrack->nHitsDedx() );
      t2j.setNhitsFit( muTrack->nHitsFit() );
      t2j.setNsigmaPion( muTrack->nSigmaPion() );
      t2j.setTdca ( muTrack->dcaGlobal().mag() );
      t2j.setTdcaz ( muTrack->dcaZ() );
      t2j.setTdcaxy ( muTrack->dcaD() );
      t2j.setetaext ( positionAt.pseudoRapidity() );
      t2j.setphiext ( positionAt.phi() );
    }
     
    jets.addTrackToIndex(t2j);

    StDetectorId mDetId = track->detectorId();
    if (mDetId==kTpcId) {
      aJet.nTracks++;
      aJet.tpcEtSum += track->eT();
    }
    else if (mDetId==kBarrelEmcTowerId) {
      aJet.nBtowers++;
      aJet.btowEtSum += track->eT();
    }
    else if (mDetId==kEndcapEmcTowerId) {
      aJet.nEtowers++;
      aJet.etowEtSum += track->eT();
    }
  }

  jets.addJet(aJet);

}

}
