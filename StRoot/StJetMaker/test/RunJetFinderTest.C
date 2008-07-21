// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <RunJetFinder.h>

#include <StJetTreeEntryCoordinator.h>
#include <StJetBEMCTree.h>
#include <StJetTPCTree.h>
#include <TrackTowerEnergyListToFourVecList.h>


#include <StProtoJet.h>
#include <StConePars.h>
#include <StConeJetFinder.h>

#include <TFile.h>
#include <TTree.h>


#include "TLorentzVectorForJet.h"


#include <iostream>
#include <string>
#include <utility>
#include <list>

#include "RunJetFinderTest.hh"

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( RunJetFinderTest );

void RunJetFinderTest::setUp() 
{

}

void RunJetFinderTest::tearDown()
{

}


void RunJetFinderTest::testRun()
{
  //  TFile* file = new TFile("/star/institutions/mit/tai/testData/jetpart_6143024.root");
  TFile* file = new TFile("./jetpart_6143024.root");

  StJetTreeEntryCoordinator* coord = new StJetTreeEntryCoordinator(file);
  coord->AddTrgTreeName("trgBJP2");
  coord->AddTrgTreeName("trgBHT2");

  const Int_t& indexMajor = coord->indexMajor();
  const Int_t& indexMinor = coord->indexMinor();

  TTree *treeTpc = dynamic_cast<TTree*>(file->Get("tpcTracks"));
  StJetTPCTree* tpc = new StJetTPCTree(treeTpc, indexMajor, indexMinor);

  TTree *treeBemc = dynamic_cast<TTree*>(file->Get("bemcTowers"));
  StJetBEMCTree* bemc = new StJetBEMCTree(treeBemc, indexMajor, indexMinor);

  TrackTowerEnergyListToFourVecList toP4;

  StConePars* cpars = new StConePars();
  cpars->setGridSpacing(56, -1.6, 1.6, 120, -3.141592613589793, 3.141592613589793);
  cpars->setConeRadius(0.4);
  cpars->setSeedEtMin(0.5);
  cpars->setAssocEtMin(0.1);
  cpars->setSplitFraction(0.5);
  cpars->setPerformMinimization(true);
  cpars->setAddMidpoints(true);
  cpars->setRequireStableMidpoints(true);
  cpars->setDoSplitMerge(true);
  cpars->setDebug(false);
  RunJetFinder jetFinder(cpars);
  jetFinder.Init();

  coord->Init();

  for (Long64_t i = 0; i < 10; ++i) {
    if(coord->eof()) break;
    coord->Make();
    TrackList trackList = tpc->getTrackList();
    TowerEnergyList energyList = bemc->getEnergyList();
    
    FourVecList fourList = toP4(trackList, energyList);

    JetList jetList = jetFinder(fourList);

    for(JetList::const_iterator it = jetList.begin(); it != jetList.end(); ++it) {
      cout 
	<< (*it).runNumber << " "
	<< (*it).eventId  << " "
	<< (*it).jetId   << " "
    	<< (*it).pt     << " "
    	<< (*it).eta   << " "
    	<< (*it).phi  << " "
    	<< (*it).m   << " "
    	<< endl;
      for(FourVecList::const_iterator jt = (*it).fourVecList.begin(); jt != (*it).fourVecList.end(); ++jt) {
	cout 
	  << "       "
	  << (*jt).runNumber  << " "
	  << (*jt).eventId    << " "
	  << (*jt).fourvecId  << " "
	  << (*jt).type       << " "
	  << (*jt).detectorId << " "
	  << (*jt).trackId   << " "
	  << (*jt).towerId  << " "
	  << (*jt).pt      << " "
	  << (*jt).eta    << " "
	  << (*jt).phi   << " "
	  << (*jt).m   << " "
	  << endl;
      }

    }

  }

  delete bemc;
  delete tpc;
  delete file;
  delete coord;
}
