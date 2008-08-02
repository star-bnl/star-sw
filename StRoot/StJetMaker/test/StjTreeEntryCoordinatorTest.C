// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTreeEntryCoordinator.h>

#include <StjBEMCTree.h>
#include <StjTPCTree.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>
#include <string>

#include "StjTreeEntryCoordinatorTest.hh"

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetTreeEntryCoordinatorTest );

void StJetTreeEntryCoordinatorTest::setUp() 
{

}

void StJetTreeEntryCoordinatorTest::tearDown() 
{

}

void StJetTreeEntryCoordinatorTest::testMake() 
{
  //  TFile* file = new TFile("/star/institutions/mit/tai/testData/jetpart_6143024.root");
  TFile* file = new TFile("./jetpart_6143024.root");

  StJetTreeEntryCoordinator* coord = new StJetTreeEntryCoordinator(file);
  coord->AddTrgTreeName("trgBJP2");
  coord->AddTrgTreeName("trgBHT2");

  CPPUNIT_ASSERT_EQUAL( string("runNumber"), string(coord->indexMajorName()) );
  CPPUNIT_ASSERT_EQUAL(   string("eventId"), string(coord->indexMinorName()) );

  const Int_t& indexMajor = coord->indexMajor();
  const Int_t& indexMinor = coord->indexMinor();

  TTree *treeTpc = dynamic_cast<TTree*>(file->Get("tpcTracks"));
  StJetTPCTree* tpc = new StJetTPCTree(treeTpc, indexMajor, indexMinor);

  TTree *treeBemc = dynamic_cast<TTree*>(file->Get("bemcTowers"));
  StJetBEMCTree* bemc = new StJetBEMCTree(treeBemc, indexMajor, indexMinor);

  coord->Init();

  while(!coord->eof()) {
    coord->Make();
    //    TrackList trackList = tpc->getTrackList();
    //    TowerEnergyList energyList = bemc->getEnergyList();
    //    cout << indexMajor << " " << indexMinor << " " << trackList.size() << " " << energyList.size() << endl;
  }

  delete bemc;
  delete tpc;
  delete file;
  delete coord;
}
