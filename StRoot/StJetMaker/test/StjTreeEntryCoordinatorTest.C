// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTreeEntryCoordinator.h>

#include <StjTreeIndex.h>
#include <StjTreeIndexList.h>
#include <StjTreeIndexListCreator.h>

#include <StjBEMCTree.h>
#include <StjTPCTree.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>
#include <string>

#include "StjTreeEntryCoordinatorTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTreeEntryCoordinatorTest );

void StjTreeEntryCoordinatorTest::setUp() 
{

}

void StjTreeEntryCoordinatorTest::tearDown() 
{

}

void StjTreeEntryCoordinatorTest::testOne() 
{
  TFile* testDir = new TFile("./part_run6143024.root");
  StjTreeIndexListCreator idxCreator(testDir);
  idxCreator.AddTrgTreeName("trgBHT2");
  idxCreator.AddTrgTreeName("trgBJP2");
  StjTreeIndexList idxList = idxCreator.create();

  StjTreeEntryCoordinator* coord = new StjTreeEntryCoordinator(idxList);

}

void StjTreeEntryCoordinatorTest::testMake() 
{
  TFile* file = new TFile("./part_run6143024.root");

  StjTreeEntryCoordinator* coord = new StjTreeEntryCoordinator(file);
  coord->AddTrgTreeName("trgBJP2");
  coord->AddTrgTreeName("trgBHT2");

  CPPUNIT_ASSERT_EQUAL( string("runNumber"), string(coord->indexMajorName()) );
  CPPUNIT_ASSERT_EQUAL(   string("eventId"), string(coord->indexMinorName()) );

  const Int_t& indexMajor = coord->indexMajor();
  const Int_t& indexMinor = coord->indexMinor();

  TTree *treeTpc = dynamic_cast<TTree*>(file->Get("tpcTracks"));
  StjTPCTree* tpc = new StjTPCTree(treeTpc, indexMajor, indexMinor);

  TTree *treeBemc = dynamic_cast<TTree*>(file->Get("bemcTowers"));
  StjBEMCTree* bemc = new StjBEMCTree(treeBemc, indexMajor, indexMinor);

  coord->Init();

  while(!coord->eof()) {
    coord->Make();
    //    StjTrackList trackList = tpc->getTrackList();
    //    StjTowerEnergyList energyList = bemc->getEnergyList();
    //    cout << indexMajor << " " << indexMinor << " " << trackList.size() << " " << energyList.size() << endl;
  }

  delete bemc;
  delete tpc;
  delete file;
  delete coord;
}
