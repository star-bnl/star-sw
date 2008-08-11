// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTreeEntryCoordinator.h>

#include <StjTreeIndex.h>
#include <StjTreeIndexList.h>
#include <StjTreeIndexListCreator.h>

#include <StjBEMCTree.h>
#include <StjTPCTree.h>

#include <StjTrackListReader.h>
#include <StjTowerEnergyListReader.h>

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

  TTree *tpctree = dynamic_cast<TTree*>(testDir->Get("tpcTracks"));
  StjTrackListReader *tpcreader = new StjTrackListReader(tpctree);
  StjTPCTree* tpc = new StjTPCTree(tpcreader);
  coord->AddReader(tpcreader);

  TTree *bemctree = dynamic_cast<TTree*>(testDir->Get("bemcTowers"));
  StjTowerEnergyListReader *bemcreader = new StjTowerEnergyListReader(bemctree);
  StjBEMCTree* bemc = new StjBEMCTree(bemcreader);
  coord->AddReader(bemcreader);

  coord->Init();

  while(!coord->eof()) {
    coord->Make();
    //    StjTrackList trackList = tpc->getTrackList();
    //    StjTowerEnergyList energyList = bemc->getEnergyList();
    //    cout << trackList.size() << " " << energyList.size() << endl;
  }

}
