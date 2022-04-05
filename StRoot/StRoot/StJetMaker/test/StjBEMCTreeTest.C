// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StjBEMCTree.h>
#include "StjBEMCTreeTest.hh"

#include <StjTowerEnergyListReader.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjBEMCTreeTest );

void StjBEMCTreeTest::setUp()
{

}

void StjBEMCTreeTest::tearDown()
{

}


void StjBEMCTreeTest::testGetEntry() 
{
  TFile* file = new TFile("./part_run6143024.root");

  TTree *tree = dynamic_cast<TTree*>(file->Get("bemcTowers"));

  StjTowerEnergyListReader *reader = new StjTowerEnergyListReader(tree);

  tree->BuildIndex("runNumber", "eventId");

  reader->Init();

  StjBEMCTree* bemc = new StjBEMCTree(reader);

  reader->GetEntryWithIndex(6143024, 38);
  StjTowerEnergyList energyList = bemc->getEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)65, energyList.size() );

  reader->GetEntryWithIndex(6143024, 41);
  energyList = bemc->getEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, energyList.size() );


  delete bemc;
  delete file;
}

