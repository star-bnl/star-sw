// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StjBEMCTree.h>
#include "StjBEMCTreeTest.hh"

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
  //  TFile* file = new TFile("/star/institutions/mit/tai/testData/jetpart_6143024.root");
  TFile* file = new TFile("./jetpart_6143024.root");

  TTree *tree = dynamic_cast<TTree*>(file->Get("bemcTowers"));

  Int_t runNumber = 6143024;
  Int_t evenId;
  StjBEMCTree* bemc = new StjBEMCTree(tree, runNumber, evenId);


  evenId = 38;
  StjTowerEnergyList energyList = bemc->getEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)65, energyList.size() );

  evenId = 41;
  energyList = bemc->getEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, energyList.size() );


  delete bemc;
  delete file;
}

