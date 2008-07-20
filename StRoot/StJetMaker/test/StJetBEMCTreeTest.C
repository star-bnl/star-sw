// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StJetBEMCTree.h>
#include "StJetBEMCTreeTest.hh"

#include <TFile.h>
#include <TTree.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetBEMCTreeTest );

void StJetBEMCTreeTest::setUp()
{

}

void StJetBEMCTreeTest::tearDown()
{

}


void StJetBEMCTreeTest::testGetEntry() 
{
  //  TFile* file = new TFile("/star/institutions/mit/tai/testData/jetpart_6143024.root");
  TFile* file = new TFile("./jetpart_6143024.root");

  TTree *tree = dynamic_cast<TTree*>(file->Get("bemcTowers"));

  Int_t runNumber = 6143024;
  Int_t evenId;
  StJetBEMCTree* bemc = new StJetBEMCTree(tree, runNumber, evenId);


  evenId = 38;
  TowerEnergyList energyList = bemc->getEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)65, energyList.size() );

  evenId = 41;
  energyList = bemc->getEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, energyList.size() );


  delete bemc;
  delete file;
}

