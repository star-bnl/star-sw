// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTreeEntryMaker.h>
#include <StjTreeEntryCoordinator.h>

#include <StjTreeIndex.h>
#include <StjTreeIndexList.h>
#include <StjTreeIndexListCreator.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>
#include <string>

#include "StjTreeEntryMakerTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTreeEntryMakerTest );

void StjTreeEntryMakerTest::setUp() 
{

}

void StjTreeEntryMakerTest::tearDown() 
{

}

void StjTreeEntryMakerTest::testMake() 
{
  TDirectory* inFile = new TFile("./part_run6143024.root");

  StjTreeIndexListCreator idxCreator(inFile);
  idxCreator.AddTrgTreeName("trgBHT2");
  idxCreator.AddTrgTreeName("trgBJP2");
  StjTreeIndexList idxList = idxCreator.create();

  StjTreeEntryCoordinator* coord = new StjTreeEntryCoordinator(idxList);

  StjTreeEntryMaker *maker = new StjTreeEntryMaker("entryMaker", coord);

  maker->Init();

  for(int i = 0; i < 9313; ++i) {
    Int_t ret =  maker->Make();
    if(ret == kStEOF) break;
    CPPUNIT_ASSERT( 47 != i );
  }


  delete maker;
}
