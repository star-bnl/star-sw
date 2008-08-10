// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTreeEntryMaker.h>
#include <StjTreeEntryCoordinator.h>

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
  //  TDirectory *inFile = TFile::Open("./jetpart_6143024.root");
  TDirectory* inFile = new TFile("./part_run6143024.root");

  StjTreeEntryCoordinator* coord = new StjTreeEntryCoordinator(inFile);
  coord->AddTrgTreeName("trgBJP2");
  coord->AddTrgTreeName("trgBHT2");

  StjTreeEntryMaker *maker = new StjTreeEntryMaker("entryMaker", coord);

  StjTreeEntryCoordinator::TrgTreeNameList trgTreeNameList = coord->trgTreeNameList(); 
  CPPUNIT_ASSERT_EQUAL( (size_t)2,  trgTreeNameList.size() );
  CPPUNIT_ASSERT_EQUAL( string("trgBJP2"),  trgTreeNameList[0] );
  CPPUNIT_ASSERT_EQUAL( string("trgBHT2"),  trgTreeNameList[1] );

  maker->Init();

  for(int i = 0; i < 9313; ++i) {
    Int_t ret =  maker->Make();
    if(ret == kStEOF) break;
    CPPUNIT_ASSERT( 9312 != i );
  }


  delete maker;
}
