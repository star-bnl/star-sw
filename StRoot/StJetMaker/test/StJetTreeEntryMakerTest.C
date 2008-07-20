// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StJetTreeEntryMaker.h>
#include <StJetTreeEntryCoordinator.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>
#include <string>

#include "StJetTreeEntryMakerTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetTreeEntryMakerTest );

void StJetTreeEntryMakerTest::setUp() 
{

}

void StJetTreeEntryMakerTest::tearDown() 
{

}

void StJetTreeEntryMakerTest::testMake() 
{
  StJetTreeEntryMaker *maker = new StJetTreeEntryMaker("entryMaker", "./jetpart_6143024.root");
  //  StJetTreeEntryMaker *maker = new StJetTreeEntryMaker("entryMaker", "/star/institutions/mit/tai/testData/jetpart_6143024.root");
  maker->AddTrgTreeName("trgBJP2");
  maker->AddTrgTreeName("trgBHT2");

  StJetTreeEntryCoordinator* coord = maker->coordinator();

  CPPUNIT_ASSERT( coord );
  StJetTreeEntryCoordinator::TrgTreeNameList trgTreeNameList = coord->trgTreeNameList(); 
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
