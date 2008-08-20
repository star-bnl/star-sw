// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "UniqueStringGenerator.hh"

#include <StjTreeIndex.h>
#include <StjTreeIndexList.h>
#include <StjTreeIndexListCreator.h>

#include <TDirectory.h>
#include <TFile.h>
#include <TTree.h>

#include <string>

#include "StjTreeIndexListCreatorTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTreeIndexListCreatorTest );

void StjTreeIndexListCreatorTest::setUp() 
{

}

void StjTreeIndexListCreatorTest::tearDown() 
{

}

void StjTreeIndexListCreatorTest::testOne()
{
  TDirectory* testDir = setupTestTDirecotry();

  StjTreeIndexListCreator idxCreator(testDir);
  idxCreator.AddTrgTreeName("trgBHT2");
  idxCreator.AddTrgTreeName("trgBJP2");

  StjTreeIndexList actualList = idxCreator.create();

  StjTreeIndexList expectedList = createExpectedList();

//  cout << endl;
//  for(StjTreeIndexList::const_iterator it = actualList.begin(); it != actualList.end(); ++it) {
//    cout << (*it) << endl;
//  }

  CPPUNIT_ASSERT_EQUAL( expectedList, actualList);

}

TDirectory *StjTreeIndexListCreatorTest::setupTestTDirecotry()
{
  return new TFile("./part_run6143024.root");
}

StjTreeIndexList StjTreeIndexListCreatorTest::createExpectedList()
{
  StjTreeIndexList ret;

  ret.push_back(StjTreeIndex(6143024, 3	   ));;
  ret.push_back(StjTreeIndex(6143024, 4	   ));
  ret.push_back(StjTreeIndex(6143024, 6	   ));
  ret.push_back(StjTreeIndex(6143024, 1095 ));
  ret.push_back(StjTreeIndex(6143024, 14619));
  ret.push_back(StjTreeIndex(6143024, 17180));
  ret.push_back(StjTreeIndex(6143024, 18358));
  ret.push_back(StjTreeIndex(6143024, 20875));
  ret.push_back(StjTreeIndex(6143024, 23411));
  ret.push_back(StjTreeIndex(6143024, 24897));
  ret.push_back(StjTreeIndex(6143024, 28661));
  ret.push_back(StjTreeIndex(6143024, 33177));
  ret.push_back(StjTreeIndex(6143024, 34414));
  ret.push_back(StjTreeIndex(6143024, 38442));
  ret.push_back(StjTreeIndex(6143024, 39305));
  ret.push_back(StjTreeIndex(6143024, 43061));
  ret.push_back(StjTreeIndex(6143024, 43439));
  ret.push_back(StjTreeIndex(6143024, 45577));
  ret.push_back(StjTreeIndex(6143024, 48044));
  ret.push_back(StjTreeIndex(6143024, 53010));
  ret.push_back(StjTreeIndex(6143024, 53026));
  ret.push_back(StjTreeIndex(6143024, 55446));
  ret.push_back(StjTreeIndex(6143024, 55474));
  ret.push_back(StjTreeIndex(6143024, 55646));
  ret.push_back(StjTreeIndex(6143024, 55720));
  ret.push_back(StjTreeIndex(6143024, 56694));
  ret.push_back(StjTreeIndex(6143024, 56712));
  ret.push_back(StjTreeIndex(6143024, 64039));
  ret.push_back(StjTreeIndex(6143024, 67815));
  ret.push_back(StjTreeIndex(6143024, 71596));
  ret.push_back(StjTreeIndex(6143024, 72863));
  ret.push_back(StjTreeIndex(6143024, 80160));
  ret.push_back(StjTreeIndex(6143024, 81099));
  ret.push_back(StjTreeIndex(6143024, 81234));
  ret.push_back(StjTreeIndex(6143024, 81290));
  ret.push_back(StjTreeIndex(6143024, 81384));
  ret.push_back(StjTreeIndex(6143024, 86137));

  return ret;
}

