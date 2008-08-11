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
  testDir->ls();

  StjTreeIndexListCreator idxCreator(testDir);
  idxCreator.AddTrgTreeName("trgBHT2");
  idxCreator.AddTrgTreeName("trgBJP2");

  StjTreeIndexList* actualList = idxCreator.create();

  StjTreeIndexList* expectedList = createExpectedList();

  CPPUNIT_ASSERT_EQUAL( *expectedList, *actualList);

}

TDirectory *StjTreeIndexListCreatorTest::setupTestTDirecotry()
{
  return new TFile("./part_run6143024.root");
}

StjTreeIndexList *StjTreeIndexListCreatorTest::createExpectedList()
{
  StjTreeIndexList* ret = new StjTreeIndexList("runNumber", "eventId");

  ret->push_back(StjTreeIndex(6143024, 3));

  return ret;
}

// 6143024 *         3 *
// 6143024 *         4 *
// 6143024 *         6 *
// 6143024 *      1095 *
// 6143024 *      4823 *
// 6143024 *     14619 *
// 6143024 *     17180 *
// 6143024 *     18358 *
// 6143024 *     20875 *
// 6143024 *     23411 *
// 6143024 *     24897 *
// 6143024 *     28661 *
// 6143024 *     31930 *
// 6143024 *     33177 *
// 6143024 *     34414 *
// 6143024 *     38442 *
// 6143024 *     39305 *
// 6143024 *     43061 *
// 6143024 *     43439 *
// 6143024 *     45577 *
// 6143024 *     48044 *
// 6143024 *     53010 *
// 6143024 *     53026 *
// 6143024 *     55446 *
// 6143024 *     55474 *
// 6143024 *     55720 *
// 6143024 *     56694 *
// 6143024 *     56712 *
// 6143024 *     64039 *
// 6143024 *     67815 *
// 6143024 *     71596 *
// 6143024 *     72863 *
// 6143024 *     80160 *
// 6143024 *     81290 *
// 6143024 *     81099 *
// 6143024 *     81384 *
// 6143024 *     86137 *
