// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "UniqueStringGenerator.hh"

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
  typedef std::set<StjTreeIndexList::index_t> IndexSet;

  StjTreeIndexList* ret = new StjTreeIndexList("runNumber", "eventId");

  return ret;
}

//*        0 *   6143024 *      1095 *
//*        2 *   6143024 *     18358 *
//*        3 *   6143024 *     20875 *
//*        4 *   6143024 *     23411 *
//*        5 *   6143024 *     24897 *
//*        6 *   6143024 *     28661 *
//*        7 *   6143024 *     34414 *
//*        8 *   6143024 *     38442 *
//*        9 *   6143024 *     39305 *
//*       11 *   6143024 *     43439 *
//*       12 *   6143024 *     48044 *
//*       13 *   6143024 *     53010 *
//*       14 *   6143024 *     53026 *
//*       15 *   6143024 *     55446 *
//*       17 *   6143024 *     55720 *
//*       18 *   6143024 *     56694 *
//*       19 *   6143024 *     56712 *
//*       20 *   6143024 *     64039 *
//*       21 *   6143024 *     67815 *
//*       22 *   6143024 *     71596 *
//*       23 *   6143024 *     72863 *
//*       24 *   6143024 *     80160 *
//*       26 *   6143024 *     81290 *
//*       27 *   6143024 *     81384 *
//*       28 *   6143024 *     86137 *
//*       30 *   6143024 *         4 *
//*       31 *   6143024 *         6 *



// ************************************
// *        0 *   6143024 *      4823 *
// *        1 *   6143024 *     14619 *
// *        2 *   6143024 *     17180 *
// *        4 *   6143024 *     28661 *
// *        5 *   6143024 *     31930 *
// *        6 *   6143024 *     33177 *
// *        7 *   6143024 *     43061 *
// *        8 *   6143024 *     45577 *
// *        9 *   6143024 *     55474 *
// *       10 *   6143024 *     64039 *
// *       11 *   6143024 *     81099 *
// *       12 *   6143024 *         3 *
// *       13 *   6143024 *         4 *
// ************************************
