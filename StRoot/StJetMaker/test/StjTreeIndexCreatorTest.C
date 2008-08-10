// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "UniqueStringGenerator.hh"

#include <StjTreeIndex.h>
#include <StjTreeIndexCreator.h>

#include <TDirectory.h>
#include <TFile.h>
#include <TTree.h>

#include <string>

#include "StjTreeIndexCreatorTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTreeIndexCreatorTest );

void StjTreeIndexCreatorTest::setUp() 
{

}

void StjTreeIndexCreatorTest::tearDown() 
{

}

void StjTreeIndexCreatorTest::testOne()
{
  TDirectory* testDir = setupTestTDirecotry();
  testDir->ls();

  StjTreeIndexCreator idxCreator(testDir);
  idxCreator.AddTrgTreeName("trgBHT2");
  idxCreator.AddTrgTreeName("trgBJP2");

  StjTreeIndex actualIdx = idxCreator.create();

  StjTreeIndex expectedIdx = createExpectedIdx();

  CPPUNIT_ASSERT_EQUAL( expectedIdx, actualIdx);

}

TDirectory *StjTreeIndexCreatorTest::setupTestTDirecotry()
{
  return new TFile("./part_run6143024.root");
}

StjTreeIndex StjTreeIndexCreatorTest::createExpectedIdx()
{
  StjTreeIndex ret;
  return ret;
}


