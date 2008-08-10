// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "UniqueStringGenerator.hh"

#include <TDirectory.h>
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
}

TDirectory *StjTreeIndexCreatorTest::setupTestTDirecotry()
{
  string testdir(UniqueStringGenerator::generate());
  gROOT->cd();
  gDirectory->mkdir(testdir.c_str());
  gDirectory->cd(testdir.c_str());
  new TTree("bemcTowers", "bemcTowers");
  new TTree("tpcTracks", "tpcTracks");
}
