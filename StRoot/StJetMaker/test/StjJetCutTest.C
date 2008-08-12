// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StjJetCutPt.h>
#include <StjJetCutEta.h>
#include <StjJetCutNFourVecs.h>
#include <StjJetCutTrgBHT.h>
#include <StjJetCutTrgBJP.h>

#include <StjTrgJetPatchTowerIdMap2005.h>

#include <StjJetList.h>

#include "StjTrgMock.hh"

#include "StjJetCutTest.hh"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjJetCutTest );

void StjJetCutTest::setUp() 
{

}

void StjJetCutTest::tearDown() 
{

}

void StjJetCutTest::testPt()
{
  StjJetCutPt cut(5.0);

  StjJet p1;
  p1.pt = 4.8;

  CPPUNIT_ASSERT( cut(p1) );

  StjJet p2;
  p2.pt = 5.1;

  CPPUNIT_ASSERT( ! cut(p2) );
}

void StjJetCutTest::testEta()
{
  StjJetCutEta cut(-5.0, 5.0);

  StjJet p1;
  p1.eta = 4.8;

  CPPUNIT_ASSERT( ! cut(p1) );

  StjJet p2;
  p2.eta = 5.1;

  CPPUNIT_ASSERT( cut(p2) );

  StjJet p3;
  p3.eta = -5.1;

  CPPUNIT_ASSERT( cut(p3) );
}

void StjJetCutTest::testNFourVecs()
{
  StjJetCutNFourVecs cut(4);

  StjJet p1;
  p1.fourVecList.push_back(StjFourVec());
  p1.fourVecList.push_back(StjFourVec());
  p1.fourVecList.push_back(StjFourVec());

  CPPUNIT_ASSERT( cut(p1) );

  StjJet p2;
  p2.fourVecList.push_back(StjFourVec());
  p2.fourVecList.push_back(StjFourVec());
  p2.fourVecList.push_back(StjFourVec());
  p2.fourVecList.push_back(StjFourVec());

  CPPUNIT_ASSERT( ! cut(p2) );
}

void StjJetCutTest::testTrgBHT_notPass()
{
  StjTrgMock trg;
  trg._pass = false;

  StjJetCutTrgBHT cut(&trg);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 1;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBHT_pass()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjJetCutTrgBHT cut(&trg);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 1;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( ! cut(p1) );
}

void StjJetCutTest::testTrgBHT_type()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjJetCutTrgBHT cut(&trg);

  StjJet p1;
  StjFourVec four1;
  four1.type = 1;
  four1.detectorId = 0;
  four1.towerId = 0;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBHT_detectorId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjJetCutTrgBHT cut(&trg);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 13;
  four1.towerId = 1;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBHT_towerId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjJetCutTrgBHT cut(&trg);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 3;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBJP_notPass()
{
  StjTrgMock trg;
  trg._pass = false;

  StjJetCutTrgBJP cut(&trg, new StjTrgJetPatchTowerIdMap2005);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 1;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBJP_pass()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjJetCutTrgBJP cut(&trg, new StjTrgJetPatchTowerIdMap2005);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 503;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( ! cut(p1) );
}

void StjJetCutTest::testTrgBJP_type()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjJetCutTrgBJP cut(&trg, new StjTrgJetPatchTowerIdMap2005);

  StjJet p1;
  StjFourVec four1;
  four1.type = 1;
  four1.detectorId = 0;
  four1.towerId = 0;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBJP_detectorId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjJetCutTrgBJP cut(&trg, new StjTrgJetPatchTowerIdMap2005);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 13;
  four1.towerId = 503;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}

void StjJetCutTest::testTrgBJP_jetPatchId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjJetCutTrgBJP cut(&trg, new StjTrgJetPatchTowerIdMap2005);

  StjJet p1;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 101;
  p1.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(p1) );
}
