// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjDijetCutAsymmetricPt.h>
#include <StjDijetCutEta.h>
#include <StjDijetCutEta3MinusEta4.h>
#include <StjDijetCutDPhi.h>
#include <StjDijetCutTrgBHT.h>
#include <StjDijetCutTrgBJP.h>

#include <StjTrgBEMCJetPatchTowerIdMap2005.h>

#include <StjDijetList.h>

#include "StjTrgMock.hh"

#include "StjDijetCutTest.hh"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjDijetCutTest );

void StjDijetCutTest::setUp() 
{

}

void StjDijetCutTest::tearDown() 
{

}

void StjDijetCutTest::testAsymmetricPt()
{
  StjDijetCutAsymmetricPt cut(7.0, 10.0);

  StjDijet dijet;
  dijet.jet3.pt = 5.0;
  dijet.jet4.pt = 6.0;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.pt = 5.0;
  dijet.jet4.pt = 11.0;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.pt = 11.0;
  dijet.jet4.pt = 5.0;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.pt = 8.0;
  dijet.jet4.pt = 8.0;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.pt = 8.0;
  dijet.jet4.pt = 11.0;
  CPPUNIT_ASSERT( ! cut(dijet) );

  dijet.jet3.pt = 11.0;
  dijet.jet4.pt = 8.0;
  CPPUNIT_ASSERT( ! cut(dijet) );
}

void StjDijetCutTest::testEta()
{
  StjDijetCutEta cut(-0.05, 0.95);

  StjDijet dijet;
  dijet.jet3.eta = 0.1;
  dijet.jet4.eta = 0.2;
  CPPUNIT_ASSERT( ! cut(dijet) );

  dijet.jet3.eta = -0.1;
  dijet.jet4.eta = 0.2;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.eta = 1.1;
  dijet.jet4.eta = 0.2;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.eta = 0.2;
  dijet.jet4.eta = -0.1;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.eta = 0.2;
  dijet.jet4.eta = 1.1;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.eta = -0.2;
  dijet.jet4.eta = 1.1;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.jet3.eta = 1.1;
  dijet.jet4.eta = -0.2;
  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testEta3MinusEta4()
{
  StjDijetCutEta3MinusEta4 cut(0.5);

  StjDijet dijet;
  dijet.jet3.eta = 0.2;
  dijet.jet4.eta = 0.1;
  CPPUNIT_ASSERT( ! cut(dijet) );

  dijet.jet3.eta = 0.61;
  dijet.jet4.eta = 0.1;
  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testDPhi()
{
  StjDijetCutDPhi cut(2.0);

  StjDijet dijet;
  dijet.dphi = 0.5;
  CPPUNIT_ASSERT( cut(dijet) );

  dijet.dphi = 2.1;
  CPPUNIT_ASSERT( ! cut(dijet) );
}

void StjDijetCutTest::testTrgBHT_notPass()
{
  StjTrgMock trg;
  trg._pass = false;

  StjDijetCutTrgBHT cut(&trg);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 1;
  dijet.jet3.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBHT_pass()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjDijetCutTrgBHT cut(&trg);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 1;
  dijet.jet3.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( ! cut(dijet) );

  StjDijet dijet2;
  StjFourVec four2;
  four2.type = 2;
  four2.detectorId = 9;
  four2.towerId = 1;
  dijet2.jet4.fourVecList.push_back(four2);

  CPPUNIT_ASSERT( ! cut(dijet) );
}

void StjDijetCutTest::testTrgBHT_type()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjDijetCutTrgBHT cut(&trg);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 1;
  four1.detectorId = 0;
  four1.towerId = 0;
  dijet.jet3.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBHT_detectorId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjDijetCutTrgBHT cut(&trg);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 13;
  four1.towerId = 1;
  dijet.jet4.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBHT_towerId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._towers.push_back(1);

  StjDijetCutTrgBHT cut(&trg);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 3;
  dijet.jet3.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBJP_notPass()
{
  StjTrgMock trg;
  trg._pass = false;

  StjDijetCutTrgBJP cut(&trg, new StjTrgBEMCJetPatchTowerIdMap2005);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 1;
  dijet.jet4.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBJP_pass()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjDijetCutTrgBJP cut(&trg, new StjTrgBEMCJetPatchTowerIdMap2005);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 503;
  dijet.jet3.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( ! cut(dijet) );
}

void StjDijetCutTest::testTrgBJP_type()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjDijetCutTrgBJP cut(&trg, new StjTrgBEMCJetPatchTowerIdMap2005);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 1;
  four1.detectorId = 0;
  four1.towerId = 0;
  dijet.jet4.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBJP_detectorId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjDijetCutTrgBJP cut(&trg, new StjTrgBEMCJetPatchTowerIdMap2005);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 13;
  four1.towerId = 503;
  dijet.jet4.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

void StjDijetCutTest::testTrgBJP_jetPatchId()
{
  StjTrgMock trg;
  trg._pass = true;
  trg._jetPatches.push_back(2);

  StjDijetCutTrgBJP cut(&trg, new StjTrgBEMCJetPatchTowerIdMap2005);

  StjDijet dijet;
  StjFourVec four1;
  four1.type = 2;
  four1.detectorId = 9;
  four1.towerId = 101;
  dijet.jet3.fourVecList.push_back(four1);

  CPPUNIT_ASSERT( cut(dijet) );
}

