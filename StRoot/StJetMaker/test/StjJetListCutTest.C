// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjJetListCut.h>

#include <StjJetCutPt.h>
#include <StjJetCutEta.h>
#include <StjJetCutNFourVecs.h>

#include <StjJetList.h>

#include <iostream>
#include <set>
#include <cmath>

#include "StjJetListCutTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjJetListCutTest );

void StjJetListCutTest::setUp()
{

}

void StjJetListCutTest::tearDown()
{

}

void StjJetListCutTest::testPt()
{
  StjJetListCut* cut = new StjJetListCut();
  cut->addCut(new StjJetCutPt(5.0));

  StjJetList listIn;

  StjJet p1;
  p1.pt = 4.8;
  listIn.push_back(p1);

  StjJet p2;
  p2.pt = 5.1;
  listIn.push_back(p2);

  StjJetList listExpected;
  listExpected.push_back(p2);

  StjJetList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}

void StjJetListCutTest::testEta()
{
  StjJetListCut* cut = new StjJetListCut();
  cut->addCut(new StjJetCutEta(-5.0, 5.0));

  StjJetList listIn;

  StjJet p1;
  p1.eta = 4.8;
  listIn.push_back(p1);

  StjJet p2;
  p2.eta = 5.1;
  listIn.push_back(p2);

  StjJetList listExpected;
  listExpected.push_back(p1);

  StjJetList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}

void StjJetListCutTest::testNFourVecs()
{
  StjJetListCut* cut = new StjJetListCut();
  cut->addCut(new StjJetCutNFourVecs(4));

  StjJetList listIn;

  StjJet p1;
  p1.fourVecList.push_back(StjFourVec());
  p1.fourVecList.push_back(StjFourVec());
  p1.fourVecList.push_back(StjFourVec());
  listIn.push_back(p1);

  StjJet p2;
  p2.fourVecList.push_back(StjFourVec());
  p2.fourVecList.push_back(StjFourVec());
  p2.fourVecList.push_back(StjFourVec());
  p2.fourVecList.push_back(StjFourVec());
  listIn.push_back(p2);

  StjJetList listExpected;
  listExpected.push_back(p2);

  StjJetList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}
