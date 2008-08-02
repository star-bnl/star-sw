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
using namespace StSpinJet;
using namespace StJetJetCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetJetListCutTest );

void StJetJetListCutTest::setUp()
{

}

void StJetJetListCutTest::tearDown()
{

}

void StJetJetListCutTest::testPt()
{
  StJetJetListCut* cut = new StJetJetListCut();
  cut->addCut(new JetCutPt(5.0));

  JetList listIn;

  Jet p1;
  p1.pt = 4.8;
  listIn.push_back(p1);

  Jet p2;
  p2.pt = 5.1;
  listIn.push_back(p2);

  JetList listExpected;
  listExpected.push_back(p2);

  JetList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}

void StJetJetListCutTest::testEta()
{
  StJetJetListCut* cut = new StJetJetListCut();
  cut->addCut(new JetCutEta(-5.0, 5.0));

  JetList listIn;

  Jet p1;
  p1.eta = 4.8;
  listIn.push_back(p1);

  Jet p2;
  p2.eta = 5.1;
  listIn.push_back(p2);

  JetList listExpected;
  listExpected.push_back(p1);

  JetList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}

void StJetJetListCutTest::testNFourVecs()
{
  StJetJetListCut* cut = new StJetJetListCut();
  cut->addCut(new JetCutNFourVecs(4));

  JetList listIn;

  Jet p1;
  p1.fourVecList.push_back(FourVec());
  p1.fourVecList.push_back(FourVec());
  p1.fourVecList.push_back(FourVec());
  listIn.push_back(p1);

  Jet p2;
  p2.fourVecList.push_back(FourVec());
  p2.fourVecList.push_back(FourVec());
  p2.fourVecList.push_back(FourVec());
  p2.fourVecList.push_back(FourVec());
  listIn.push_back(p2);

  JetList listExpected;
  listExpected.push_back(p2);

  JetList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}
