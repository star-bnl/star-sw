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

