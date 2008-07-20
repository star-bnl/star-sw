// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StJetFourVecListCut.h>

#include <FourVecCutPt.h>

#include <FourVecList.h>

#include <iostream>
#include <set>
#include <cmath>

#include "StJetFourVecListCutTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetFourVecCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetFourVecListCutTest );

void StJetFourVecListCutTest::setUp()
{

}

void StJetFourVecListCutTest::tearDown()
{

}

void StJetFourVecListCutTest::testPt()
{
  StJetFourVecListCut* cut = new StJetFourVecListCut();
  cut->addCut(new FourVecCutPt(0.2));

  FourVecList listIn;

  FourVec p1;
  p1.pt = 0.3;
  listIn.push_back(p1);

  FourVec p2;
  p2.pt = 0.1;
  listIn.push_back(p2);

  FourVecList listExpected;
  listExpected.push_back(p1);

  FourVecList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}
