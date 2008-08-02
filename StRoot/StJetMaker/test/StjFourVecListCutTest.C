// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjFourVecListCut.h>

#include <StjFourVecCutPt.h>

#include <StjFourVecList.h>

#include <iostream>
#include <set>
#include <cmath>

#include "StjFourVecListCutTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetFourVecCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjFourVecListCutTest );

void StjFourVecListCutTest::setUp()
{

}

void StjFourVecListCutTest::tearDown()
{

}

void StjFourVecListCutTest::testPt()
{
  StjFourVecListCut* cut = new StjFourVecListCut();
  cut->addCut(new StjFourVecCutPt(0.2));

  StjFourVecList listIn;

  StjFourVec p1;
  p1.pt = 0.3;
  listIn.push_back(p1);

  StjFourVec p2;
  p2.pt = 0.1;
  listIn.push_back(p2);

  StjFourVecList listExpected;
  listExpected.push_back(p1);

  StjFourVecList listActual = (*cut)(listIn);

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );

  delete cut;
}
