// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StJetFourListCut.h>

#include <FourCutPt.h>

#include <TObjArray.h>


#include <iostream>
#include <set>
#include <cmath>

#include "StJetFourListCutTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetFourCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetFourListCutTest );

void StJetFourListCutTest::setUp()
{

}

void StJetFourListCutTest::tearDown()
{

}

void StJetFourListCutTest::testPt()
{
  StJetFourListCut* cut = new StJetFourListCut();
  cut->addCut(new FourCutPt(0.2));

  TObjArray listIn;
  listIn.SetOwner(kTRUE);

  TLorentzVectorWithId p1;
  p1.SetPtEtaPhiM(0.3, 0, 0, 0);
  listIn.Add(new TLorentzVectorWithId(p1));

  TLorentzVectorWithId p2;
  p2.SetPtEtaPhiM(0.2, 0, 0, 0);
  listIn.Add(new TLorentzVectorWithId(p2));

  TObjArray listExpected;
  listExpected.SetOwner(kTRUE);
  listExpected.Add(new TLorentzVectorWithId(p1));

  TObjArray listActual = (*cut)(listIn);
  listActual.SetOwner(kTRUE);

  CPPUNIT_ASSERT_EQUAL( listExpected.GetEntries(), listActual.GetEntries() );
  CPPUNIT_ASSERT_EQUAL( *(TLorentzVectorWithId*)listExpected[0], *(TLorentzVectorWithId*)listActual[0] );

  delete cut;
}
