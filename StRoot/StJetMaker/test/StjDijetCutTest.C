// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjDijetCutAsymmetricPt.h>
#include <StjDijetCutEta.h>
#include <StjDijetCutEta3MinusEta4.h>
#include <StjDijetCutDPhi.h>

#include <StjDijetList.h>

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

