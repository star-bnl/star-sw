// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>
#include "StppAnaParsTest.hh"

#include <StppAnaPars.h>

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StppAnaParsTest );

void StppAnaParsTest::setUp() 
{

}

void StppAnaParsTest::tearDown() 
{

}

void StppAnaParsTest::testDefaultValues() 
{
  StppAnaPars* anapars = new StppAnaPars();
  CPPUNIT_ASSERT_EQUAL(     0.2, anapars->ptMin()     );
  CPPUNIT_ASSERT_EQUAL(   100.0, anapars->etaMax()    );
  CPPUNIT_ASSERT_EQUAL(     3.5, anapars->jetPtMin()  );
  CPPUNIT_ASSERT_EQUAL(   100.0, anapars->jetEtaMax() );
  CPPUNIT_ASSERT_EQUAL(     0.0, anapars->jetEtaMin() );
  CPPUNIT_ASSERT_EQUAL(       0, anapars->jetNmin()   );
  CPPUNIT_ASSERT_EQUAL(      12, anapars->nHits()     );
  CPPUNIT_ASSERT_EQUAL(       0, anapars->flagMin()   );

  delete anapars;
}

