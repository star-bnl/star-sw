// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECLISTCUTTEST_HH
#define STJETFOURVECLISTCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StJetFourVecListCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StJetFourVecListCutTest );
  CPPUNIT_TEST( testPt );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testPt();

private:

};

#endif // STJETFOURVECLISTCUTTEST_HH

