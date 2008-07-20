// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURLISTCUTTEST_HH
#define STJETFOURLISTCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StJetFourListCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StJetFourListCutTest );
  CPPUNIT_TEST( testPt );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testPt();

private:

};

#endif // STJETFOURLISTCUTTEST_HH

