// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECLISTCUTTEST_HH
#define STJFOURVECLISTCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjFourVecListCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjFourVecListCutTest );
  CPPUNIT_TEST( testPt );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testPt();

private:

};

#endif // STJFOURVECLISTCUTTEST_HH

