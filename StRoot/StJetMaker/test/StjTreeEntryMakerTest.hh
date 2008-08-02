// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEENTRYMAKERTEST_HH
#define STJTREEENTRYMAKERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTreeEntryMakerTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTreeEntryMakerTest );
  CPPUNIT_TEST( testMake );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testMake();

private:

};

#endif // STJTREEENTRYMAKERTEST_HH
