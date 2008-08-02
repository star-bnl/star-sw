// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJTRACKLISTTOFOURLISTTEST_HH
#define STJTRACKLISTTOFOURLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrackListToFourListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrackListToFourListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};

#endif // STJTRACKLISTTOFOURLISTTEST_HH
