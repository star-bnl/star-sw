// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJTRACKLISTTOSTMUTRACKFOURVECLISTTEST_HH
#define STJTRACKLISTTOSTMUTRACKFOURVECLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrackListToStMuTrackFourVecListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrackListToStMuTrackFourVecListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};

#endif // STJTRACKLISTTOSTMUTRACKFOURVECLISTTEST_HH
