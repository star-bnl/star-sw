// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef TRACKLISTTOSTMUTRACKFOURVECLISTTEST_HH
#define TRACKLISTTOSTMUTRACKFOURVECLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class TrackListToStMuTrackFourVecListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( TrackListToStMuTrackFourVecListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};

#endif // TRACKLISTTOSTMUTRACKFOURVECLISTTEST_HH
