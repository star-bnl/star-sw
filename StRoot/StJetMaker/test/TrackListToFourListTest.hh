// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef TRACKLISTTOFOURLIST_HH
#define TRACKLISTTOFOURLIST_HH

#include <cppunit/extensions/HelperMacros.h>

class TrackListToFourListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( TrackListToFourListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};

#endif // TRACKLISTTOFOURLIST_HH
