// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKLISTCUTTEST_HH
#define STJTRACKLISTCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrackListCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrackListCutTest );
  CPPUNIT_TEST( testNHits );
  CPPUNIT_TEST( testFlag );
  CPPUNIT_TEST( test2005 );
  CPPUNIT_TEST( test2006 );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testFlag();
  void testNHits();
  void test2005();
  void test2006();

private:

};

#endif // STJTRACKLISTCUTTEST_HH
