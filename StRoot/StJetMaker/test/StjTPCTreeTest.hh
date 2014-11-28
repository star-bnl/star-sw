// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPCTREETEST_HH
#define STJTPCTREETEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTPCTreeTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTPCTreeTest );
  CPPUNIT_TEST( testGetEntry );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testGetEntry();

private:

};


#endif // STJTPCTREETEST_HH
