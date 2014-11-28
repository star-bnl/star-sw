// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMCTREETEST_HH
#define STJBEMCTREETEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjBEMCTreeTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjBEMCTreeTest );
  CPPUNIT_TEST( testGetEntry );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testGetEntry();

private:

};


#endif // STJBEMCTREETEST_HH
