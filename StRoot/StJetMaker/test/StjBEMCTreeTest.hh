// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMCTREETEST_HH
#define STJETBEMCTREETEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StJetBEMCTreeTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StJetBEMCTreeTest );
  CPPUNIT_TEST( testGetEntry );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testGetEntry();

private:

};


#endif // STJETBEMCTREETEST_HH
