// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGTREETEST_HH
#define STJTRGTREETEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrgTreeTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrgTreeTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

private:

};

#endif // STJTRGTREETEST_HH
