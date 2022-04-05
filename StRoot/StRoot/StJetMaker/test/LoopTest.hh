// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef LOOPTEST_HH
#define LOOPTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class LoopTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( LoopTest );
  //  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

private:

};

#endif // LOOPTEST_HH
