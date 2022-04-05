// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STPPANAPARSTEST_HH
#define STPPANAPARSTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StppAnaParsTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StppAnaParsTest );
  CPPUNIT_TEST( testDefaultValues );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testDefaultValues();

private:

};

#endif // STPPANAPARSTEST_HH
