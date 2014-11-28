// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STPYTHIAFOURPMAKERTEST_HH
#define STPYTHIAFOURPMAKERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StPythiaFourPMakerTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StPythiaFourPMakerTest );
  CPPUNIT_TEST( testMake );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testMake();

private:

};

#endif // STPYTHIAFOURPMAKERTEST_HH
