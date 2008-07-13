// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STBET4PMAKERTEST_HH
#define STBET4PMAKERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StBET4pMakerTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StBET4pMakerTest );
  CPPUNIT_TEST( testMacroInterface );
  CPPUNIT_TEST( testInit );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testMacroInterface();
  void testInit();


private:

};

#endif // STBET4PMAKERIMPTEST_HH
