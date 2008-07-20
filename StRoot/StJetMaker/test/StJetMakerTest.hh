// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETMAKERTEST_HH
#define STJETMAKERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StJetMakerTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StJetMakerTest );
  CPPUNIT_TEST( testConstruct );
  CPPUNIT_TEST( testMacroInterface );
  CPPUNIT_TEST( testInit );
  CPPUNIT_TEST( testTreeWriter );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testConstruct();
  void testMacroInterface();
  void testInit();
  void testTreeWriter();

private:

};

#endif // STJETMAKERTEST_HH
