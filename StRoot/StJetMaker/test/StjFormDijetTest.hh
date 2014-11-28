// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFORMDIJETTEST_HH
#define STJFORMDIJETTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjFormDijetTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjFormDijetTest );
  CPPUNIT_TEST( testOneJet );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOneJet();
  void testOne();

private:

};

#endif // STJFORMDIJETTEST_HH
