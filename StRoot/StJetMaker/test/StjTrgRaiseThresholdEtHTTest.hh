// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGRAISETHRESHOLDETHTTEST_HH
#define STJTRGRAISETHRESHOLDETHTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrgRaiseThresholdEtHTTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrgRaiseThresholdEtHTTest );
  CPPUNIT_TEST( testOneTowerPass );
  CPPUNIT_TEST( testOneTowerNotPass );
  CPPUNIT_TEST( testTwoTowersPass );
  CPPUNIT_TEST( testTwoTowersNotPass );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOneTowerPass();
  void testOneTowerNotPass();
  void testTwoTowersPass();
  void testTwoTowersNotPass();

private:

};

#endif // STJTRGRAISETHRESHOLDETHTTEST_HH
