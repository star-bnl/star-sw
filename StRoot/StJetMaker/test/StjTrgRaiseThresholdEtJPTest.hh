// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGRAISETHRESHOLDETJPTEST_HH
#define STJTRGRAISETHRESHOLDETJPTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrgRaiseThresholdEtJPTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrgRaiseThresholdEtJPTest );
  CPPUNIT_TEST( testOneJetPatchPass );
  CPPUNIT_TEST( testOneJetPatchNotPass );
  CPPUNIT_TEST( testTwoJetPatchesPass );
  CPPUNIT_TEST( testTwoJetPatchesNotPass );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOneJetPatchPass();
  void testOneJetPatchNotPass();
  void testTwoJetPatchesPass();
  void testTwoJetPatchesNotPass();

private:

};

#endif // STJTRGRAISETHRESHOLDETJPTEST_HH
