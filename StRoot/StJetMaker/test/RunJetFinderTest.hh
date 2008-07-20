// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDERTEST_HH
#define RUNJETFINDERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class RunJetFinderTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( RunJetFinderTest );
  CPPUNIT_TEST( testRun );
  CPPUNIT_TEST( testScratch );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testRun();
  void testScratch();

};


#endif // RUNJETFINDERTEST_HH
