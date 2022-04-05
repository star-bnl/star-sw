// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJEBEMCENERGYSUMCALCULATORBUILDERTEST_HH
#define STJEBEMCENERGYSUMCALCULATORBUILDERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjeBemcEnergySumCalculatorBuilderTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjeBemcEnergySumCalculatorBuilderTest );
  CPPUNIT_TEST( testBuildImp );
  CPPUNIT_TEST( testBuildNull );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testBuildImp();
  void testBuildNull();

private:

};

#endif // STJEBEMCENERGYSUMCALCULATORBUILDERTEST_HH
