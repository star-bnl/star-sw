// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYVARIATIONTEST_HH
#define STJTOWERENERGYVARIATIONTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTowerEnergyListVariationTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTowerEnergyListVariationTest );
  CPPUNIT_TEST( testEnergyNull   );
  CPPUNIT_TEST( testEnergyPlus5   );
  CPPUNIT_TEST( testEnergyMinus5  );
  CPPUNIT_TEST( testEnergyPlus10  );
  CPPUNIT_TEST( testEnergyMinus10 );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testEnergyNull();
  void testEnergyPlus5();
  void testEnergyMinus5();
  void testEnergyPlus10();
  void testEnergyMinus10();

};

#endif // STJTOWERENERGYVARIATIONTEST_HH
