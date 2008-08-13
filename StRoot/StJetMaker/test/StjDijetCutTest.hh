// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTTEST_HH
#define STJDIJETCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjDijetCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjDijetCutTest );
  CPPUNIT_TEST( testAsymmetricPt );
  CPPUNIT_TEST( testEta );
  CPPUNIT_TEST( testEta3MinusEta4 );
  CPPUNIT_TEST( testDPhi );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testAsymmetricPt();
  void testEta();
  void testEta3MinusEta4();
  void testDPhi();

private:

};

#endif // STJDIJETCUTTEST_HH
