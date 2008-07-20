// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETJETLISTCUTTEST_HH
#define STJETJETLISTCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StJetJetListCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StJetJetListCutTest );
  CPPUNIT_TEST( testPt );
  CPPUNIT_TEST( testEta );
  CPPUNIT_TEST( testNFourVecs );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testPt();
  void testEta();
  void testNFourVecs();

private:

};

#endif // STJETJETLISTCUTTEST_HH

