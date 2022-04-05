// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETLISTCUTTEST_HH
#define STJJETLISTCUTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjJetListCutTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjJetListCutTest );
  CPPUNIT_TEST( testPt );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testPt();

private:

};

#endif // STJJETLISTCUTTEST_HH

