// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXCREATORTEST_HH
#define STJTREEINDEXCREATORTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTreeIndexCreatorTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTreeIndexCreatorTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

private:

  TDirectory *setupTestTDirecotry();

  StjTreeIndex createExpectedIdx();
  

};

#endif // STJTREEINDEXCREATORTEST_HH
