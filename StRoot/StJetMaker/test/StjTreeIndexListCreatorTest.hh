// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXLISTCREATORTEST_HH
#define STJTREEINDEXLISTCREATORTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTreeIndexListCreatorTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTreeIndexListCreatorTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

private:

  TDirectory *setupTestTDirecotry();

  StjTreeIndexList *createExpectedList();
  
};

#endif // STJTREEINDEXLISTCREATORTEST_HH
