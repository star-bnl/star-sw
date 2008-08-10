// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STBET4PMAKERIMPBUILDERTEST_HH
#define STBET4PMAKERIMPBUILDERTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StBET4pMakerImpBuilderTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StBET4pMakerImpBuilderTest );
  CPPUNIT_TEST( test2003 );
  CPPUNIT_TEST( test2005 );
  CPPUNIT_TEST( test2006 );
  CPPUNIT_TEST( testBEMCout );
  CPPUNIT_TEST( testTPCout );
  CPPUNIT_TEST( testReadTree );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void test2003();
  void test2005();
  void test2006();
  void testBEMCout();
  void testTPCout();
  void testReadTree();

private:

  TDirectory *setupTestTDirecotry();

};

#endif // STBET4PMAKERIMPBUILDERTEST_HH
