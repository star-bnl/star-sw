// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef TREECONTENTTEST_HH
#define TREECONTENTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class TreeContentTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( TreeContentTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

private:

};

#endif // TREECONTENTTEST_HH
