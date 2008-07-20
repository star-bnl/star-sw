// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef TOWERENERGYLISTTOFOURLISTTEST_HH
#define TOWERENERGYLISTTOFOURLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class TowerEnergyListToFourListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( TowerEnergyListToFourListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};

#endif // TOWERENERGYLISTTOFOURLISTTEST_HH
