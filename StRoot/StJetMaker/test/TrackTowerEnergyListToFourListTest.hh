// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOWERENERGYLISTTOFOURLISTTEST_HH
#define TRACKTOWERENERGYLISTTOFOURLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class TrackTowerEnergyListToFourListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( TrackTowerEnergyListToFourListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};


#endif // TRACKTOWERENERGYLISTTOFOURLISTTEST_HH
