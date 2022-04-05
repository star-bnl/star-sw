// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOWERENERGYLISTTOFOURVECLISTTEST_HH
#define STJTRACKTOWERENERGYLISTTOFOURVECLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTrackTowerEnergyListToFourVecListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTrackTowerEnergyListToFourVecListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};


#endif // STJTRACKTOWERENERGYLISTTOFOURVECLISTTEST_HH
