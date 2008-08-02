// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef TOWERENERGYLISTTOSTMUTRACKFOURVECLISTTEST_HH
#define TOWERENERGYLISTTOSTMUTRACKFOURVECLISTTEST_HH

#include <cppunit/extensions/HelperMacros.h>

class StjTowerEnergyListToStMuTrackFourVecListTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StjTowerEnergyListToStMuTrackFourVecListTest );
  CPPUNIT_TEST( testOne );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp();
  void tearDown();

  void testOne();

};

#endif // TOWERENERGYLISTTOSTMUTRACKFOURVECLISTTEST_HH
