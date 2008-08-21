// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMock.hh"

#include <StjTrgRaiseThresholdEtHT.h>
#include <StjTrgPassCondition.h>

#include "StjTrgRaiseThresholdEtHTTest.hh"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTrgRaiseThresholdEtHTTest );

void StjTrgRaiseThresholdEtHTTest::setUp() 
{

}

void StjTrgRaiseThresholdEtHTTest::tearDown() 
{

}

void StjTrgRaiseThresholdEtHTTest::testOneTowerPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;
  trgSrc._towers.push_back(1);
  trgSrc._towerDsmAdc.push_back(0);
  trgSrc._towerAdc.push_back(0);
  trgSrc._towerEnergy.push_back(0);
  trgSrc._towerEt.push_back(3.4);

  StjTrgRaiseThresholdEtHT trg(&trgSrc, new StjTrgPassConditionHardAndSoft, 3.0);

  CPPUNIT_ASSERT( trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1, trg.towers().size() );
  CPPUNIT_ASSERT_EQUAL(   1, trg.towers()[0] );
  CPPUNIT_ASSERT_EQUAL( 3.4, trg.towerEt()[0] );
}

void StjTrgRaiseThresholdEtHTTest::testOneTowerNotPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;
  trgSrc._towers.push_back(1);
  trgSrc._towerDsmAdc.push_back(0);
  trgSrc._towerAdc.push_back(0);
  trgSrc._towerEnergy.push_back(0);
  trgSrc._towerEt.push_back(3.4);

  StjTrgRaiseThresholdEtHT trg(&trgSrc, new StjTrgPassConditionHardAndSoft, 4.0);

  CPPUNIT_ASSERT( ! trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( ! trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)0, trg.towers().size() );
}

void StjTrgRaiseThresholdEtHTTest::testTwoTowersPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;

  trgSrc._towers.push_back(1);
  trgSrc._towerDsmAdc.push_back(0);
  trgSrc._towerAdc.push_back(0);
  trgSrc._towerEnergy.push_back(0);
  trgSrc._towerEt.push_back(3.4);

  trgSrc._towers.push_back(10);
  trgSrc._towerDsmAdc.push_back(0);
  trgSrc._towerAdc.push_back(0);
  trgSrc._towerEnergy.push_back(0);
  trgSrc._towerEt.push_back(2.5);

  StjTrgRaiseThresholdEtHT trg(&trgSrc, new StjTrgPassConditionHardAndSoft, 3.0);

  CPPUNIT_ASSERT( trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1, trg.towers().size() );
  CPPUNIT_ASSERT_EQUAL(   1, trg.towers()[0] );
  CPPUNIT_ASSERT_EQUAL( 3.4, trg.towerEt()[0] );
}

void StjTrgRaiseThresholdEtHTTest::testTwoTowersNotPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;

  trgSrc._towers.push_back(1);
  trgSrc._towerDsmAdc.push_back(0);
  trgSrc._towerAdc.push_back(0);
  trgSrc._towerEnergy.push_back(0);
  trgSrc._towerEt.push_back(3.4);

  trgSrc._towers.push_back(10);
  trgSrc._towerDsmAdc.push_back(0);
  trgSrc._towerAdc.push_back(0);
  trgSrc._towerEnergy.push_back(0);
  trgSrc._towerEt.push_back(2.5);

  StjTrgRaiseThresholdEtHT trg(&trgSrc, new StjTrgPassConditionHardAndSoft, 4.0);

  CPPUNIT_ASSERT( ! trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( ! trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)0, trg.towers().size() );
}

