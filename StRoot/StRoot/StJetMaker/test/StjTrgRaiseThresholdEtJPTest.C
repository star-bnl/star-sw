// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMock.hh"

#include <StjTrgRaiseThresholdEtJP.h>

#include "StjTrgRaiseThresholdEtJPTest.hh"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTrgRaiseThresholdEtJPTest );

void StjTrgRaiseThresholdEtJPTest::setUp() 
{

}

void StjTrgRaiseThresholdEtJPTest::tearDown() 
{

}

void StjTrgRaiseThresholdEtJPTest::testOneJetPatchPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;
  trgSrc._jetPatches.push_back(1);
  trgSrc._jetPatchDsmAdc.push_back(0);
  trgSrc._jetPatchAdc.push_back(0);
  trgSrc._jetPatchEnergy.push_back(0);
  trgSrc._jetPatchEt.push_back(3.4);

  StjTrgRaiseThresholdEtJP trg(&trgSrc, 3.0);

  CPPUNIT_ASSERT( trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1, trg.jetPatches().size() );
  CPPUNIT_ASSERT_EQUAL(   1, trg.jetPatches()[0] );
  CPPUNIT_ASSERT_EQUAL( 3.4, trg.jetPatchEt()[0] );
}

void StjTrgRaiseThresholdEtJPTest::testOneJetPatchNotPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;
  trgSrc._jetPatches.push_back(1);
  trgSrc._jetPatchDsmAdc.push_back(0);
  trgSrc._jetPatchAdc.push_back(0);
  trgSrc._jetPatchEnergy.push_back(0);
  trgSrc._jetPatchEt.push_back(3.4);

  StjTrgRaiseThresholdEtJP trg(&trgSrc, 4.0);

  CPPUNIT_ASSERT( ! trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( ! trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)0, trg.jetPatches().size() );
}

void StjTrgRaiseThresholdEtJPTest::testTwoJetPatchesPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;

  trgSrc._jetPatches.push_back(1);
  trgSrc._jetPatchDsmAdc.push_back(0);
  trgSrc._jetPatchAdc.push_back(0);
  trgSrc._jetPatchEnergy.push_back(0);
  trgSrc._jetPatchEt.push_back(3.4);

  trgSrc._jetPatches.push_back(3);
  trgSrc._jetPatchDsmAdc.push_back(0);
  trgSrc._jetPatchAdc.push_back(0);
  trgSrc._jetPatchEnergy.push_back(0);
  trgSrc._jetPatchEt.push_back(2.5);

  StjTrgRaiseThresholdEtJP trg(&trgSrc, 3.0);

  CPPUNIT_ASSERT( trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1, trg.jetPatches().size() );
  CPPUNIT_ASSERT_EQUAL(   1, trg.jetPatches()[0] );
  CPPUNIT_ASSERT_EQUAL( 3.4, trg.jetPatchEt()[0] );
}

void StjTrgRaiseThresholdEtJPTest::testTwoJetPatchesNotPass()
{
  StjTrgMock trgSrc;
  trgSrc._runNumber = 1;
  trgSrc._eventId = 1;
  trgSrc._passed = true;
  trgSrc._hard = true;
  trgSrc._soft = true;

  trgSrc._jetPatches.push_back(1);
  trgSrc._jetPatchDsmAdc.push_back(0);
  trgSrc._jetPatchAdc.push_back(0);
  trgSrc._jetPatchEnergy.push_back(0);
  trgSrc._jetPatchEt.push_back(3.4);

  trgSrc._jetPatches.push_back(3);
  trgSrc._jetPatchDsmAdc.push_back(0);
  trgSrc._jetPatchAdc.push_back(0);
  trgSrc._jetPatchEnergy.push_back(0);
  trgSrc._jetPatchEt.push_back(2.5);

  StjTrgRaiseThresholdEtJP trg(&trgSrc, 4.0);

  CPPUNIT_ASSERT( ! trg.passed() );
  CPPUNIT_ASSERT( trg.hard() );
  CPPUNIT_ASSERT( ! trg.soft() );
  CPPUNIT_ASSERT_EQUAL( (size_t)0, trg.jetPatches().size() );
}

