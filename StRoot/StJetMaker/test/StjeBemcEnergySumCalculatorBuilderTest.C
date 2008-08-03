// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>

#include <StjeBemcEnergySumCalculator.h>
#include <StjeBemcEnergySumCalculatorBuilder.h>


#include "StjeBemcEnergySumCalculatorBuilderTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjeBemcEnergySumCalculatorBuilderTest );

void StjeBemcEnergySumCalculatorBuilderTest::setUp() 
{

}

void StjeBemcEnergySumCalculatorBuilderTest::tearDown() 
{

}

void StjeBemcEnergySumCalculatorBuilderTest::testBuildImp() 
{
  StjeBemcEnergySumCalculatorBuilder builder;
  bool useBEMC = true;
  CPPUNIT_ASSERT(dynamic_cast<StjeBemcEnergySumCalculatorImp*>(builder.build(useBEMC)));
}

void StjeBemcEnergySumCalculatorBuilderTest::testBuildNull() 
{
  StjeBemcEnergySumCalculatorBuilder builder;
  bool useBEMC = false;
  CPPUNIT_ASSERT(dynamic_cast<StjeBemcEnergySumCalculatorNull*>(builder.build(useBEMC)));
}

