// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>

#include <StjeBemcEnergySumCalculator.h>
#include <StjeBemcEnergySumCalculatorBuilder.h>


#include "StjeBemcEnergySumCalculatorBuilderTest.hh"

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( BemcEnergySumCalculatorBuilderTest );

void BemcEnergySumCalculatorBuilderTest::setUp() 
{

}

void BemcEnergySumCalculatorBuilderTest::tearDown() 
{

}

void BemcEnergySumCalculatorBuilderTest::testBuildImp() 
{
  BemcEnergySumCalculatorBuilder builder;
  bool useBEMC = true;
  CPPUNIT_ASSERT(dynamic_cast<BemcEnergySumCalculatorImp*>(builder.build(useBEMC)));
}

void BemcEnergySumCalculatorBuilderTest::testBuildNull() 
{
  BemcEnergySumCalculatorBuilder builder;
  bool useBEMC = false;
  CPPUNIT_ASSERT(dynamic_cast<BemcEnergySumCalculatorNull*>(builder.build(useBEMC)));
}

