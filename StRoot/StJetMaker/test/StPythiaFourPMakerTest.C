// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StPythiaFourPMakerTest.hh"

#include <StPythiaFourPMaker.h>

using namespace std;
// using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StPythiaFourPMakerTest );

void StPythiaFourPMakerTest::setUp() 
{

}

void StPythiaFourPMakerTest::tearDown() 
{

}

void StPythiaFourPMakerTest::testMake() 
{
  StPythiaFourPMaker* maker = new StPythiaFourPMaker("StPythiaFourPMaker", 0, 0);

  //  maker->Make();

  delete maker;
}
