// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTowerEnergyVariation.h>
#include <StjTowerEnergyList.h>

#include <iostream>
#include <set>
#include <cmath>

#include "StjTowerEnergyVariationTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTowerEnergyVariationTest );

void StjTowerEnergyVariationTest::setUp()
{

}

void StjTowerEnergyVariationTest::tearDown()
{

}


void StjTowerEnergyVariationTest::testEnergyPlus5()
{
  StjTowerEnergyVariation* variation = new StjTowerEnergyVariation(0.05);

  StjTowerEnergyList listIn;

  StjTowerEnergy energy1;
  energy1.energy = 3.0;
  listIn.push_back(energy1);

  StjTowerEnergy energy2;
  energy2.energy = 5.0;
  listIn.push_back(energy2);

  StjTowerEnergyList listExpected;
  energy1.energy *= (1.0 + 0.05);
  energy2.energy *= (1.0 + 0.05);
  listExpected.push_back(energy1);
  listExpected.push_back(energy2);
  
  // excercise
  StjTowerEnergyList listActual = (*variation)(listIn);
  
  // verify
  //CPPUNIT_ASSERT_EQUAL( listExpected.size(), listActual.size() );
  //for(size_t i = 0; i < listExpected.size(); ++i) {
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].runNumber , listActual[i].runNumber  );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].eventId   , listActual[i].eventId    );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].detectorId, listActual[i].detectorId );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].towerId   , listActual[i].towerId    );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].towerR    , listActual[i].towerR     );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].towerEta  , listActual[i].towerEta   );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].towerPhi  , listActual[i].towerPhi   );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].vertexX   , listActual[i].vertexX    );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].vertexY   , listActual[i].vertexY    );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].vertexZ   , listActual[i].vertexZ    );
  //  CPPUNIT_ASSERT_DOUBLES_EQUAL( listExpected[i].energy    , listActual[i].energy , 0.000000000001    );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].adc       , listActual[i].adc        );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].pedestal  , listActual[i].pedestal   );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].rms       , listActual[i].rms        );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i].status    , listActual[i].status     );
  //  CPPUNIT_ASSERT_EQUAL( listExpected[i], listActual[i] );
  //};

  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );
  
  delete variation;
}

void StjTowerEnergyVariationTest::testEnergyMinus5()
{
  StjTowerEnergyVariation* variation = new StjTowerEnergyVariation(-0.05);

  StjTowerEnergyList listIn;

  StjTowerEnergy energy1;
  energy1.energy = 3.0;
  listIn.push_back(energy1);

  StjTowerEnergy energy2;
  energy2.energy = 5.0;
  listIn.push_back(energy2);

  StjTowerEnergyList listExpected;
  energy1.energy *= (1.0 - 0.05);
  energy2.energy *= (1.0 - 0.05);
  listExpected.push_back(energy1);
  listExpected.push_back(energy2);
  
  // excercise
  StjTowerEnergyList listActual = (*variation)(listIn);
  
  // verify
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );
  
  delete variation;
}

void StjTowerEnergyVariationTest::testEnergyPlus10()
{
  StjTowerEnergyVariation* variation = new StjTowerEnergyVariation(0.1);

  StjTowerEnergyList listIn;

  StjTowerEnergy energy1;
  energy1.energy = 3.0;
  listIn.push_back(energy1);

  StjTowerEnergy energy2;
  energy2.energy = 5.0;
  listIn.push_back(energy2);

  StjTowerEnergyList listExpected;
  energy1.energy *= (1.0 + 0.1);
  energy2.energy *= (1.0 + 0.1);
  listExpected.push_back(energy1);
  listExpected.push_back(energy2);
  
  // excercise
  StjTowerEnergyList listActual = (*variation)(listIn);
  
  // verify
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );
  
  delete variation;
}

void StjTowerEnergyVariationTest::testEnergyMinus10()
{
  StjTowerEnergyVariation* variation = new StjTowerEnergyVariation(-0.1);

  StjTowerEnergyList listIn;

  StjTowerEnergy energy1;
  energy1.energy = 3.0;
  listIn.push_back(energy1);

  StjTowerEnergy energy2;
  energy2.energy = 5.0;
  listIn.push_back(energy2);

  StjTowerEnergyList listExpected;
  energy1.energy *= (1.0 - 0.1);
  energy2.energy *= (1.0 - 0.1);
  listExpected.push_back(energy1);
  listExpected.push_back(energy2);
  
  // excercise
  StjTowerEnergyList listActual = (*variation)(listIn);
  
  // verify
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );
  
  delete variation;
}


void StjTowerEnergyVariationTest::testEnergyNull()
{
  StjTowerEnergyVariation* variation = new StjTowerEnergyVariation(0);

  StjTowerEnergyList listIn;

  StjTowerEnergy energy1;
  energy1.energy = 3.0;
  listIn.push_back(energy1);

  StjTowerEnergy energy2;
  energy2.energy = 5.0;
  listIn.push_back(energy2);

  StjTowerEnergyList listExpected;
  listExpected.push_back(energy1);
  listExpected.push_back(energy2);
  
  // excercise
  StjTowerEnergyList listActual = (*variation)(listIn);
  
  // verify
  CPPUNIT_ASSERT_EQUAL( listExpected, listActual );
  
  delete variation;
}
