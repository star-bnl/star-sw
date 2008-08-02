// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StjTowerEnergyListToStMuTrackFourVecList.h>
#include <StMuTrackFourVec.h>

#include "StjTowerEnergyListToStMuTrackFourVecListTest.hh"


using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( TowerEnergyListToStMuTrackFourVecListTest );

void TowerEnergyListToStMuTrackFourVecListTest::setUp() 
{

}

void TowerEnergyListToStMuTrackFourVecListTest::tearDown() 
{

}

void TowerEnergyListToStMuTrackFourVecListTest::testOne() 
{
  TowerEnergyListToStMuTrackFourVecList etof;
  TowerEnergyList elist;

  TowerEnergy energy;
  energy.energy   =  1.8530849;
  energy.towerR   =  225.40499;
  energy.towerEta =  0.8750000;
  energy.towerPhi = -1.281418;
  energy.vertexX =  -0.840182;
  energy.vertexY =   0.0856855;
  energy.vertexZ =  -78.94999;
  elist.push_back(energy);

  FourList flist = etof(elist);
  CPPUNIT_ASSERT_EQUAL( (size_t)1, flist.size());
  StMuTrackFourVec *p4 = (StMuTrackFourVec*)flist[0];
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1086495 , p4->pt()   , 1e-5);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.10221   , p4->eta()  , 1e-5);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-1.277958  , p4->phi()  , 1e-5);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0       , p4->mass() , 1e-5);

}

