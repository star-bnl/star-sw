// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTrackTowerEnergyListToFourVecList.h>

#include <StjTowerEnergyList.h>
#include <StjTrackList.h>
#include <StjFourVecList.h>

#include "StjTrackTowerEnergyListToFourVecListTest.hh"

#include <iostream>

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTrackTowerEnergyListToFourVecListTest );

void StjTrackTowerEnergyListToFourVecListTest::setUp() 
{

}

void StjTrackTowerEnergyListToFourVecListTest::tearDown()
{

}

void StjTrackTowerEnergyListToFourVecListTest::testOne()
{
  StjTrackList trackList;

  StjTrack track1;
  track1.runNumber  = 100;
  track1.eventId    = 10;
  track1.detectorId = 1;
  track1.id    = 1;
  track1.pt  = 0.2;
  track1.eta = 0.3;
  track1.phi = 0.1;
  trackList.push_back(track1);

  StjTrack track2;
  track2.runNumber  = 100;
  track2.eventId    = 10;
  track2.detectorId = 1;
  track2.id    = 2;
  track2.pt  = 0.3;
  track2.eta = 0.1;
  track2.phi = 0.2;
  trackList.push_back(track2);

  StjTowerEnergyList energyList;

  StjTowerEnergy energy1;
  energy1.runNumber  = 100;
  energy1.eventId    = 10;
  energy1.detectorId = 9;
  energy1.towerId    = 9;
  energy1.energy     = 3.2;
  energy1.towerR     = 225.0;
  energy1.towerEta   = 0.3;
  energy1.towerPhi   = 0.0;
  energy1.towerR     = 225.0;
  energy1.vertexX    = 0.0;
  energy1.vertexY    = 0.0;
  energy1.vertexZ    = 0.0;
  energyList.push_back(energy1);

  StjTowerEnergy energy2;
  energy2.runNumber  = 100;
  energy2.eventId    = 10;
  energy2.detectorId = 9;
  energy2.towerId    = 19;
  energy2.energy     = 3.2;
  energy2.towerR     = 225.0;
  energy2.towerEta   = 0.3;
  energy2.towerPhi   = 0.0;
  energy2.towerR     = 225.0;
  energy2.vertexX    = 0.0;
  energy2.vertexY    = 0.0;
  energy2.vertexZ    = 0.0;
  energyList.push_back(energy2);

  StjTowerEnergy energy3;
  energy3.runNumber  = 100;
  energy3.eventId    = 10;
  energy3.detectorId = 9;
  energy3.towerId    = 119;
  energy3.energy     = 3.2;
  energy3.towerR     = 225.0;
  energy3.towerEta   = 0.3;
  energy3.towerPhi   = 0.0;
  energy3.towerR     = 225.0;
  energy3.vertexX    = 0.0;
  energy3.vertexY    = 0.0;
  energy3.vertexZ    = 0.0;
  energyList.push_back(energy3);

  StjTrackTowerEnergyListToFourVecList toFourVec;

  StjFourVecList fourList = toFourVec(trackList, energyList);

  CPPUNIT_ASSERT_EQUAL( (size_t)5, fourList.size());
  for(StjFourVecList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {
//    cout << (*it).runNumber << " "
//	 << (*it).eventId   << " "
//	 << (*it).detectorId << " "
//	 << (*it).pt << " "
//	 << (*it).eta << " "
//	 << (*it).phi << " "
//	 << (*it).m << " "
//	 << endl;
  }

}
