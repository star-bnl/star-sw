// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StBET4pMakerImp.h>
#include <StjTPCTxt.h>
#include <StjBEMCTxt.h>
#include <StjEEMCTxt.h>

#include "StjTrackListCut.h"

#include <StjTrackCutDca.h>
#include <StjTrackCutDcaPtDependent.h>
#include <StjTrackCutEta.h>
#include <StjTrackCutPossibleHitRatio.h>

#include "StjTowerEnergyListCut.h"

#include <StjTowerEnergyCut2003BemcTower.h>
#include <StjTowerEnergyCutBemcWestOnly.h>
#include <StjTowerEnergyCutEnergy.h>
#include <StjTowerEnergyCutBemcStatus.h>
#include <StjTowerEnergyCutAdc.h>

#include "StjTowerEnergyCorrectionForTracks.h"

#include <iostream>
#include <fstream>
#include <sstream>

#include "StBET4pMakerImpTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;
using namespace StJetTrackCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StBET4pMakerImpTest );

void StBET4pMakerImpTest::setUp() 
{

}

void StBET4pMakerImpTest::tearDown() 
{

}

void StBET4pMakerImpTest::testGetTrackAndEnergyList_withCut() 
{
  tpc = new StJetTPCMock(35);
  bemc = new StJetBEMCMock(4500);
  eemc = new StJetEEMCMock(1000);

  tpcCut = new StJetTPCTrackCut();
  tpcCut->addCut(new TrackCutMock());

  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCutMock());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  pair<TrackList, TowerEnergyList> actual = imp->getTrackAndEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, actual.first.size() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1000, actual.second.size() );

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::testGetTrackAndEnergyList_withoutCut() 
{
  tpc = new StJetTPCMock(35);
  bemc = new StJetBEMCMock(4500);
  eemc = new StJetEEMCMock(1000);

  tpcCut = new StJetTPCTrackCut();

  bemcCut = new StJetBEMCEnergyCut();

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  pair<TrackList, TowerEnergyList> actual = imp->getTrackAndEnergyList();
  CPPUNIT_ASSERT_EQUAL( (size_t)35, actual.first.size() );
  CPPUNIT_ASSERT_EQUAL( (size_t)5500, actual.second.size() );

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::testMake()
{

}
