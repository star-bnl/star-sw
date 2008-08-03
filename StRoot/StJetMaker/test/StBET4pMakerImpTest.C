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
  tpc = new StjTPCMock(35);
  bemc = new StjBEMCMock(4500);
  eemc = new StjEEMCMock(1000);

  tpcCut = new StjTrackListCut();
  tpcCut->addCut(new StjTrackCutMock());

  bemcCut = new StjTowerEnergyListCut();
  bemcCut->addCut(new StjTowerEnergyCutMock());

  corr = new StjTowerEnergyCorrectionForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  pair<StjTrackList, StjTowerEnergyList> actual = imp->getTrackAndEnergyList();
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
  tpc = new StjTPCMock(35);
  bemc = new StjBEMCMock(4500);
  eemc = new StjEEMCMock(1000);

  tpcCut = new StjTrackListCut();

  bemcCut = new StjTowerEnergyListCut();

  corr = new StjTowerEnergyCorrectionForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  pair<StjTrackList, StjTowerEnergyList> actual = imp->getTrackAndEnergyList();
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
