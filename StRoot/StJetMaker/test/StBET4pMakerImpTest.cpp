// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>

#include <StFourPMakers/StBET4pMakerImp.h>
#include <StFourPMakers/StJetTPCTxt.h>
#include <StFourPMakers/StJetBEMCTxt.h>
#include <StFourPMakers/StJetEEMCTxt.h>

#include "StFourPMakers/StJetBEMCEnergyCut.h"
#include "StFourPMakers/StJetTPCTrackCut.h"

#include <StMuTrackFourVec.h>

#include <Rtypes.h>
#include <iostream>
#include <fstream>
#include <sstream>

#include "StBET4pMakerImpTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StBET4pMakerImpTest );

void StBET4pMakerImpTest::setUp() 
{

}

void StBET4pMakerImpTest::tearDown() 
{

}


void StBET4pMakerImpTest::testMake2003() 
{
  tpc = new StJetTPCTxt("./tpctracks.txt");
  bemc = new StJetBEMCTxt("./bemcenergy.txt");
  eemc = new StJetEEMCNull();

  tpcCut = new StJetTPCTrackCut();
  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCut2003BemcTower());
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  tpcCut->setUse2006Cuts(false);

  assertResults("StBET4pMakerImpTest_testMake_2003.txt");

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::testMake2005() 
{
  tpc = new StJetTPCTxt("./tpctracks.txt");
  bemc = new StJetBEMCTxt("./bemcenergy.txt");
  eemc = new StJetEEMCNull();

  tpcCut = new StJetTPCTrackCut();
  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  tpcCut->setUse2006Cuts(false);

  assertResults("StBET4pMakerImpTest_testMake_2005.txt");

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::testMake2006() 
{
  tpc = new StJetTPCTxt("./tpctracks.txt");
  bemc = new StJetBEMCTxt("./bemcenergy.txt");
  eemc = new StJetEEMCTxt("./eemcenergy.txt");

  tpcCut = new StJetTPCTrackCut();
  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  tpcCut->setUse2006Cuts(true);

  assertResults("StBET4pMakerImpTest_testMake_2006.txt");

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}



void StBET4pMakerImpTest::testMakeExpected2003() 
{
  tpc = new StJetTPCTxt("./tpctracks.txt");
  bemc = new StJetBEMCTxt("./bemcenergy.txt");
  eemc = new StJetEEMCNull();

  tpcCut = new StJetTPCTrackCut();
  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCut2003BemcTower());
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  tpcCut->setUse2006Cuts(false);

  writeExpected("StBET4pMakerImpTest_testMake_2003.txt");

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::testMakeExpected2005() 
{
  tpc = new StJetTPCTxt("./tpctracks.txt");
  bemc = new StJetBEMCTxt("./bemcenergy.txt");
  eemc = new StJetEEMCNull();

  tpcCut = new StJetTPCTrackCut();
  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  tpcCut->setUse2006Cuts(false);

  writeExpected("StBET4pMakerImpTest_testMake_2005.txt");

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::testMakeExpected2006() 
{
  tpc = new StJetTPCTxt("./tpctracks.txt");
  bemc = new StJetBEMCTxt("./bemcenergy.txt");
  eemc = new StJetEEMCTxt("./eemcenergy.txt");

  tpcCut = new StJetTPCTrackCut();
  bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  corr = new CorrectTowerEnergyForTracks();
  imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, corr, eemc);

  tpcCut->setUse2006Cuts(true);

  writeExpected("StBET4pMakerImpTest_testMake_2006.txt");

  delete imp;
  delete corr;
  delete bemcCut;
  delete tpcCut;
  delete eemc;
  delete bemc;
  delete tpc;
}

void StBET4pMakerImpTest::assertResults(const char *path)
{
  ifstream istr(path);

  for(Long64_t i = 0; i <= 127; ++i) {
    
    imp->Clear(0);

    imp->Make();

    FourList fourList = imp->getTracks();
    for(vector<AbstractFourVec*>::const_iterator fourp = fourList.begin(); fourp != fourList.end(); ++fourp) {
      StMuTrackFourVec* particle = dynamic_cast<StMuTrackFourVec*>(*fourp);
      StMuTrackEmu* track = particle->track() ? particle->track() : new StMuTrackEmu();

      string line;
      getline( istr, line);
      istringstream ist(line);
      int j;
      double px, py, phi, eta, eT, e, mass, charge;
      int getIndex;
      int detectorId;
      short flag;
      unsigned short nHits;
      Short_t charge_;
      unsigned short nHitsPoss, nHitsDedx, nHitsFit;
      double nSigmaPion, Tdca;
      Float_t dcaZ, dcaD;
      double etaext, phiext, dEdx;
      int trackIndex;
      short id;

      ist >> j 
	  >> px >> py >> phi >> eta >> eT >> e >> mass >> charge >> getIndex 
	//	  >> detectorId
	  >> flag >> nHits >>  charge_
	  >> nHitsPoss >>  nHitsDedx >> nHitsFit
	  >> nSigmaPion >> Tdca
	  >> dcaZ >> dcaD
	  >> etaext >> phiext >> dEdx
	  >> trackIndex
	  >> id;

      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->px()      , px          , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->py()      , py          , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->phi()     , phi         , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->eta()     , eta         , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->eT()      , eT          , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->e()       , e           , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->mass()    , mass        , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(particle->charge()  , charge      , 1e-4);
      CPPUNIT_ASSERT_EQUAL(        particle->getIndex(), getIndex      );
      CPPUNIT_ASSERT_EQUAL(        track->flag()       , flag          );
      CPPUNIT_ASSERT_EQUAL(        track->nHits()      , nHits         );
      CPPUNIT_ASSERT_EQUAL(        track->charge()     , charge_       );
      CPPUNIT_ASSERT_EQUAL(        track->nHitsPoss()  , nHitsPoss     );
      CPPUNIT_ASSERT_EQUAL(        track->nHitsDedx()  , nHitsDedx     );
      CPPUNIT_ASSERT_EQUAL(        track->nHitsFit()   , nHitsFit      );
      CPPUNIT_ASSERT_DOUBLES_EQUAL(track->nSigmaPion() , nSigmaPion  , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(track->Tdca()       , Tdca        , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(track->dcaZ()       , dcaZ        , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(track->etaext()     , etaext      , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(track->phiext()     , phiext      , 1e-4);
      CPPUNIT_ASSERT_DOUBLES_EQUAL(track->dEdx()       , dEdx        , 1e-4);
      CPPUNIT_ASSERT_EQUAL(        track->trackIndex() , trackIndex    );
      CPPUNIT_ASSERT_EQUAL(        track->id()         , id            );

    }
  }

}

void StBET4pMakerImpTest::writeExpected(const char *path) 
{
  ofstream ofs(path);
  
  for(Long64_t i = 0; i <= 127; ++i) {
    
    imp->Clear(0);

    imp->Make();

    FourList fourList = imp->getTracks();
    for(vector<AbstractFourVec*>::const_iterator fourp = fourList.begin(); fourp != fourList.end(); ++fourp) {
      StMuTrackFourVec* particle = dynamic_cast<StMuTrackFourVec*>(*fourp);
      StMuTrackEmu* track = particle->track() ? particle->track() : new StMuTrackEmu();
      

      ofs << i << " "
	  << particle->px() << " "
	  << particle->py() << " "
	  << particle->phi() << " "
	  << particle->eta() << " "
	  << particle->eT() << " "
	  << particle->e() << " "
	  << particle->mass() << " "
	  << particle->charge() << " "
	  << particle->getIndex() << " "
	//	  << particle->detectorId() << " " 
	  << track->flag()  << " " 
	  << track->nHits() << " " 
	  << track->charge() << " " 
	  << track->nHitsPoss() << " " 
	  << track->nHitsDedx() << " " 
	  << track->nHitsFit() << " " 
	  << track->nSigmaPion() << " " 
	  << track->Tdca() << " " 
	  << track->dcaZ() << " " 
	  << track->dcaD() << " " 
	  << track->etaext() << " " 
	  << track->phiext() << " " 
	  << track->dEdx() << " " 
	  << track->trackIndex() << " " 
	  << track->id();
      ofs << endl;

      if(!(particle->track())) delete track;
    }
  }

}

