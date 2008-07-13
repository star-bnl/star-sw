// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StFourPMakers/StBET4pMaker.h>
#include <StFourPMakers/StBET4pMakerImp.h>
#include <StFourPMakers/StJetBEMCEnergyCut.h>
#include <StFourPMakers/StJetTPCTrackCut.h>

#include <StFourPMakers/StJetTPCMuDst.h>

#include "StBET4pMakerTest.hh"

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StBET4pMakerTest );

void StBET4pMakerTest::setUp() 
{

}

void StBET4pMakerTest::tearDown() 
{

}

void StBET4pMakerTest::testMacroInterface()
{
  StBET4pMaker* bet4pmaker = new StBET4pMaker("BET4pMaker", 0, true);
  bet4pmaker->setUseTPC(true);
  bet4pmaker->setUseBEMC(true);
  bet4pmaker->setUseEndcap(false);
  bet4pmaker->setUse2003Cuts(true);
  bet4pmaker->setUse2005Cuts(false);
  bet4pmaker->setUse2006Cuts(false);

  delete bet4pmaker;
}


void StBET4pMakerTest::testInit()
{
  StBET4pMaker* bet4pmaker = new StBET4pMaker("BET4pMaker", 0, true);

  CPPUNIT_ASSERT_EQUAL((StBET4pMakerImp*)0,  bet4pmaker->GetImp()  );
  bet4pmaker->Init();

  StBET4pMakerImp* imp = bet4pmaker->GetImp();
  CPPUNIT_ASSERT(  bet4pmaker->GetImp()  );

  delete bet4pmaker;
}
