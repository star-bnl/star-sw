// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include <StBET4pMaker.h>
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

void StBET4pMakerTest::testMacroInterface_Construct_mudst_swap()
{
  StBET4pMaker* maker = new StBET4pMaker("BET4pMaker", (StMuDstMaker*)0, true);
  CPPUNIT_ASSERT_EQUAL(  false, maker->useTree() );
  delete maker;
}

void StBET4pMakerTest::testMacroInterface_Construct_mudst()
{
  StBET4pMaker* maker = new StBET4pMaker("BET4pMaker", (StMuDstMaker*)0);
  CPPUNIT_ASSERT_EQUAL(  false, maker->useTree() );
  delete maker;
}

void StBET4pMakerTest::testMacroInterface_Construct_tree()
{
  StBET4pMaker* maker = new StBET4pMaker("BET4pMaker", (StJetTreeEntryMaker*)0);
  CPPUNIT_ASSERT_EQUAL(   true, maker->useTree() );
  delete maker;
}

void StBET4pMakerTest::testMacroInterface()
{
  StBET4pMaker* maker = new StBET4pMaker("BET4pMaker", 0, true);
  maker->setUseTPC(true);
  maker->setUseBEMC(true);
  maker->setUseEndcap(false);
  maker->setUse2003Cuts(true);
  maker->setUse2005Cuts(false);
  maker->setUse2006Cuts(false);
  maker->setUseBEMCEnergySum();

  delete maker;
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
