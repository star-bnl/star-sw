// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetMakerTest.hh"

#include <StJetMaker.h>
#include <StppAnaPars.h>
#include <StConePars.h>
#include <StjeDefaultJetTreeWriter.h>

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StJetMakerTest );

void StJetMakerTest::setUp() 
{

}

void StJetMakerTest::tearDown() 
{

}

void StJetMakerTest::testConstruct() 
{
  StJetMaker* jetmaker = new StJetMaker("emcJetMaker", 0, "");
  delete jetmaker;
}

void StJetMakerTest::testMacroInterface()
{
  StJetMaker* jetmaker = new StJetMaker("emcJetMaker", 0, "");

  StppAnaPars* anapars = new StppAnaPars();
  StConePars* cpars = new StConePars();
  jetmaker->addAnalyzer(anapars, cpars, 0, "ConeJets5");
  delete jetmaker;
}

void StJetMakerTest::testTreeWriter()
{
  StJetMaker* jetmaker = new StJetMaker("emcJetMaker", 0, "test.root");

  StppAnaPars* anapars = new StppAnaPars();
  StConePars* cpars = new StConePars();
  jetmaker->addAnalyzer(anapars, cpars, 0, "ConeJets12");

  CPPUNIT_ASSERT(static_cast<StjeDefaultJetTreeWriter*>(jetmaker->getTreeWriter()));

  jetmaker->Init();
  delete jetmaker;
}

void StJetMakerTest::testInit()
{
  StJetMaker* jetmaker = new StJetMaker("emcJetMaker", 0, "test.root");

  StppAnaPars* anapars = new StppAnaPars();
  StConePars* cpars = new StConePars();
  jetmaker->addAnalyzer(anapars, cpars, 0, "ConeJets12");

  jetmaker->Init();
  delete jetmaker;
}
