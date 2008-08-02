// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>

#include <StBET4pMakerImpBuilder.h>
#include <StBET4pMakerImp.h>

#include <StjTPCMuDst.h>
#include <StjBEMCMuDst.h>
#include <StjEEMCMuDst.h>
#include <StjTPCTree.h>
#include <StjBEMCTree.h>

#include <StjTrackListCut.h>

#include <StjTrackCutDca.h>
#include <StjTrackCutDcaPtDependent.h>
#include <StjTrackCutEta.h>
#include <StjTrackCutPossibleHitRatio.h>

#include <StjTowerEnergyListCut.h>

#include <StjTowerEnergyCut2003BemcTower.h>
#include <StjTowerEnergyCutBemcWestOnly.h>
#include <StjTowerEnergyCutEnergy.h>
#include <StjTowerEnergyCutBemcStatus.h>
#include <StjTowerEnergyCutAdc.h>

#include <StjTreeEntryMaker.h>

#include "StBET4pMakerImpBuilderTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;
using namespace StJetTrackCut;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StBET4pMakerImpBuilderTest );

void StBET4pMakerImpBuilderTest::setUp() 
{

}

void StBET4pMakerImpBuilderTest::tearDown() 
{

}

void StBET4pMakerImpBuilderTest::test2003()
{
  StMuDstMaker* uDstMaker(0);
  bool doTowerSwapFix = true;
  bool         useTPC = true;
  bool        useBEMC = true;
  bool        useEEMC = false;
  bool    use2003Cuts = true;
  bool    use2005Cuts = false;
  bool    use2006Cuts = false;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, uDstMaker, doTowerSwapFix);

  imp->Init();

  CPPUNIT_ASSERT(  dynamic_cast<StJetTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StJetBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StJetEEMCNull*>(imp->EEMC())  );

  StJetBEMCEnergyCut* bemcCut = imp->getBEMCEnergyCut();
  StJetBEMCEnergyCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCut2003BemcTower*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutEnergy*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcStatus*>(bemcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutAdc*>(bemcCutList[3])     );


  StJetTPCTrackCut* tpcCut    = imp->getTPCTrackCut();
  StJetTPCTrackCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutEta*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutPossibleHitRatio*>(tpcCutList[2])     );

  delete imp;
}

 void StBET4pMakerImpBuilderTest::test2005()
 {
  StMuDstMaker* uDstMaker(0);
  bool doTowerSwapFix = true;
  bool         useTPC = true;
  bool        useBEMC = true;
  bool        useEEMC = false;
  bool    use2003Cuts = false;
  bool    use2005Cuts = true;
  bool    use2006Cuts = false;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, uDstMaker, doTowerSwapFix);

  imp->Init();


  CPPUNIT_ASSERT(  dynamic_cast<StJetTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StJetBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StJetEEMCNull*>(imp->EEMC())  );

  StJetBEMCEnergyCut* bemcCut = imp->getBEMCEnergyCut();
  StJetBEMCEnergyCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcWestOnly*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutEnergy*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcStatus*>(bemcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutAdc*>(bemcCutList[3])     );

  StJetTPCTrackCut* tpcCut    = imp->getTPCTrackCut();
  StJetTPCTrackCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutEta*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutPossibleHitRatio*>(tpcCutList[2])     );

  delete imp;
 }
 
 void StBET4pMakerImpBuilderTest::test2006()
 {
  StMuDstMaker* uDstMaker(0);
  bool doTowerSwapFix = true;
  bool         useTPC = true;
  bool        useBEMC = true;
  bool        useEEMC = true;
  bool    use2003Cuts = false;
  bool    use2005Cuts = false;
  bool    use2006Cuts = true;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, uDstMaker, doTowerSwapFix);

  imp->Init();


  CPPUNIT_ASSERT(  dynamic_cast<StJetTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StJetBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StJetEEMCMuDst*>(imp->EEMC())  );

  StJetBEMCEnergyCut* bemcCut = imp->getBEMCEnergyCut();
  StJetBEMCEnergyCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutEnergy*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcStatus*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutAdc*>(bemcCutList[2])     );


  StJetTPCTrackCut* tpcCut    = imp->getTPCTrackCut();
  StJetTPCTrackCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDcaPtDependent*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutEta*>(tpcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutPossibleHitRatio*>(tpcCutList[3])     );

  delete imp;
 }
 
 void StBET4pMakerImpBuilderTest::testBEMCout()
 {
  StMuDstMaker* uDstMaker(0);
  bool doTowerSwapFix = true;
  bool         useTPC = true;
  bool        useBEMC = false;
  bool        useEEMC = true;
  bool    use2003Cuts = false;
  bool    use2005Cuts = false;
  bool    use2006Cuts = true;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, uDstMaker, doTowerSwapFix);

  imp->Init();


  CPPUNIT_ASSERT(  dynamic_cast<StJetTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StJetBEMCNull*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StJetEEMCMuDst*>(imp->EEMC())  );

  StJetBEMCEnergyCut* bemcCut = imp->getBEMCEnergyCut();
  StJetBEMCEnergyCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, bemcCutList.size() );

  StJetTPCTrackCut* tpcCut    = imp->getTPCTrackCut();
  StJetTPCTrackCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDcaPtDependent*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutEta*>(tpcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutPossibleHitRatio*>(tpcCutList[3])     );

  delete imp;
 }
 
 void StBET4pMakerImpBuilderTest::testTPCout()
 {
  StMuDstMaker* uDstMaker(0);
  bool doTowerSwapFix = true;
  bool         useTPC = false;
  bool        useBEMC = true;
  bool        useEEMC = true;
  bool    use2003Cuts = false;
  bool    use2005Cuts = false;
  bool    use2006Cuts = true;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, uDstMaker, doTowerSwapFix);

  imp->Init();


  CPPUNIT_ASSERT(  dynamic_cast<StJetTPCNull*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StJetBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StJetEEMCMuDst*>(imp->EEMC())  );

  StJetBEMCEnergyCut* bemcCut = imp->getBEMCEnergyCut();
  StJetBEMCEnergyCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutEnergy*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcStatus*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutAdc*>(bemcCutList[2])     );


  StJetTPCTrackCut* tpcCut    = imp->getTPCTrackCut();
  StJetTPCTrackCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, tpcCutList.size() );

  delete imp;
 }

 void StBET4pMakerImpBuilderTest::testReadTree()
 {
   //  StJetTreeEntryMaker *maker = new StJetTreeEntryMaker("entryMaker", "/star/institutions/mit/tai/testData/jetpart_6143024.root");
  StJetTreeEntryMaker *maker = new StJetTreeEntryMaker("entryMaker", "./jetpart_6143024.root");
  bool         useTPC = true;
  bool        useBEMC = true;
  bool        useEEMC = false;
  bool    use2003Cuts = false;
  bool    use2005Cuts = true;
  bool    use2006Cuts = false;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, maker);

  imp->Init();


  CPPUNIT_ASSERT(  dynamic_cast<StJetTPCTree*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StJetBEMCTree*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StJetEEMCNull*>(imp->EEMC())  );

  StJetBEMCEnergyCut* bemcCut = imp->getBEMCEnergyCut();
  StJetBEMCEnergyCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcWestOnly*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutEnergy*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutBemcStatus*>(bemcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<TowerEnergyCutAdc*>(bemcCutList[3])     );

  StJetTPCTrackCut* tpcCut    = imp->getTPCTrackCut();
  StJetTPCTrackCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutEta*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<TrackCutPossibleHitRatio*>(tpcCutList[2])     );

  delete imp;
 }
