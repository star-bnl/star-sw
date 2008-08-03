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

  CPPUNIT_ASSERT(  dynamic_cast<StjTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StjBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StjEEMCNull*>(imp->EEMC())  );

  StjTowerEnergyListCut* bemcCut = imp->getBEMCEnergyCut();
  StjTowerEnergyListCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCut2003BemcTower*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutEnergy*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcStatus*>(bemcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutAdc*>(bemcCutList[3])     );


  StjTrackListCut* tpcCut    = imp->getTPCTrackCut();
  StjTrackListCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutEta*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutPossibleHitRatio*>(tpcCutList[2])     );

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


  CPPUNIT_ASSERT(  dynamic_cast<StjTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StjBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StjEEMCNull*>(imp->EEMC())  );

  StjTowerEnergyListCut* bemcCut = imp->getBEMCEnergyCut();
  StjTowerEnergyListCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcWestOnly*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutEnergy*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcStatus*>(bemcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutAdc*>(bemcCutList[3])     );

  StjTrackListCut* tpcCut    = imp->getTPCTrackCut();
  StjTrackListCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutEta*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutPossibleHitRatio*>(tpcCutList[2])     );

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


  CPPUNIT_ASSERT(  dynamic_cast<StjTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StjBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StjEEMCMuDst*>(imp->EEMC())  );

  StjTowerEnergyListCut* bemcCut = imp->getBEMCEnergyCut();
  StjTowerEnergyListCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutEnergy*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcStatus*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutAdc*>(bemcCutList[2])     );


  StjTrackListCut* tpcCut    = imp->getTPCTrackCut();
  StjTrackListCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDcaPtDependent*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutEta*>(tpcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutPossibleHitRatio*>(tpcCutList[3])     );

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


  CPPUNIT_ASSERT(  dynamic_cast<StjTPCMuDst*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StjBEMCNull*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StjEEMCMuDst*>(imp->EEMC())  );

  StjTowerEnergyListCut* bemcCut = imp->getBEMCEnergyCut();
  StjTowerEnergyListCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, bemcCutList.size() );

  StjTrackListCut* tpcCut    = imp->getTPCTrackCut();
  StjTrackListCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDcaPtDependent*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutEta*>(tpcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutPossibleHitRatio*>(tpcCutList[3])     );

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


  CPPUNIT_ASSERT(  dynamic_cast<StjTPCNull*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StjBEMCMuDst*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StjEEMCMuDst*>(imp->EEMC())  );

  StjTowerEnergyListCut* bemcCut = imp->getBEMCEnergyCut();
  StjTowerEnergyListCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutEnergy*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcStatus*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutAdc*>(bemcCutList[2])     );


  StjTrackListCut* tpcCut    = imp->getTPCTrackCut();
  StjTrackListCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)0, tpcCutList.size() );

  delete imp;
 }

 void StBET4pMakerImpBuilderTest::testReadTree()
 {
   //  StjTreeEntryMaker *maker = new StjTreeEntryMaker("entryMaker", "/star/institutions/mit/tai/testData/jetpart_6143024.root");
  StjTreeEntryMaker *maker = new StjTreeEntryMaker("entryMaker", "./jetpart_6143024.root");
  bool         useTPC = true;
  bool        useBEMC = true;
  bool        useEEMC = false;
  bool    use2003Cuts = false;
  bool    use2005Cuts = true;
  bool    use2006Cuts = false;

  StBET4pMakerImpBuilder builder;
  StBET4pMakerImp* imp =  builder.build(useTPC, useBEMC, useEEMC, use2003Cuts, use2005Cuts, use2006Cuts, maker);

  imp->Init();


  CPPUNIT_ASSERT(  dynamic_cast<StjTPCTree*>(imp->TPC())   );
  CPPUNIT_ASSERT(  dynamic_cast<StjBEMCTree*>(imp->BEMC()) );
  CPPUNIT_ASSERT(  dynamic_cast<StjEEMCNull*>(imp->EEMC())  );

  StjTowerEnergyListCut* bemcCut = imp->getBEMCEnergyCut();
  StjTowerEnergyListCut::CutList bemcCutList = bemcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)4, bemcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcWestOnly*>(bemcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutEnergy*>(bemcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutBemcStatus*>(bemcCutList[2])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTowerEnergyCutAdc*>(bemcCutList[3])     );

  StjTrackListCut* tpcCut    = imp->getTPCTrackCut();
  StjTrackListCut::CutList tpcCutList = tpcCut->getCutList();
  CPPUNIT_ASSERT_EQUAL( (size_t)3, tpcCutList.size() );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutDca*>(tpcCutList[0])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutEta*>(tpcCutList[1])     );
  CPPUNIT_ASSERT(   dynamic_cast<StjTrackCutPossibleHitRatio*>(tpcCutList[2])     );

  delete imp;
 }
