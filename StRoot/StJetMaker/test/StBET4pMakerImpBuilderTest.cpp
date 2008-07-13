// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>

#include <StFourPMakers/StBET4pMakerImpBuilder.h>
#include <StFourPMakers/StBET4pMakerImp.h>

#include <StFourPMakers/StJetTPCMuDst.h>
#include <StFourPMakers/StJetBEMCMuDst.h>
#include <StFourPMakers/StJetEEMCMuDst.h>

#include <StFourPMakers/StJetBEMCEnergyCut.h>
#include <StFourPMakers/StJetTPCTrackCut.h>

#include "StBET4pMakerImpBuilderTest.hh"

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;

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
  CPPUNIT_ASSERT_EQUAL( false,  tpcCut->Use2006Cuts()  );

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
  CPPUNIT_ASSERT_EQUAL( false,  tpcCut->Use2006Cuts()  );

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
  CPPUNIT_ASSERT_EQUAL(  true,  tpcCut->Use2006Cuts()  );

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
  CPPUNIT_ASSERT_EQUAL(  true,  tpcCut->Use2006Cuts()  );

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
  CPPUNIT_ASSERT_EQUAL(  true,  tpcCut->Use2006Cuts()  );

  delete imp;
 }
