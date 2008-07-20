// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>
#include "LoopTest.hh"

#include <TSystem.h>
#include <TFile.h>
#include <TTree.h>
#include <TObject.h>


#include <Stiostream.h>
#include <StMessMgr.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>
#include <StJetFinder/StProtoJet.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StFourPMakers/StMuEmcPosition.h>
#include <StMuTrackFourVec.h>
#include <StJet.h>
#include <StJets.h>

#include <StFourPMakers/StBET4pMakerImp.h>
#include <StFourPMakers/StJetTPCMuDst.h>
#include <StFourPMakers/StJetBEMCMuDst.h>

#include <StEmcRawMaker/StBemcTables.h>

#include <St_db_Maker/St_db_Maker.h>
#include <StEmcADCtoEMaker/StEmcADCtoEMaker.h>
#include <StSpinPool/StSpinDbMaker/StSpinDbMaker.h>
#include <StTriggerUtilities/StTriggerSimuMaker.h>
#include <StTriggerUtilities/Bemc/StBemcTriggerSimu.h>
#include <StTriggerUtilities/L2Emulator/StGenericL2Emulator.h>
#include <StTriggerUtilities/L2Emulator/StL2_2006EmulatorMaker.h>

#include <StEEmcDbMaker/StEEmcDbMaker.h>

#include <StMuTrackFourVec.h>

#include <Rtypes.h>

using namespace std;
using namespace StSpinJet;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( LoopTest );

void LoopTest::setUp() 
{

}

void LoopTest::tearDown() 
{

}

void LoopTest::testOne() 
{
//  StChain *chain= new StChain("StChain"); 
//
//  StMuDstMaker* mudstMaker = new StMuDstMaker(0,0, "", "/star/institutions/mit/tai/mudst_/st_physics_7156024_raw_1010001.MuDst.root", "", 1,"MuDst");
//
//  StEmcADCtoEMaker *adc = new StEmcADCtoEMaker("Eread");
//
//  StJetTPC* tpc = new StJetTPCMuDst(mudstMaker);
//  CollectChargedTracksFromTPC* collTrack = new CollectChargedTracksFromTPC(tpc);
//  StBemcTables* bemcTbl = new StBemcTables(true);
//  StJetBEMC *bemc = new StJetBEMCMuDst(mudstMaker, bemcTbl);
//  CollectEnergyDepositsFromBEMC* collBemc = new CollectEnergyDepositsFromBEMC(bemc);
//  CollectEnergyDepositsFromEEMC* collEemc = new CollectEnergyDepositsFromEEMC(mudstMaker);
//  CorrectTowerEnergyForTracks* corr = new CorrectTowerEnergyForTracks();
//  StBET4pMakerImp *imp = new StBET4pMakerImp(collTrack, collBemc, collEemc, corr);
//  imp->setUseEndcap(false);
//  imp->setUseBEMC(false);
//
//  chain->Init();
//
//  for (Int_t i = 0; i < 20; ++i) {
//    chain->Clear();
//    imp->Clear(0);
//
//    bemcTbl->loadTables((StMaker*)adc);
//
//    int ret = chain->Make(i); 
//    if(ret == kStEOF) break;
//
//    imp->Make();
//  }

  //  chain->Finish();
}
