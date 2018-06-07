#include "StTrgTreeMaker.h"

#include "StEventTypes.h"
#include "StEvent/StTriggerData.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "TFile.h"
#include "TTree.h"
#include "StMessMgr.h"

ClassImp(StTrgTreeMaker);

//-----------------------------------------------------------------------------
StTrgTreeMaker::StTrgTreeMaker(const char* name, const char* outName)
  : StMaker(name)
{
    LOG_INFO << "Instattiating StTrgTreeMaker with name "<<name<<endm;
    mOutName = outName;
}

//----------------------------------------------------------------------------- 
StTrgTreeMaker::~StTrgTreeMaker()
{ /*  */ }

//----------------------------------------------------------------------------- 
Int_t StTrgTreeMaker::Init() {
    if(mFile==0){
	LOG_INFO << "Creating "<<mOutName.Data()<<endm;
	mFile = new TFile(mOutName.Data(),"RECREATE");
	mTree = new TTree("trgtree", "trigger tree");
	mTree->Branch("bbcAdcSumE", &mTrg.bbcAdcSumE, "bbcAdcSumE/I");
	mTree->Branch("bbcAdcSumW", &mTrg.bbcAdcSumW, "bbcAdcSumW/I");
	mTree->Branch("bbcTacDiff", &mTrg.bbcTacDiff, "bbcTacDiff/I");
	mTree->Branch("vpdAdcSumE", &mTrg.vpdAdcSumE, "vpdAdcSumE/I");
	mTree->Branch("vpdAdcSumW", &mTrg.vpdAdcSumW, "vpdAdcSumW/I");
	mTree->Branch("vpdTacDiff", &mTrg.vpdTacDiff, "vpdTacDiff/F");
	mTree->Branch("zdcAdcSumE", &mTrg.zdcAdcSumE, "zdcAdcSumE/I");
	mTree->Branch("zdcAdcSumW", &mTrg.zdcAdcSumW, "zdcAdcSumW/I");
	mTree->Branch("zdcTacDiff", &mTrg.zdcTacDiff, "zdcTacDiff/I");
	mTree->Branch("tofMult",    &mTrg.tofMult,    "togMult/I");
	mTree->Branch("epdNHitsE",  &mTrg.epdNHitsE,  "epdNHitsE/I");
	mTree->Branch("epdNHitsW",  &mTrg.epdNHitsW,  "epdNHitsW/I");
	mTree->Branch("epdTacDiff", &mTrg.epdTacDiff, "epdTacDiff/I");
    }
    return kStOK;
}

//----------------------------------------------------------------------------- 
Int_t StTrgTreeMaker::Finish() {
    if(mOutName!="") {
        mFile->Write();
        mFile->Close();
    }
    return kStOK;
}

//----------------------------------------------------------------------------- 
void StTrgTreeMaker::Clear(Option_t *opt) {
}

//----------------------------------------------------------------------------- 
Int_t StTrgTreeMaker::Make() {
    const StMuDst* muDst = (StMuDst*)GetInputDS("MuDst");
    if(!muDst) { LOG_WARN<< "No MuDst Found!"<<endm; return kStWarn;}
    const StMuEvent* muEvt = muDst->event();
    if(!muEvt) { LOG_WARN<< "No MuEvent in MuDst!"<<endm; return kStWarn;}
    const StTriggerData* trg = muEvt->triggerData();
    if(!trg) { LOG_WARN<< "No Trigger Data in MuDst->MuEvent!"<<endm; return kStWarn;}
    
    //tof
    mTrg.tofMult    = trg->tofMultiplicity();

    //bbc
    mTrg.bbcAdcSumE = trg->bbcADCSum(east);
    mTrg.bbcAdcSumW = trg->bbcADCSum(west);
    mTrg.bbcTacDiff = trg->bbcTimeDifference();

    //zdc
    mTrg.zdcAdcSumE = trg->zdcUnAttenuated(east);
    mTrg.zdcAdcSumW = trg->zdcUnAttenuated(west);
    mTrg.zdcTacDiff = trg->zdcTimeDifference();

    //vpd
    mTrg.vpdAdcSumE = (trg->bbcVP101(4) & 0x7ff);
    mTrg.vpdAdcSumW = (trg->bbcVP101(6) & 0x7ff);
    unsigned int ne=(trg->bbcVP101(4) >> 11) & 0x1f;
    unsigned int nw=(trg->bbcVP101(6) >> 11) & 0x1f;
    unsigned int se=trg->bbcVP101(5);
    unsigned int sw=trg->bbcVP101(7);
    int nwse=nw*se;
    int nesw=ne*sw;
    int nenw=ne*nw;
    float dvpd = -1000.0;
    if(nenw>0) dvpd=float(nwse-nesw)/float(nenw);
    mTrg.vpdTacDiff = dvpd;
    
    //epd
    unsigned short dsmEP001OutR = trg->epdLayer1(0, 0);
    unsigned short dsmEP001OutL = trg->epdLayer1(1, 0);
    unsigned int dsmEP001Out = (dsmEP001OutL<<16) + dsmEP001OutR;
    int maxTac03 = (dsmEP001Out >> 12) & 0xfff; // max tac from channels 0:3                          
    int maxTac47 = (dsmEP001Out >> 0 ) & 0xfff; // max tac from channels 4:7                          
    int hitCountEast = (dsmEP001Out >> 24) & 0xff;
    int maxTacEast   = (maxTac03>maxTac47)?maxTac03:maxTac47;
    unsigned short dsmEP002OutR = trg->epdLayer1(2, 0);
    unsigned short dsmEP002OutL = trg->epdLayer1(3, 0);
    unsigned int dsmEP002Out = (dsmEP002OutL<<16) + dsmEP002OutR;
    maxTac03 = (dsmEP002Out >> 12) & 0xfff; // max tac from channels 0:3                              
    maxTac47 = (dsmEP002Out >> 0 ) & 0xfff; // max tac from channels 4:7                              
    int hitCountWest = (dsmEP002Out >> 24) & 0xff;
    int maxTacWest   = (maxTac03>maxTac47)?maxTac03:maxTac47;
    unsigned short dsmL2EpdOutput = trg->vertexDSM(5);
    int tacDiff = (unsigned short)(dsmL2EpdOutput & 0x1fff); //(0-12)   EPD TAC-Difference                
    mTrg.epdNHitsE = hitCountEast;
    mTrg.epdNHitsW = hitCountWest;
    mTrg.epdTacDiff = tacDiff;
    mTree->Fill();
    return kStOK;
}
