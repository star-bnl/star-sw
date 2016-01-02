#include "TTree.h"
#include "TFile.h"
#include "TChain.h"
#include "TH1.h"


#include "StarClassLibrary/StThreeVectorF.hh"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoDstMaker/StPicoTrack.h"
#include "StPicoDstMaker/StPicoBTofPidTraits.h"
#include "StPicoMixedEventMaker.h"
#include "StPicoEventMixer.h"
#include "StRoot/StRefMultCorr/StRefMultCorr.h"

#include <vector>

ClassImp(StPicoMixedEventMaker)

// _________________________________________________________
StPicoMixedEventMaker::StPicoMixedEventMaker(char const* name, StPicoDstMaker* picoMaker, StRefMultCorr* grefmultCorrUtil,
        char const* outputBaseFileName,  char const* inputHFListHFtree = "") :
    StMaker(name), mPicoDst(NULL), mPicoDstMaker(picoMaker),  mPicoEvent(NULL),
    mGRefMultCorrUtil(grefmultCorrUtil),
    mOuputFileBaseName(outputBaseFileName), mInputFileName(inputHFListHFtree),
    mEventCounter(0), mTree(NULL), mOutputFileTree(NULL) {

  TH1::AddDirectory(false);
    // -- create OutputTree
    mOutputFileTree = new TFile(Form("%s.picoMEtree.root", mOuputFileBaseName.Data()), "RECREATE");
    mOutputFileTree->SetCompressionLevel(1);
    mOutputFileTree->cd();
    int BufSize = (int)pow(2., 16.);
    int Split = 1;
}

// _________________________________________________________
StPicoMixedEventMaker::~StPicoMixedEventMaker() {

  delete mGRefMultCorrUtil;
  for(int iVz =0 ; iVz < 10 ; ++iVz){
    for(int iCentrality = 0 ; iCentrality < 9 ; ++iCentrality){
      delete mPicoEventMixer[iVz][iCentrality];
    }
  }
  mOutputFileTree->Close();
}
// _________________________________________________________
bool StPicoMixedEventMaker::loadEventPlaneCorr(Int_t const run) {
    //needs to implement, will currently break maker
    return false;
}
// _________________________________________________________
Int_t StPicoMixedEventMaker::Init() {
    mOutputFileTree->cd();
    for(int iVz =0 ; iVz < 10 ; ++iVz){
      for(int iCentrality = 0 ; iCentrality < 9 ; ++iCentrality){
	mPicoEventMixer[iVz][iCentrality] = new StPicoEventMixer(Form("Cent_%i_Vz_%i",iCentrality,iVz));
	mPicoEventMixer[iVz][iCentrality]->setEventBuffer(10);
      }
    }
    mGRefMultCorrUtil = new StRefMultCorr("grefmult");
    // if(!LoadEventPlaneCorr(mRunId)){
    // LOG_WARN << "Event plane calculations unavalable! Skipping"<<endm;
    // return kStOk;
    // }

    // -- reset event to be in a defined state
    //resetEvent();

    return kStOK;
}

// _________________________________________________________
Int_t StPicoMixedEventMaker::Finish() {
    mOutputFileTree->cd();
    for(int iVz =0 ; iVz < 10 ; ++iVz){
      for(int iCentrality = 0 ; iCentrality < 9 ; ++iCentrality){
	mPicoEventMixer[iVz][iCentrality]->finish();
	//delete mPicoEventMixer[iVz][iCentrality];
      }
    }
    return kStOK;
}
// _________________________________________________________
void StPicoMixedEventMaker::Clear(Option_t* opt) {
}
// _________________________________________________________
Int_t StPicoMixedEventMaker::Make() {

    if(!mPicoDstMaker) {
        LOG_WARN << "No PicoDstMaker! Skipping! "<<endm;
        return kStWarn;
    }

    StPicoDst const* picoDst = mPicoDstMaker->picoDst();
    if (!picoDst) {
        LOG_WARN << "No picoDst ! Skipping! "<<endm;
        return kStWarn;
    }
    // - GRefMultiplicty
    if(!mGRefMultCorrUtil) {
        LOG_WARN << " No mGRefMultCorrUtil! Skip! " << endl;
        return kStWarn;
    }
    StThreeVectorF const pVtx = picoDst->event()->primaryVertex();
    if( fabs(pVtx.z()) >=6.0 )
      return kStOk;
    mGRefMultCorrUtil->init(picoDst->event()->runId());
    mGRefMultCorrUtil->initEvent(picoDst->event()->grefMult(),pVtx.z(),picoDst->event()->ZDCx()) ;
    int const centrality  = mGRefMultCorrUtil->getCentralityBin9();
    if(centrality < 0 || centrality >8 ) return kStOk;
    int const vz_bin = (int)((6 +pVtx.z())/1.2) ;
    //     Bin       Centrality (16)   Centrality (9)
    //     -1           80-100%           80-100% // this one should be rejected in your centrality related analysis
    //     0            75-80%            70-80%
    //     1            70-75%            60-70%
    //     2            65-70%            50-60%
    //     3            60-65%            40-50%
    //     4            55-60%            30-40%
    //     5            50-55%            20-30%
    //     6            45-50%            10-20%
    //     7            40-45%             5-10%
    //     8            35-40%             0- 5%

    if( mPicoEventMixer[vz_bin][centrality] -> addPicoEvent(picoDst) ==  true )
      mPicoEventMixer[vz_bin][centrality]->mixEvents();

    return kStOk;
}
// _________________________________________________________
Int_t StPicoMixedEventMaker::SetCategories() {
    return kStOk;
}
// _________________________________________________________
int StPicoMixedEventMaker::categorize(StPicoDst const * picoDst ) {
    StThreeVectorF pVertex = (picoDst->event())->primaryVertex();
    if( fabs(pVertex.z())>6.0 ) return -99;
    int bin = -6.0 + (pVertex.z()+6.0)/1.2;
    return bin;
}
