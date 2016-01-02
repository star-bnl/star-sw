#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "TFile.h"
#include "TClonesArray.h"
#include "TTree.h"

#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoDstMaker/StPicoTrack.h"
#include "StPicoDstMaker/StPicoEmcPidTraits.h"
#include "StPicoDstMaker/StPicoBTofPidTraits.h"
#include "StPicoNpeEventMaker/StPicoNpeEvent.h"
#include "StPicoNpeEventMaker/StElectronPair.h"
#include "StBTofUtil/tofPathLength.hh"
#include "phys_constants.h"
#include "SystemOfUnits.h"

#include "StPicoNpeAnaMaker.h"
#include "StCuts.h"


ClassImp(StPicoNpeAnaMaker)

StPicoNpeAnaMaker::StPicoNpeAnaMaker(char const * name,char const * inputFilesList,
                                     char const * outName, StPicoDstMaker* picoDstMaker):
StMaker(name),mPicoDstMaker(picoDstMaker),mPicoNpeEvent(NULL), mOutFileName(outName), mInputFileList(inputFilesList),
mOutputFile(NULL), mChain(NULL), mEventCounter(0)
{}

Int_t StPicoNpeAnaMaker::Init()
{
    mPicoNpeEvent = new StPicoNpeEvent();
    
    mChain = new TChain("T");
    std::ifstream listOfFiles(mInputFileList.Data());
    if (listOfFiles.is_open())
    {
        std::string file;
        while (getline(listOfFiles, file))
        {
            LOG_INFO << "StPicoNpeAnaMaker - Adding :" << file << endm;
            mChain->Add(file.c_str());
        }
    }
    else
    {
        LOG_ERROR << "StPicoNpeAnaMaker - Could not open list of files. ABORT!" << endm;
        return kStErr;
    }
    mChain->GetBranch("npeEvent")->SetAutoDelete(kFALSE);
    mChain->SetBranchAddress("npeEvent", &mPicoNpeEvent);
    
    mOutputFile = new TFile(mOutFileName.Data(), "RECREATE");
    mOutputFile->cd();
    
    // -------------- USER VARIABLES -------------------------

    
    
    
    
    return kStOK;
}
//-----------------------------------------------------------------------------
StPicoNpeAnaMaker::~StPicoNpeAnaMaker()
{
    /*  */
}
//-----------------------------------------------------------------------------
Int_t StPicoNpeAnaMaker::Finish()
{
    LOG_INFO << " StPicoNpeAnaMaker - writing data and closing output file " <<endm;
    mOutputFile->cd();
    // --------------- USER HISTOGRAM WRITE --------------------
    
    
    

    mOutputFile->Close();
    
    return kStOK;
}
//-----------------------------------------------------------------------------
Int_t StPicoNpeAnaMaker::Make()
{
    readNextEvent();
    
    if (!mPicoDstMaker)
    {
        LOG_WARN << " StPicoNpeAnaMaker - No PicoDstMaker! Skip! " << endm;
        return kStWarn;
    }
    
    StPicoDst const* picoDst = mPicoDstMaker->picoDst();
    
    if (!picoDst)
    {
        LOG_WARN << "StPicoNpeAnaMaker - No PicoDst! Skip! " << endm;
        return kStWarn;
    }
    
    if(mPicoNpeEvent->runId() != picoDst->event()->runId() ||
       mPicoNpeEvent->eventId() != picoDst->event()->eventId())
    {
        LOG_ERROR <<" StPicoNpeAnaMaker - !!!!!!!!!!!! ATTENTION !!!!!!!!!!!!!"<<endm;
        LOG_ERROR <<" StPicoNpeAnaMaker - SOMETHING TERRIBLE JUST HAPPENED. StPicoEvent and StPicoNpeEvent are not in sync."<<endm;
        exit(1);
    }
    
    
    TClonesArray const * aElectronPair = mPicoNpeEvent->electronPairArray();
    for (int idx = 0; idx < aElectronPair->GetEntries(); ++idx)
    {
        // this is an example of how to get the ElectronPair pairs and their corresponsing tracks
        StElectronPair const* epair = (StElectronPair*)aElectronPair->At(idx);
        if(!isGoodPair(epair)) continue;
        
        StPicoTrack const* electron = picoDst->track(epair->electronIdx());
        StPicoTrack const* partner = picoDst->track(epair->partnerIdx());
        
        // -------------- USER ANALYSIS -------------------------
        
    }
    
    return kStOK;
}
//-----------------------------------------------------------------------------
bool StPicoNpeAnaMaker::isGoodPair(StElectronPair const* const epair) const
{
    if(!epair) return false;
    
    StPicoTrack const* electron = mPicoDstMaker->picoDst()->track(epair->electronIdx());
    StPicoTrack const* partner = mPicoDstMaker->picoDst()->track(epair->partnerIdx());
    
    return epair->pairMass() < anaCuts::pairMass &&
    epair->pairDca() < anaCuts::pairDca
    ;
}

