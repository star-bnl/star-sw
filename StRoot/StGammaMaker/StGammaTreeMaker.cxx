#include <TFile.h>
#include <TTree.h>
#include <TString.h>
#include "TObjString.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StGammaRawMaker.h"

#include "StGammaTreeMaker.h"

ClassImp(StGammaTreeVersion);
ClassImp(StGammaTreeMaker);

using namespace std;

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaTreeVersion::StGammaTreeVersion(const char *name, const char *title): TNamed(name, title)
{}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaTreeVersion::~StGammaTreeVersion()
{}

//////////////////////////////////////////////////
//           Print version information          //
//////////////////////////////////////////////////
void StGammaTreeVersion::print()
{

    cout << "-- List of makers --" << endl << endl;
    for(UInt_t i = 0; i < mMakerTags.size(); i++)
    {
        cout << Form("[%i]: %s", (int)i, mMakerTags[i].Data()) << endl;
    }
    
    cout << endl;
    cout << "-- List of containers --" << endl <<  endl;
    for (UInt_t i = 0; i < mStorageTags.size(); i++)
    {
        cout << Form("[%i]: %s", (int)i, mStorageTags[i].Data()) << endl;
    }

}

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaTreeMaker::StGammaTreeMaker(const char *name): StMaker(name)
{
    mFilename="gamma_tree.root";
    mGammaFile  = 0;
    mGammaTree  = 0;
    mGammaEvent = 0;
    mSkipEmpty = true;
}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaTreeMaker::~StGammaTreeMaker()
{}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaTreeMaker::Init()
{

    // Create a file if necessary
    if(!mGammaFile) mGammaFile = new TFile(mFilename, "RECREATE");
    
    // Create a tree if necessary
    if(!mGammaTree) 
    {
        TString title = "Gamma TTree $Id: StGammaTreeMaker.cxx,v 1.12 2015/07/29 19:10:12 smirnovd Exp $ built " __DATE__ " " __TIME__;
        mGammaTree = new TTree("gammas", title);    
        mGammaTree->SetDirectory(mGammaFile);
    }
    
    // Retrieve StGammaEventMaker from the chain
    StGammaEventMaker *mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
    if(!mGammaEventMaker) 
    {
        LOG_DEBUG << "Make() - No StGammaEventMaker found!" << endm;
        return kStFatal;
    }
    
    mGammaEvent = mGammaEventMaker->event();
    mGammaTree->Branch("GammaEvent", &mGammaEvent, 32000, 99);
    mGammaTree->BranchRef(); // Ensure that the reference table is saved
    
    // Generate versioning information for each type of storage container
    mVersion.mStorageTags.push_back( TString(mGammaEvent->GetCVS()) );
    mVersion.mStorageTags.push_back( TString(mGammaEvent->newCandidate()->GetCVS()) );
    mVersion.mStorageTags.push_back( TString(mGammaEvent->newTower()->GetCVS()) );
    mVersion.mStorageTags.push_back( TString(mGammaEvent->newTrack()->GetCVS()) );
    mVersion.mStorageTags.push_back( TString(mGammaEvent->newStrip()->GetCVS()) );
    mVersion.mStorageTags.push_back( TString(mGammaEvent->candidate(0)->smdFit().GetCVS() ) ); 
    
    // Clear the current event
    mGammaEvent->Clear();
    
    assert(mGammaFile);
    assert(mGammaTree);
    
    return StMaker::Init();

}

//////////////////////////////////////////////////
//                 Maker Clear                  //
//////////////////////////////////////////////////
void StGammaTreeMaker::Clear(Option_t *opts)
{
    mGammaEvent->Clear();
    StMaker::Clear(opts);
}

//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
Int_t StGammaTreeMaker::Make()
{

    if( !GetDataSet("MuDst") ) 
    {
        LOG_DEBUG << "Make() - MuDst not found!" << endm;
        return kStFatal;
    }

    // Grabbing StGammaRawMaker was originally here,
    // is it still necessary?

    // Store event information in the tree
    if(mSkipEmpty)
    {
        if(mGammaEvent->numberOfCandidates() > 0) mGammaTree->Fill();
    }
    else
    {
        mGammaTree->Fill();
    }
    
    return kStOK;

}

//////////////////////////////////////////////////
//                 Maker Finish                 //
//////////////////////////////////////////////////
Int_t StGammaTreeMaker::Finish()
{

    // Write QA information to log files AND append information
    // to an object which we will save w/in the tree file
    
    TString cvstag = GetCVS();
    
    TIter next( GetParentChain()->GetMakeList());
    mVersion.mMakerTags.push_back( cvstag );
    
    StMaker *maker;
    while( (maker = (StMaker*)next()) )
    {
        cvstag = maker->GetCVS();
        mVersion.mMakerTags.push_back(cvstag);
    }
    
    LOG_QA <<" -- StGammaMaker versioning information follows --" << endm;
    mVersion.print();
    
    // Save tree
    mGammaFile->cd();
    mVersion.Write();
    mGammaTree->Write();
  
    return kStOK;
    
}
