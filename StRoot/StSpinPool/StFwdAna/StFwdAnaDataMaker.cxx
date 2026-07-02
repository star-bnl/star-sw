#include <chrono>

#include "StEnumerations.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StEventTypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StSpinPool/StFcsQaMaker/StFcsQaMaker.h"
#include "StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"
#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StThreeVectorF.hh"
#include "Stypes.h"
#include "TBox.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TLine.h"
#include "TMarker.h"
#include "TROOT.h"
#include "TString.h"
#include "TStyle.h"
#include "TText.h"

#include "StFwdAnaDataMaker.h"

ClassImp(StFwdAnaDataMaker)


StFwdAnaDataMaker::StFwdAnaDataMaker(const Char_t* name) : StMaker(name)
{
}

StFwdAnaDataMaker::~StFwdAnaDataMaker()
{
  //std::cout << "StFwdAnaDataMaker::~StFwdAnaDataMaker()" << std::endl;
  delete mAnaData;
  if( mInternalHists ){ delete mHists; } //This deletes the file too
  for( unsigned int i=0; i<mAnaList.size(); ++i ){
    delete mAnaList.at(i);
  }
  mAnaList.clear();
}

UInt_t StFwdAnaDataMaker::addAna(StFwdAnaVirtual* ana)
{
  if( ana==0 ){ return -1; }
  mAnaList.push_back(ana);
  return mAnaList.size()-1;
}

void StFwdAnaDataMaker::setAnaData( StFwdAnaData* anadata )
{
  if( anadata==0 ){ return; }
  mAnaData = anadata;
}

void StFwdAnaDataMaker::setHistManager( HistManager* hm )
{
  if( mInternalHists ){ delete mHists; mHists = 0; }
  mInternalHists = false;
  if( mHists!=0 ){ LOG_WARN << "StFwdAnaDataMaker::setHistManager() - HistManager exists and is external - no changes made" << endm; return; }
  else{ mHists = hm; }
}

UInt_t StFwdAnaDataMaker::LoadDataFromFile(TFile* file)
{
  if( file==0 || file->IsZombie() ){ LOG_ERROR << "StFwdAnaDataMaker::LoadDataFromFile() - ERROR:Unable to load from null file or zombie file" << std::endl; return 0; }
  if( mAnaData == 0 ){
    LOG_ERROR << "StFwdAnaDataMaker::LoadDataFromFile() - No StFwdAnaData object set" << std::endl;
    return 0;
  }
  
  mAnaData->loadTree(file);
  mAnaData->readFcsTrigTxtFile(mFcsTrigFilename.Data());
  if( mAnaData->sizeOfFcsTriggers()<=0 ){ LOG_WARN << "StFwdAnaDataMaker::Init() - Trigger Map is empty" << endm; }
  
  Int_t npoldata = mAnaData->ReadPolFile(mPolDataFilename.Data());
  //The 259 fills is special for my Run 22 text file after filtering out data from nonexistent fills
  if( npoldata!=259 ){ LOG_WARN << "Incorrect number of polarizations found in file 'Run22PolForJobs.txt' either because file is missing or file is improperly formatted" << endm; }

  if( mAnaData->mEpdGeom==0 ){ mAnaData->mEpdGeom = new StEpdGeom(); }
  else{ LOG_INFO << "StFwdAnaDataMaker::LoadDataFromFile() - StEpdGeom Exists!" << endm; }

  
  if( mAnaData->sizeOfFcsTriggers()<=0 ){ std::cout << "StFwdAnaDataMaker::LoadDataFromFile() - FCS Trigger Map is empty" << std::endl; }
  
  if( mHists==0 ){ mHists = new HistManager(); }
  //Histograms that need to be created by this class since can't move functionality
  UInt_t totalhists = mHists->AddH1D(file,mH1D_Entries,"H1D_Entries", "Number of entries", 1,-0.5,0.5 );
  totalhists += mHists->AddH1F(file,mH1F_RndmSpin,"H1F_RndmSpin","Histogram to know if using random spin or not",2,0,2);
  for( unsigned int i=0; i<mAnaList.size(); ++i ){
    totalhists += mAnaList.at(i)->LoadHists(file,mHists,mAnaData);
  }
  //std::cout << "TotalHistsLoaded: "<<totalhists << std::endl;
  //std::cout << "DUMB CHECK "<<mFcsTrigMap << std::endl;
  //std::string name(mFcsTrigMap->nameFromId(45,22349011));
  //std::cout << name << std::endl;
  
  return totalhists;
}

Int_t StFwdAnaDataMaker::Init()
{
  //std::cout << this->ClassName()<<"|Start Init:" << std::endl;
  TFile* savefile = 0;
  if( mFilename.Length() == 0){ mFilename="StMuFcsAna.root"; } //Ensure a TFile is always created
  if( mHists==0 ){
    LOG_INFO << "StFwdAnaDataMaker::Init() - No HistManager specified. Creating a new one with file name " << mFilename << ". Potential conflicts exist if a HistManager exists with same file name" << endm;
    mHists = new HistManager();
    mInternalHists = true;
    savefile = mHists->InitFile(mFilename.Data(),"RECREATE");//new TFile(mFilename.Data(), "RECREATE");
  }
  else{
    savefile = mHists->InitFile(); //No arguments just returns the internal file pointer
    mInternalHists = false;
  }
  
  if( mAnaData==0 ){
    LOG_ERROR << "StFwdAnaDataMaker::Init() - mAnaData is null" << endm;
    return kStFatal;
  }
  mAnaData->makeTree(savefile);

  mAnaData->readFcsTrigTxtFile(mFcsTrigFilename.Data());
  if( mAnaData->sizeOfFcsTriggers()<=0 ){ LOG_WARN << "StFwdAnaDataMaker::Init() - Trigger Map is empty" << endm; }
  
  Int_t npoldata = mAnaData->ReadPolFile(mPolDataFilename.Data());
  //The 259 fills is special for my Run 22 text file after filtering out data from nonexistent fills
  if( npoldata!=259 ){ LOG_WARN << "Incorrect number of polarizations found in file 'Run22PolForJobs.txt' either because file is missing or file is improperly formatted" << endm; }

  if( mAnaData->mEpdGeom==0 ){ mAnaData->mEpdGeom = new StEpdGeom(); }
  else{ LOG_INFO << "StFwdAnaDataMaker::Init - StEpdGeom Exists!" << endm; }
  
  UInt_t totalhists = mHists->AddH1D(0,mH1D_Entries,"H1D_Entries", "Number of entries", 1,-0.5,0.5 );
  totalhists += mHists->AddH1F(0,mH1F_RndmSpin,"H1F_RndmSpin","Histogram to know if using random spin or not",2,0,2);
  for( unsigned int i=0; i<mAnaList.size(); ++i ){
    totalhists += mAnaList.at(i)->LoadHists(0,mHists,mAnaData); //This is total of histograms loaded from a file not created. Don't use mFileOutput as you are not trying to load from #mFileOutput
  }
  mHists->SetOwner(kTRUE);
  LOG_INFO << "StFwdAnaDataMaker::Init() - Loaded " << totalhists << " histograms" << endm;

  return kStOk;
}

Int_t StFwdAnaDataMaker::InitRun(int runnumber)
{
  std::cout << this->ClassName()<<"|Start InitRun|runnum:"<<runnumber << std::endl;
  if( mAnaData->mFcsDb==0 ){
    mAnaData->mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
    //mFcsDb->setDbAccess(0);
    if(!(mAnaData->mFcsDb)){
      LOG_ERROR << "StFwdAnaDataMaker::InitRun - Failed to get StFcsDbMaker" << endm;
      return kStFatal;
    }
  }
  //std::cout << "End FcsDb:|fcsdb:"<<mAnaData->mFcsDb <<"|spindb:"<<mAnaData->mSpinDbMkr << std::endl;

  if( mAnaData->mSpinDbMkr==0 ){
    mAnaData->mSpinDbMkr = static_cast<StSpinDbMaker*>(GetMaker("spinDb"));
    if( mAnaData->mSpinDbMkr==0 ){
      LOG_WARN << "StFwdAnaDataMaker::InitRun - Could not find StSpinDbMaker named 'spinDb'" << endm;
    }
    else{
      if( !(mAnaData->mSpinDbMkr->isValid()) ){
	LOG_WARN << "StFwdAnaDataMaker::InitRun - Found StSpinDbMaker but contains invalid data so will not use it" << endm;
	mAnaData->mSpinDbMkr=0;
	mH1F_RndmSpin->SetBinContent(1,1);
      }
      else{
	mH1F_RndmSpin->SetBinContent(2,1);
      }
    }
  }
  //std::cout << "End Spin:|spindb:"<<mAnaData->mSpinDbMkr << std::endl;

  //std::cout << "End InitRun:|EpdQaMkr:"<<mAnaData->mEpdQaMkr << std::endl;
  return kStOK;
}

void StFwdAnaDataMaker::Clear(Option_t* option)
{
  mAnaData->resetEvent();
  return;
}

//----------------------
Int_t StFwdAnaDataMaker::Make()
{
  //std::cout << this->ClassName() << "|Start:Make_LoadEvent()" << std::endl;
  mAnaData->mMuDstMkr = (StMuDstMaker*)GetInputDS("MuDst");
  if( mAnaData->mMuDstMkr==0 ){ LOG_ERROR <<"StFwdAnaDataMaker::Make_LoadEvent - !MuDstMkr" <<endm; return kStErr; }
  mAnaData->mMuDst = mAnaData->mMuDstMkr->muDst();
  if( mAnaData->mMuDst==0 ){ LOG_ERROR << "StFwdAnaDataMaker::Make_LoadEvent - !MuDst" << endm; return kStErr; }
  mAnaData->mMuEvent = mAnaData->mMuDst->event();
  if( mAnaData->mMuEvent==0 ){ LOG_ERROR <<"StFwdAnaDataMaker::Make_LoadEvent - !MuEvent" <<endm; return kStErr; }
  mAnaData->mTrigData = mAnaData->mMuEvent->triggerData();
  //if( mAnaData->mTrigData==0 ){ LOG_ERROR <<"StFwdAnaDataMaker::Make_LoadEvent - !TrigData" <<endm; return kStErr; }
  mAnaData->mRunInfo = &(mAnaData->mMuEvent->runInfo());
  if( mAnaData->mRunInfo==0 ){ LOG_ERROR <<"StFwdAnaDataMaker::Make_LoadEvent - !RunInfo" <<endm; return kStErr; }

  mAnaData->mStEvent = (StEvent*)GetInputDS("StEvent");

  mAnaData->mEvent = mAnaData->mEvent+1;

  mH1D_Entries->Fill(0); //This is just counting valid make calls (i.e. increment bin 1 by 1)
  
  mAnaData->mEvtData->mRunTime         = mAnaData->mMuEvent->eventInfo().time();
  mAnaData->mEvtData->mRunNum          = mAnaData->mMuEvent->runNumber();
  mAnaData->mEvtData->mFill            = mAnaData->mRunInfo->beamFillNumber(StBeamDirection::east);    //using yellow beam
  mAnaData->mEvtData->mEvent           = mAnaData->mMuEvent->eventId();
  mAnaData->mEvtData->mBx48Id          = mAnaData->mTrigData->bunchId48Bit();
  mAnaData->mEvtData->mBx7Id           = mAnaData->mTrigData->bunchId7Bit();
  mAnaData->mEvtData->mTofMultiplicity = mAnaData->mTrigData->tofMultiplicity();
  //std::cout << "|runtime:"<<mEvtData->mRunTime << "|runnum:"<<mEvtData->mRunNum << "|event:"<<mEvtData->mEvent << std::endl;

  //Get EPD collection
  mAnaData->mMuEpdHits = 0;
  mAnaData->mEpdColl = 0;
  mAnaData->mMuEpdHits = mAnaData->mMuDst->epdHits();
  if( mAnaData->mMuEpdHits!=0 ){ if( mAnaData->mMuEpdHits->GetEntriesFast()==0 ){mAnaData->mMuEpdHits=0;} }//If mMuEpdHits is not zero but has no hits set it to zero so rest of code processes from StEpdHitMaker
  if( mAnaData->mMuEpdHits==0 ){ LOG_INFO << "StFwdAnaDataMaker::Make - No MuEPD hits" << endm;
    mAnaData->mEpdHitMkr = (StEpdHitMaker*)GetMaker("epdHit");
    if( (mAnaData->mEpdHitMkr)==0 ){ LOG_WARN << "StFwdAnaDataMaker::Make - No StEpdHitMaker(\"epdHit\")" << endm; mAnaData->mEpdColl=0; }
    else{ mAnaData->mEpdColl = mAnaData->mEpdHitMkr->GetEpdCollection(); }
    if( (mAnaData->mEpdColl)==0 ){ LOG_WARN << "StFwdAnaDataMaker::Make - No Epd hit information found" << endm; mAnaData->mEpdHitMkr=0; }//Set the hit maker back to zero so it can be used as a check that the epd collection doesn't exist
  }

  //Get Fcs collection
  mAnaData->mMuFcsColl = mAnaData->mMuDst->muFcsCollection();
  if( mAnaData->mMuFcsColl==0 ){ LOG_WARN << "StFwdAnaDataMaker::Make - No Fcs Collection" << endm; }

  //auto start = std::chrono::steady_clock::now();
  for( unsigned int i=0; i<mAnaList.size(); ++i ){
    Int_t result = mAnaList.at(i)->DoMake(mAnaData);
    switch( result ){
    case kStErr: return kStErr;
    case kStFatal: return kStFatal;
    case kStSkip: continue;
    case kStStop: return kStStop;
    }
  }
  //@[January 26, 2026] > [The right way to measure time in C++ is to use steady_clock](https://stackoverflow.com/questions/22387586/measuring-execution-time-of-a-function-in-c) which was a duplicate and redirected [to](https://stackoverflow.com/questions/11062804/measuring-the-runtime-of-a-c-code) 
  //auto end = std::chrono::steady_clock::now();
  //auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
  //std::cout << "|ievent:"<<mH1D_Entries->GetEntries() << "|time:"<<elapsed.count() << std::endl;

  mAnaData->fillTree();        //Checks if tree exists before filling
  
  return kStOk;
}

Int_t StFwdAnaDataMaker::Finish()
{
  //std::cout << this->ClassName()<<"|Start Finish:" << std::endl;
  TFile* savefile = mHists->InitFile(); //With no arguments just returns the file pointer
  if( savefile!=0 ){
    savefile->cd();
    mAnaData->writeTree();      //Checks if tree exists before writing
    mHists->Write();
  }
  return kStOK;
}

/*  
Int_t StFwdAnaDataMaker::LoadGraphsFromFile(TFile* file, TObjArray* graphs )
{
  Int_t gloaded = 0;
  gloaded += StMuFcsRun22QaMaker::MakeGraph(file,graphs,mGE_AllCuts_InvMass,"GE_AllCuts_InvMass","Mean of Invariant Mass (Err=RMS) vs. Run Index");
  gloaded += StMuFcsRun22QaMaker::MakeGraph(file,graphs,mGE_AllCuts_Pi0En,"GE_AllCuts_Pi0En","Mean Pi0 Energy (Err=RMS) vs. Run Index");
  return gloaded;
}


void StFwdAnaDataMaker::FillGraphs(Int_t irun)
{
  //TF1* pi0gausfit = new TF1("pi0gausfit","gaus(0)",0.1,0.2);
  //mH1F_InvMassAllCuts->Fit(pi0gausfit,"RQN");
  mGE_AllCuts_InvMass->SetPoint(irun,irun,((TH1*)mH1F_InvMassAllCuts->UncheckedAt(0))->GetMean());
  mGE_AllCuts_InvMass->SetPointError(irun,0,((TH1*)mH1F_InvMassAllCuts->UncheckedAt(0))->GetRMS());
  //delete pi0gausfit;
  //pi0gausfit = 0;

  mGE_AllCuts_Pi0En->SetPoint(irun,irun,((TH1*)mH1F_AllCuts_Pi0En->UncheckedAt(0))->GetMean());
  mGE_AllCuts_Pi0En->SetPointError(irun,0,((TH1*)mH1F_AllCuts_Pi0En->UncheckedAt(0))->GetRMS());

  return;
}

void StFwdAnaDataMaker::DrawQaGraphs(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->cd();
  canv->Divide(1,2);

  canv->cd(1);
  mGE_AllCuts_InvMass->Draw("AL");
  canv->cd(2);
  mGE_AllCuts_Pi0En->Draw("AL");
  
  canv->SaveAs(savename);
}

void StFwdAnaDataMaker::MergeForTssa( TH1* totalhistinc[][2], TH1* totalhistbg1[][2], TH1* totalhistbg2[][2], TH3* mergedinvmass, TH1* mergedpolblue, TH1* mergedpolyell, TH1* mergedpolblueerr, TH1* mergedpolyellerr )
{
  if( mH1F_RndmSpin->GetBinContent(1)>0.1 ){ std::cout << "  + RandomSpinFound" << std::endl; return; } //Don't merge histograms from files with random spin patterns
  for( int ibeam=0; ibeam<2; ++ibeam ){
    for( int ispin=0; ispin<2; ++ispin ){
      totalhistinc[ibeam][ispin]->Add(mH2F_NPi0Inc_xfVphi[ibeam][ispin]);
      totalhistbg1[ibeam][ispin]->Add(mH2F_NPi0Bg1_xfVphi[ibeam][ispin]);
      totalhistbg2[ibeam][ispin]->Add(mH2F_NPi0Bg2_xfVphi[ibeam][ispin]);
    }
  }
  mergedinvmass->Add(mH3F_AllCutsInvMass_xfVphi);
  
  mergedpolblue->Add(mH1D_BluePol);
  mergedpolblueerr->Add(mH1D_BluePolErr);
  mergedpolyell->Add(mH1D_YellowPol);
  mergedpolyellerr->Add(mH1D_YellowPolErr);
  //mergedpoldata->Add(mH1D_Entries);
  
}*/




