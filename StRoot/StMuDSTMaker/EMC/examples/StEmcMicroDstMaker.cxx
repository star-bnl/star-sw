#include "StEmcMicroDstMaker.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StEmcMicroEvent.h"
#include "StIOMaker/StIOMaker.h"
#include "StEmcUtil/filters/StEmcFilter.h"
#include "StEmcMicroUtil.h"
#include "StChain/StEvtHddr.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StTree.h"

ClassImp(StEmcMicroDstMaker);
/*! Default constructor
*/
StEmcMicroDstMaker::StEmcMicroDstMaker(const Char_t *name) : StMaker(name)
{
 
  mGFilter   = new StEmcFilter();
  mPFilter   = new StEmcFilter();

  mPFilter->setEmcPresent(kTRUE);
  mPFilter->setPrintLog(kTRUE);
  mPFilter->setHaveVertex(kTRUE);
  mPFilter->setHavePrimaries(kTRUE);
  mPFilter->setZVertexCut(100);
  mPFilter->setMinMult(0);
  mPFilter->setMaxMult(500000);
    
  mGFilter->setPtCut(0.0);
  mGFilter->setFitPointsCut(0);
  mGFilter->setMustProjectEmc(kFALSE);
    
  mPFilter->setPtCut(0.0);
  mPFilter->setFitPointsCut(0);
  mPFilter->setDCACut(3);
  mPFilter->setEtaMin(-4.0);
  mPFilter->setEtaMax( 4.0);
  mPFilter->setMustProjectEmc(kFALSE);
  // end cuts
  
  mEventDir="./";
  mEventFileOld="";
  mEventFile="";    
  mMicroEventChain=NULL;
  mDoRead=kFALSE;
  mDoCreateStEvent=kTRUE;
  
  mDoSavePrimaries = kTRUE;
  mDoSaveGlobals = kTRUE;
  mDoSaveEmc = kTRUE;
  mDoSaveFpd = kTRUE;
  mDoSaveV0 = kTRUE;
	
	mOldMaker = NULL;
  
  mStart = 0;
 
}
StEmcMicroDstMaker::~StEmcMicroDstMaker() 
{
  if(mMicroUtil) delete mMicroUtil;
  if(mGFilter) delete mGFilter;
  if(mPFilter) delete mPFilter;
}
//------------------------------------------------------------------------
/*! Init method
*/
Int_t StEmcMicroDstMaker::Init()
{
  mAcc=new TH1F("macc","Events accepted and rejected",2,-0.5,1.5);
  mMicroUtil = new StEmcMicroUtil();
  mMicroUtil->setPrimaryFilter(mPFilter);
  mMicroUtil->setGlobalFilter(mGFilter);
  
  mMicroUtil->setSavePrimaries(mDoSavePrimaries);      
  mMicroUtil->setSaveGlobals(mDoSaveGlobals);        
  mMicroUtil->setSaveEmc(mDoSaveEmc);           
  mMicroUtil->setSaveFpd(mDoSaveFpd);    
  mMicroUtil->setSaveV0(mDoSaveV0);        
  
  mCurMicroEvent=0;
  mFileCounter = 0;
  mAccEv = 0;
  return StMaker::Init();
}
//------------------------------------------------------------------------
/*! Make method. Process each event
*/
Int_t StEmcMicroDstMaker::Make()
{
  cout <<"***** StEmcMicroDstMaker::Make() *******************************\n";
  if(mMicroEvent) delete mMicroEvent;
  mMicroEvent = 0;
  mStEvent = 0;
  
  if(!mDoRead)
  {
		mStEvent=NULL;
    mStEvent = (StEvent*) GetInputDS("StEvent");
    if(!mStEvent) return kStWarn;
    
    Float_t BF=0.5;
    StEventSummary* summary = mStEvent->summary();
    if(summary) BF = summary->magneticField()/10.;
    
    mPFilter->setBField(BF);
    mGFilter->setBField(BF);
  
    //event filter
    if (!mPFilter->accept(mStEvent)) 
    {
	    cout << "StEmcMicroDstMaker::Make(): event not accepted." << endl;
      mAcc->Fill(0);
	    return kStOK;
    }
    mAcc->Fill(1);
    mAccEv++; 
    
    // at this point, the event has been accepted.
    StIOMaker* IO=(StIOMaker*)GetMaker("IOMaker");
    if(IO)
      {
	const char* chp = strrchr(IO->GetFile(),'/');
	if(chp) mEventFile = strrchr(IO->GetFile(),'/')+1;
	else mEventFile = IO->GetFile();
      }
    // char *strrchr(const char *s, int c);
    // DESCRIPTION
    // The strrchr() function  returns  a  pointer  to  the  last
    // occurrence of the character c in the string s.
    else
    {
      //try to get from StTreeMaker
      StTreeMaker *tree=(StTreeMaker*)GetMaker("outputStream");
      if(tree)
      {
        StTree *t = tree->GetTree();;
        mEventFile=t->GetBaseName();
      }
      else
      {
        if(mAccEv%100==0) mFileCounter++;
        char f[100];
        sprintf(f,"EmcEvent.file%04d",mFileCounter);
        mEventFile = f;
      }
    }

    if(mOldMaker) mEventFile = mOldMaker->getCurrentFile();
    // if the input event file has changed, 
    // close the Micro dst file and creates another one
    if(mEventFile!=mEventFileOld)
    {
      if(mMicroDstFile) if(mMicroDstFile->IsOpen())
      {
        mMicroDstFile->Write(0,TObject::kOverwrite);
        mMicroDstFile->Close();
      }
      if (mMicroEvent)   delete mMicroEvent;
      if (mMicroDstFile) delete mMicroDstFile;
      mMicroEvent=NULL;
      mMicroDstFile=NULL;
      initMicroEventFile();
      mEventFileOld=mEventFile;
    }
    else cout <<"Filename being written: "<<mEventFile.Data()<<endl;
    mMicroEvent= mMicroUtil->getMicroEvent(mStEvent);
		if(mOldMaker)
		{
			// stuff that are not converted back in StEvent...
			StEmcMicroEvent* old=mOldMaker->getMicroEvent();
      mMicroEvent->setCTB(old->getCTB());
      mMicroEvent->setZDCe(old->getZDCe());
      mMicroEvent->setZDCw(old->getZDCw());
      mMicroEvent->setZVertexZDC(old->getZVertexZDC());
      mMicroEvent->setBBCe(old->getBBCe());
      mMicroEvent->setBBCw(old->getBBCw());
      mMicroEvent->setBBCNHits(old->getBBCNHits());
			mMicroEvent->setZVertexBBC(old->getZVertexBBC());
			mMicroEvent->setBunchCrossing7bit(old->getBunchCrossing7bit());
      mMicroEvent->setBunchCrossing(old->getBunchCrossing());
      mMicroEvent->setSpinBits(old->getSpinBits());
			StFpdMicroCollection *fpd = new StFpdMicroCollection();
			StFpdMicroCollection *oldfpd = old->getFpd();
			if(oldfpd)
			{
				fpd->setToken(oldfpd->getToken());
				fpd->setSumAdcNorth(oldfpd->getSumAdcNorth());
				fpd->setSumAdcSouth(oldfpd->getSumAdcSouth());
				fpd->setSumAdcTop(oldfpd->getSumAdcTop());
				fpd->setSumAdcBottom(oldfpd->getSumAdcBottom());
				fpd->setSumAdcPreShower1(oldfpd->getSumAdcPreShower1());
				fpd->setSumAdcPreShower2(oldfpd->getSumAdcPreShower2());
				fpd->setSumAdcSmdX(oldfpd->getSumAdcSmdX());
				fpd->setSumAdcSmdY(	oldfpd->getSumAdcSmdY());
				mMicroEvent->setFpd(fpd);
			}						
		}
		cout << "before Fill()" << endl;
    mEmcTree->Fill();
    cout << "after Fill()" << endl;
  }
  else //read Mode
  {  
    if(mMicroEventChain)
    {
			if(mNMicroEvents>0)
      {
        if(mCurMicroEvent<mNMicroEvents)
        {
          mMicroEvent = new StEmcMicroEvent();
          mStEvent = 0;
          mMicroEventChain->SetBranchAddress("MicroEvent",&mMicroEvent);
          mMicroEventChain->GetEntry(mStart+mCurMicroEvent++);

          UInt_t GMTTime =  (UInt_t)mMicroEvent->getEventTime();          
          StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
          if(!hd) { hd = new StEvtHddr();  AddData(hd); }
          hd->SetGMTime(GMTTime);
          mEventFile = strrchr(mMicroEventChain->GetFile()->GetName(),'/')+1;
  				if(mEventFile.EndsWith(".emcEvent.root"))
  				{
    				Int_t size=mEventFile.Sizeof();
    				mEventFile.Remove(size-15,14);
  				}
          if(mDoCreateStEvent)
          {
            mStEvent = mMicroUtil->getStEvent(mMicroEvent);
            AddData(mStEvent);
          }
        }
        else return kStWarn;
      }
      else return kStWarn;
    }
    else return kStWarn; 
  }

  return kStOK;
}
//-----------------------------------------------------------------------
/*! Finish method
*/
Int_t StEmcMicroDstMaker::Finish() 
{
  if(!mDoRead)
  {
    if (mMicroDstFile) 
      if(mMicroDstFile->IsOpen()) 
      {
        mMicroDstFile->Write(0,TObject::kOverwrite);
        mMicroDstFile->Close();
      }
  }
  return StMaker::Finish();
}
//----------------------------------------------------------------------------
/*! This method initializes the micro DST output file. It is called
every time the input file changes.
*/
Int_t StEmcMicroDstMaker::initMicroEventFile()
{
  Int_t split   = 99;      // by default split Event into sub branches
  Int_t comp    = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split) bufsize /= 16;
  // creat a Microevent and an output file
  mMicroEvent = new StEmcMicroEvent();   
  
  cout <<"Input file = "<<mEventFile.Data()<<endl;
  TString* filestring = new TString(mEventDir);
  filestring->Append(mEventFile);
  
  if(filestring->EndsWith(".event.root"))
  {
    Int_t size=filestring->Sizeof();
    filestring->Remove(size-12,11);
  }
  
  filestring->Append(".emcEvent.root");
  cout <<"Output file = "<<filestring->Data()<<endl;
  
  mMicroDstFile = new TFile(filestring->Data(),"RECREATE","EMC Micro DST file");
  if (!mMicroDstFile) 
  {
    cout << "##### EmcMicroEventMaker: Warning: no MicroEvents file = " << filestring->Data() << endl;
    return kStFatal;
  }
  mMicroDstFile->SetCompressionLevel(comp);
  mEmcTree = new TTree("EmcTree", "EMC Micro Tree");
  if (!mEmcTree) 
  {
    cout << "##### EmcMicroEventMaker: Warning: No EmcMicroTree" << endl;
    return kStFatal;
  }
  mEmcTree->SetAutoSave(1000000);  // autosave when 1 Mbyte written  
  mEmcTree->Branch("MicroEvent", "StEmcMicroEvent", &mMicroEvent,bufsize,split);
  
  delete filestring;  
  return kStOK;
}
//----------------------------------------------------------------------------
/*! This method adds one micro DST file to the file list.
*/
void StEmcMicroDstMaker::addMicroEventFile(char * file)
{
  if(!mMicroEventChain)
  {
    mMicroEventChain = new TChain("EmcTree","EMC Micro chain");
    //mMicroEvent = new StEmcMicroEvent();
    //mMicroEventChain->SetBranchAddress("MicroEvent",&mMicroEvent);
    //mMicroEventChain->GetBranch("MicroEvent")->SetAutoDelete(kTRUE);
  }
  mMicroEventChain->Add(file);
  mNMicroEvents=(Int_t)mMicroEventChain->GetEntries();
  cout <<"Total number of events in the chain = "<<mNMicroEvents<<endl;
  mDoRead=kTRUE;
}








