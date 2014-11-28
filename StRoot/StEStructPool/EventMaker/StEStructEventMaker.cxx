/**********************************************************************
 *
 * $Id: StEStructEventMaker.cxx,v 1.2 2006/04/10 23:43:10 porter Exp $
 *
 * Author: Jeff Porter rework of Jeff Reid's code
 *
 **********************************************************************
 *
 * Description:  This is an maker designed to read a STAR dst and
 *               summarize it into an EStructEvent which contains only
 *               the information necessary for EbyE analysis.
 *
 **********************************************************************/

#include "StEStructEventMaker.h"
#include "StChain.h"
#include "StMessMgr.h"


ClassImp(StEStructEventMaker)

StEStructEventMaker::StEStructEventMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {

  mEStructEvent=0;
  mfileList=0;
  mfileCount=0;
  moutFile=0;
  mChain=0;
  mEStructEventFile=0;
  mEStructTree=0;
  
}

StEStructEventMaker::~StEStructEventMaker() { }

//-----------------------------------------------------------------------
StEStructEvent* StEStructEventMaker::event(){  
  return new StEStructEvent(*mEStructEvent);  
}

//-----------------------------------------------------------------------
Int_t StEStructEventMaker::Make() { 

if(!readEvent())return kStEOF; 
return kStOK; 
};

//-----------------------------------------------------------------------
bool StEStructEventMaker::readEvent(){

  if(!openRead()) return false;
  if(meventCount >= mChain->GetEntries() || mChain->GetEntry(meventCount++)==0)  return false;
  mEStructEvent->FillChargeCollections();

  return true;
};

//-----------------------------------------------------------------------
bool StEStructEventMaker::openRead(){

 
  if(!mfileList) return false;
  if(mChain) return true;

  mEStructEvent=new StEStructEvent();

  mChain=new TChain("EStructDst");
  mChain->SetDirectory(0);
  
  ifstream from(mfileList);
  char line[512];
  bool done=false;

  mfileCount=meventCount=0;
  while(!done){
    if(from.eof()){
      done=true;
    } else {
      from.getline(line,512);
      if(line[0]=='#')continue;
      if(!strstr(line,".estruct.root")) continue;
      mChain->Add(line);
      mfileCount++;
    }
  }
  mChain->SetBranchAddress("EStructEvent",&mEStructEvent);

  return true;
}
  
//-----------------------------------------------------------------------
bool StEStructEventMaker::openWrite() {
 

  if(mEStructEventFile) return true;
  if(!moutFile) return false;
  TBranch * branch;

  mEStructEventFile=new TFile(moutFile,"RECREATE","StEStructDst");
  mEStructTree = new TTree("EStructDst","StEStructDst",99);
  mEStructTree->SetAutoSave(1000000);  // autosave when 1 Mbyte written
  branch = mEStructTree->Branch("EStructEvent","StEStructEvent",&mEStructEvent,65526, 99);
  if(!branch){
    cout<<" EStructBranch not created !!! "<<endl;
    return false;
  }

  return true;
}

//-----------------------------------------------------------------------
bool StEStructEventMaker::writeEvent(StEStructEvent* e){
  if(!openWrite() || !e) return false;
  mEStructEventFile->cd();
  if(mEStructEvent) delete mEStructEvent;
  mEStructEvent=e;
  mEStructTree->Fill();
  return true;
}

//-----------------------------------------------------------------------
void StEStructEventMaker::setInputFileList(const char* f){return setName(f,0);};

//-----------------------------------------------------------------------
void StEStructEventMaker::setOutputFile(const char* f){return setName(f,1); };


//-----------------------------------------------------------------------
void StEStructEventMaker::setName(const char* name, int opt){

  if(!name) return;

  char* tmp=new char[strlen(name)+1];
  strcpy(tmp,name);

  switch(opt){
  case 0:
    {
      mfileList=tmp;
      break;
    }
  case 1:
    {
      moutFile=tmp;
      break;
    }
  default:
    {
      delete [] tmp;
      break;
    }
  }
}

//-----------------------------------------------------------------------
Int_t StEStructEventMaker::Init() {
  return kStOK;
}

//-----------------------------------------------------------------------
void StEStructEventMaker::Clear(Option_t *opt) {
  StMaker::Clear();
}

//-----------------------------------------------------------------------
Int_t StEStructEventMaker::Finish() {
  if(!mEStructEventFile) return kStOk;
  mEStructEventFile->cd();
  mEStructTree->Write();
  mEStructEventFile->Close();
  return kStOk;
}


/***********************************************************************
 *
 * $Log: StEStructEventMaker.cxx,v $
 * Revision 1.2  2006/04/10 23:43:10  porter
 * Protected our pico-dst writer from multiple calls to "Finish()"
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
