//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StIOMaker class for Makers                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "TClass.h"
#include "TROOT.h"
#include "StMaker.h"
#include "StIOMaker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "St_io_Maker/St_io_Maker.h"
#include "St_xdfin_Maker/St_xdfin_Maker.h"

enum { kStTREE=1,kStXDF=2,kStMDC2=3,kStDAQ=4 };
const char  IOFMTS[] = "root xdf  mdc2 daq ";
const char *IOCLAS[] = {0,"StTreeMaker","St_xdfin_Maker","St_io_Maker","StDAQMaker"};
const char *IONAME[] = {0,"Root","XDF","MDC2","DAQ"};

ClassImp(StIOMaker)

//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,  const char *iomode,
                     const char *ioFile,const char *treeName)
:StIOInterFace(name,iomode)
{
  fFileSet = 0;
  if (ioFile && ioFile[0]) { //Make small StFile
    fFileSet = new StFile();
    fFileSet->AddFile(ioFile);
  }
  Build(fFileSet,treeName);
}
//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,   const char *iomode, 
                     StFileI  *fileSet,const char *treeName )
:StIOInterFace(name,iomode)
{
  Build( fileSet,treeName);
}
//_____________________________________________________________________________
void StIOMaker::Build(StFileI *fileSet,const char *treeName) 
{
  SetTreeName(treeName); fCase=0;
  SetMaxEvent();
  fCurrMk = 0;			//!Pointer to current maker
  memset(fFmtMk,0,sizeof(fFmtMk));
  fFileSet= fileSet;
  
}
//_____________________________________________________________________________
StIOMaker::~StIOMaker()
{
  delete fFileSet;fFileSet= 0;
}

//_____________________________________________________________________________
Int_t StIOMaker::Init()
{
  return Open();
}
//_____________________________________________________________________________
 void  StIOMaker::SetFile(const char *file)
{
  if (!file || !file[0]) return;
  StIOInterFace::SetFile(file);
//		Add file to StFile
  if(!fFileSet) fFileSet = new StFile();
  fFileSet->AddFile(file);
}
//_____________________________________________________________________________
Int_t StIOMaker::Skip(int nskip)
{
  if (!fCurrMk) {
    Error("Skip","Ignored. File is NOT opened");
    return kStErr;
  }
  return fCurrMk->Skip(nskip);
}

//_____________________________________________________________________________
Int_t StIOMaker::Open()
{
  
  if (fCurrMk) return 0;
  fNumEvent = 0;
  if (!fFileSet) 			return kStEOF;

  if (fFileSet->GetNextBundle())	return kStEOF;

  fNextFile = fFileSet->GetFileName(0);
  if (!fNextFile) return kStEOF;
  if(GetDebug()) printf("<StIOMaker::Open() file %s\n",(const char*)fNextFile);
  TString fmt = fFileSet->GetFormat(0);
  TString bra = fFileSet->GetCompName(0);

  const char *cc = strstr(IOFMTS,(const char*)fmt);
  if (!cc) return kStErr;
  fCase = (cc-IOFMTS)/5+1; 

  if (!fFmtMk[fCase-1]) fFmtMk[fCase-1] = Load();
  fCurrMk = fFmtMk[fCase-1];

  if (!fCurrMk) return kStErr;
  fCurrMk->SetBranch(bra,fNextFile,0);
  StIOInterFace::SetFile(fNextFile);
  fCurrMk->SetFile(fNextFile);
  
  return fCurrMk->Open();
}
//_____________________________________________________________________________
Int_t StIOMaker::Make(){
  int iret;   
  fNIO++;
  if (fIOMode[0]=='r')  { //Read mode
AGAIN:
    iret = MakeRead();  
    SetNumber(++fNumEvent);
    if (fNumEvent > fMaxEvent) iret = kStEOF;
    if (iret != kStEOF) 	return iret;
    Close();   	
    iret = Open(); 	if (iret)		return iret;
    goto AGAIN;

  } else 		{ //Write mode
    return MakeWrite();
  }
}
//_____________________________________________________________________________
Int_t StIOMaker::MakeRead(){

  if (!fCurrMk) Open();
  if (!fCurrMk) return kStEOF;
  StUKey uk  = fFileSet->GetNextEvent();
  if (uk.EOK())	return kStEOF;

  return fCurrMk->MakeRead(uk);
}    
//_____________________________________________________________________________
Int_t StIOMaker::MakeWrite()
{
   return 1999;
}
//_____________________________________________________________________________
Int_t StIOMaker::Finish()
{ 
  int n = sizeof(fFmtMk)/sizeof(fFmtMk[0]);
  for(int i=0;i<n;i++) {
    if (!fFmtMk[i]) continue;
    fFmtMk[i]->Finish();
    fFmtMk[i]=0;}
  fCurrMk = 0;
  StIOInterFace::Finish();
  return 0;
}
//_____________________________________________________________________________
void StIOMaker::Close(Option_t *)
{ 
  if (fCurrMk) fCurrMk->Close();
  fCurrMk = 0;
}
//_____________________________________________________________________________
void StIOMaker::Clear(Option_t *opt)
{
  if(!fCurrMk) return;
  fCurrMk->Clear();
}
//_____________________________________________________________________________

//_____________________________________________________________________________
StIOInterFace *StIOMaker::Load()
{
  StIOInterFace  *Mk;
  TString ts;
  TClass *klass;

  const char *className = IOCLAS[fCase];
  klass = gROOT->GetClass(className);
  
  if (! klass ) {//lib not loaded
    if (fCase==kStXDF) gSystem->Load("xdf2root");
    if (fCase==kStDAQ) gSystem->Load("StDaqLib");
    gSystem->Load(className);
    klass = gROOT->GetClass(className);
  }
  assert (klass);
  
  StMaker *saveMK = cd();
  Mk = (StIOInterFace*)klass->New();    
  assert(Mk);
  saveMK->cd();
  
  ts = GetName();
  ts +="_";
  ts += IONAME[fCase]; 

  Mk->SetName(ts);
  Mk->SetIOMode(fIOMode);
  Mk->SetTreeName(fTreeName);
  Mk->SetFile(fNextFile);
  if (GetDebug()) Mk->SetDebug();
  TDataSet *brs = Find(".branches");
  if (brs) brs->Shunt(Mk);
  int iret = Mk->Init();
  return (iret) ? 0 : Mk;
}  
  

