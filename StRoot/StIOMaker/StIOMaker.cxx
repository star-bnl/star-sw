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

enum { kStTREE=1,kStXDF=2,kStMDC2=3 };
const char  IOFMTS[] = "root xdf  mdc2";
const char *IOCLAS[] = {0,"StTreeMaker","St_xdfin_Maker","St_io_Maker"};
const char *IONAME[] = {0,"Root","XDF","MDC2"};

ClassImp(StIOMaker)

//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,  const char *iomode,
                     const char *ioFile,const char *treeName)
:StIOInterFace(name,iomode)
{
  fFile = ioFile;
  fFileSet = 0;
  if (ioFile) { //Make small StFile
    fFileSet = new StFile();
    fFileSet->AddFile(ioFile);
  }
  Build(fFileSet,treeName);
}
//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,   const char *iomode, 
                     StFile     *fileSet,const char *treeName )
:StIOInterFace(name,iomode)
{
  Build( fileSet,treeName);
}
//_____________________________________________________________________________
void StIOMaker::Build(StFile *fileSet,const char *treeName) 
{
  SetTreeName(treeName); fCase=0;
  SetMaxEvent();
  fCurrMk = 0;			//!Pointer to current maker
  fFmtMk[0]=0; fFmtMk[1]=0;fFmtMk[2]=0;
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
Int_t StIOMaker::Open()
{

  fNumEvent = 0;
  
  assert(fFileSet);
  int nBr = fFileSet->GetNBranches();

  for (int iBr=0; iBr<nBr; iBr++) { //branch loop
  
    const char *nextFile= fFileSet->NextFileName();
    if (!nextFile) return kStEOF;
    SetFile(nextFile);
    TString fmt = fFileSet->GetFormat();
    TString bra = fFileSet->GetBraName();

    const char *cc = strstr(IOFMTS,(const char*)fmt);
    if (!cc) return kStErr;
    fCase = (cc-IOFMTS)/5+1; 

    if (!fFmtMk[fCase-1]) fFmtMk[fCase-1] = Load();
    fCurrMk = fFmtMk[fCase-1];

    if (!fCurrMk) return kStErr;
    fCurrMk->SetBranch(bra,nextFile,0);
  
  }//end branch loop
  
  return fCurrMk->Open();
}
//_____________________________________________________________________________
Int_t StIOMaker::Make(){
  int iret;   
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

  return fCurrMk->Make();
}    
//_____________________________________________________________________________
Int_t StIOMaker::MakeWrite()
{
   return 1999;
}
//_____________________________________________________________________________
Int_t StIOMaker::Finish()
{ 
  for(int i=0;i<3;i++) if (fFmtMk[i]) fFmtMk[i]->Finish();
  return 0;
}
//_____________________________________________________________________________
void StIOMaker::Close(Option_t *)
{ 
  fCurrMk->Close();
}
//_____________________________________________________________________________
void StIOMaker::Clear(Option_t *opt)
{
  if(!fCurrMk) return;
  fCurrMk->Clear();
}
//_____________________________________________________________________________
void StIOMaker::PrintInfo()
{
  if (GetDebug()) printf("StIOMaker\n"); //  %s %s \n",GetName(), GetTitle());
}

//_____________________________________________________________________________
StIOInterFace *StIOMaker::Load()
{
  StIOInterFace  *Mk;
  TString ts;
  TClass *klass;

  const char *className = IOCLAS[fCase];
  klass = gROOT->GetClass(className);
  
  if (! klass ) {//lib not loaded
    if (fCase==kStXDF) gSystem->Load("xdf2root.so");
    ts = className; ts += ".so";
    gSystem->Load(ts);
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
  Mk->SetFile(fFile);
  if (GetDebug()) Mk->SetDebug();
  St_DataSet *brs = Find(".branches");
  if (brs) brs->Shunt(Mk);
  int iret = Mk->Init();
  return (iret) ? 0 : Mk;
}  
  

