//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StIOMaker class for Makers                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "TClass.h"
#include "TROOT.h"
#include "StMaker.h"
#include "StIOMaker.h"
#include "StTreeMaker/StTreeMaker.h"
//#include "St_io_Maker/St_io_Maker.h"
#include "St_xdfin_Maker/St_xdfin_Maker.h"

enum { kStTREE=1,kStXDF=2,kStMDC2=3,kStDAQ=4, kStMuDst=5 };
const char  IOFMTS[] = "root  xdf   mdc2  daq   mudst ";
const char *IOCLAS[] = {0,"StTreeMaker","St_xdfin_Maker","St_io_Maker","StDAQMaker","StMuIOMaker"};
const char *IONAME[] = {0,"Root","XDF","MDC2","DAQ","MuDst"};

ClassImp(StIOMaker)

//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,  const char *iomode,
                     const char *ioFile,const char *treeName)
:StIOInterFace(name,iomode)
{
    Build(fFileSet,ioFile,treeName);
}
//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,   const char *iomode, 
                     StFileI  *fileSet,const char *treeName )
:StIOInterFace(name,iomode)
{
  Build( fileSet,0,treeName);
}
//_____________________________________________________________________________
void StIOMaker::Build(StFileI *fileSet,const char *ioFile,const char *treeName) 
{
  fCurrMk = 0;			//!Pointer to current maker
  fSkip=0; fFileSet=0; fCase = 0;
  SetMaxEvent();
  memset(fFmtMk,0,sizeof(fFmtMk));
  SetTreeName(treeName); fCase=0;

  if (fIOMode[0]=='r') {	//ReadOnly
    fFileSet= fileSet;
    if (ioFile && ioFile[0]) {  //Make small StFile
      fFileSet = new StFile();
      fFileSet->AddFile(ioFile);
      AddConst(new TObjectSet("FileSet",fFileSet,1)); 	//To be deleted at the end
    }  
  } else {			//Write/Update
    fCase = 1;
    fNextFile = ioFile;
  }         
}
//_____________________________________________________________________________
StIOMaker::~StIOMaker()
{
  fFileSet= 0;
}
//_____________________________________________________________________________
void StIOMaker::Rewind()
{
  Close();
  if (fFileSet) fFileSet->Rewind();
}

//_____________________________________________________________________________
Int_t StIOMaker::Init()
{
//VP  return Open();
  return 0;
}
//_____________________________________________________________________________
 void  StIOMaker::SetFile(const char *file)
{
  if (!file || !file[0]) return;
  StIOInterFace::SetFile(file);
  fNextFile = file;
  if (fIOMode[0]!='r') return;

//		Add file to StFile
  if(!fFileSet) fFileSet = new StFile();
  fFileSet->AddFile(file);
}
//_____________________________________________________________________________
Int_t StIOMaker::Skip()
{
  
  if (!fSkip) 	return 0;
  if (!fCurrMk) OpenRead();
  if (!fCurrMk) return 0;
  
  fSkip = fCurrMk->Skip(fSkip);
  if (!fSkip) return 0;
  Close();
  return Skip();
}

//_____________________________________________________________________________
Int_t StIOMaker::Open(const char *)
{
  if (fIOMode[0]==0) return 0;
  if (fIOMode[0]=='r') { Skip(); return OpenRead();}
  return OpenWrite();
}
//_____________________________________________________________________________
Int_t StIOMaker::OpenRead()
{
  
  if (fCurrMk) return 0;
  fNumEvent = 0;
  if (!fFileSet) 			return kStEOF;

  if (fFileSet->GetNextBundle())	return kStEOF;

  fNextFile = fFileSet->GetFileName(0);
  if (fNextFile.IsNull()) return kStEOF;
  if(GetDebug()) printf("<StIOMaker::Open() file %s\n",(const char*)fNextFile);
  TString fmt = fFileSet->GetFormat(0);
  TString bra = fFileSet->GetCompName(0);

  const char *cc = strstr(IOFMTS,(const char*)fmt);
  if (!cc) return kStErr;
  fCase = (cc-IOFMTS)/6+1; 

  if (!fFmtMk[fCase-1]) fFmtMk[fCase-1] = Load();
  fCurrMk = fFmtMk[fCase-1];

  if (!fCurrMk) return kStErr;
  fCurrMk->SetBranch(bra,fNextFile.Data(),0);
  StIOInterFace::SetFile(fNextFile.Data());
  fCurrMk->SetFile(fNextFile.Data());
  NotifyEm("OpenFile",fNextFile.Data());
  return fCurrMk->Open();
}
//_____________________________________________________________________________
Int_t StIOMaker::OpenWrite()
{
  
  if (fCurrMk) return 0;
  fNumEvent = 0;
  if (fNextFile.IsNull()) return kStEOF;
  if(GetDebug()) printf("<StIOMaker::Open() file %s\n",(const char*)fNextFile);
  fCase = 1; 

  if (!fFmtMk[fCase-1]) fFmtMk[fCase-1] = Load();
  fCurrMk = fFmtMk[fCase-1];
  if (!fCurrMk) return kStErr;
  fCurrMk->Close();
  fCurrMk->SetFile(fNextFile.Data());
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

  return MakeRead(uk);
}    
//_____________________________________________________________________________
Int_t StIOMaker::MakeRead(const StUKey &uk){

  if (!fCurrMk) Open();
  if (!fCurrMk) return kStEOF;
  return fCurrMk->MakeRead(uk);
}    
//_____________________________________________________________________________
Int_t StIOMaker::MakeWrite()
{
  if (!fCurrMk) Open();
  if (!fCurrMk) return kStEOF;
  return fCurrMk->MakeWrite();
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
  if (!fCurrMk) return;
  fCurrMk->Close();
  NotifyEm("CloseFile",fFile.Data());
  fCurrMk = 0;
}
//_____________________________________________________________________________
void StIOMaker::Clear(Option_t *opt)
{
  if(!fCurrMk) return;
  fCurrMk->Clear();
}
//_____________________________________________________________________________
void  StIOMaker::NotifyMe(const char *about,const void *info)
{
   if (strcmp("CloseFile",about)==0) { Close(); 		return;}
   if (strcmp("OpenFile" ,about)==0) { fNextFile =((char*)info);return;}
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
    if (fCase==kStXDF)   gSystem->Load("xdf2root");
    if (fCase==kStDAQ)   gSystem->Load("StDaqLib");
    if (fCase==kStMuDst) gSystem->Load("StMuDSTMaker");
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
  Mk->SetDebug(GetDebug());
  TDataSet *brs = Find(".branches");
  if (brs) brs->Shunt(Mk);
  int iret = Mk->Init();
  return (iret) ? 0 : Mk;
}  
  

