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
//#include "St_xdfin_Maker/St_xdfin_Maker.h"

enum { kStTREE=1,
       kStXDF=2,
       kStMDC2=3,
       kStDAQ=4,
       kStMuDst=5,
       kStDAT=6 
};
const char  IOFMTS[] = "root  xdf   mdc2  daq   mudst dat   ";
const char *IOCLAS[] = {0
			,"StTreeMaker"
			,"St_xdfin_Maker"
			,"St_io_Maker"
			,"StDAQMaker"
			,"StMuIOMaker"
			,"StDAQMaker"
//         ,"StTrgDatFileReader"
};
const char *IONAME[] = {0,
			"Root",
			"XDF",
			"MDC2",
			"DAQ",
			"MuDst",
         "DAT"
};

ClassImp(StIOMaker)

//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,  const char *iomode,
                     const char *ioFile,const char *treeName)
:StIOInterFace(name,iomode),fFileSet(0),fCurrMk(0)
{
    Build(0,ioFile,treeName);
}
//_____________________________________________________________________________
StIOMaker::StIOMaker(const char *name,   const char *iomode, 
                     StFileI  *fileSet,const char *treeName )
:StIOInterFace(name,iomode),fFileSet(0),fCurrMk(0)
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
//VP      AddConst(new TObjectSet("..FileSet",fFileSet,1)); 	//To be deleted at the end
    }  
  } else {			//Write/Update
    fCase = 1;
    fNextFile = ioFile;
  }         
}
//_____________________________________________________________________________
StIOMaker::~StIOMaker()
{
  // pointer may belong to someone else - do not delete
  if (fFileSet) delete fFileSet; fFileSet= 0;
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
  if(!fFileSet) {
    fFileSet = new StFile();
//    AddConst(new TObjectSet("..FileSet",fFileSet,1)); 	//To be deleted at the end
  }
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
  SetNumber((fNumEvent+=fSkip));
  Close();

  return fSkip;
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
  if(GetDebug()) (void) printf("<StIOMaker::Open() file %s\n",(const char*)fNextFile);
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
  if(GetDebug()) (void) printf("<StIOMaker::Open() file %s\n",(const char*)fNextFile);
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

    LOG_QA << "StIOMaker:  Event: " << GetIventNumber()
        << "  Run: " << GetRunNumber() 
        << "  EventId: " << GetEventNumber() <<   endm;


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
  delete fFileSet; fFileSet=0;
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
  (void) printf("<StIOMaker::Load() trying to GetClass(%s) case %d\n",className,fCase);
  klass = gROOT->GetClass(className);
  
  if (! klass || klass->Size()==0) {        // lib not loaded
    Int_t Loaded=0;      // library load stack may be self-sufficient, set to 1
//  if (fCase==kStXDF)   gSystem->Load("xdf2root");
    if (fCase==kStDAQ || fCase==kStDAT)
    {
       gSystem->Load("RTS");
       gSystem->Load("StDaqLib");
    }
    if (fCase==kStDAT) {
       gSystem->Load("StTrgDatFileReader");
    }
    if (fCase==kStMuDst){
      Loaded = 1;
      gSystem->Load("St_Tables");
      gSystem->Load("StEmcUtil");
      gSystem->Load("StStrangeMuDstMaker");
      gSystem->Load("StMuDSTMaker");
    }
    if (! Loaded) gSystem->Load(className);
    klass = gROOT->GetClass(className);
  }
  if ( ! klass ){
    (void) printf("<StIOMaker::Load() className %s was not loaded\n",
		  className);
    assert (klass);
  }
  
  ts = GetName(); ts +="_"; ts += IONAME[fCase]; 
  Mk = (StIOInterFace*)GetMaker(ts.Data());
  if (Mk){
     assert(Mk->InheritsFrom(StIOInterFace::Class()));
  } else {  
    Mk = (StIOInterFace*)New(className,ts.Data()); assert(Mk);
  }  
  Mk->SetAttr(this);
  AddMaker(Mk);
  Mk->SetIOMode(fIOMode);
  Mk->SetTreeName(fTreeName);
  Mk->SetFile(fNextFile);
  Mk->SetDebug(GetDebug());
  TDataSet *brs = Find(".branches");
  if (brs) brs->Shunt(Mk);
  int iret = Mk->Init();
  return (iret) ? 0 : Mk;
}  
  

