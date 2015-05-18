#include <stdio.h>
#include "Stypes.h"
#include "StTree.h"
#include "TRegexp.h"
#include "TDirIter.h"
#include "TKey.h"
#include "TError.h"
#include <stdlib.h>

#ifdef __RFIO__
#include "TRFIOFile.h"
#endif

void DummyErrorHandlerFunc(int , Bool_t , const char *, const char *){;}




const char* TFOPT[9] = {"0","READ","RECREATE","UPDATE",
                        "0","READ","RECREATE","UPDATE",0};
const char  RWU[] = "0rwu0rnu0rcu";
char IOMODE[] = "0";

Int_t StIO::fgDebug = 0;

static inline Int_t IntOMode(char ciomode)
{
char *c=(char *)strchr(RWU,tolower(ciomode));
return (c) ? (c-RWU)&3 : 0;
}

#ifdef __RFIO__
extern "C" {
   int rfio_open(const char *filepath, int flags, int mode);
   int rfio_close(int s);
   int rfio_read(int s, char *ptr, int size);
   int rfio_write(int s, char *ptr, int size);
   int rfio_lseek(int s, int offset, int how);
   int rfio_access(const char *filepath, int mode);
   int rfio_unlink(const char *filepath);
   int rfio_parse(const char *name, char **host, char **path);
};
#endif

class Cat : public TNamed {
public:
  Cat(){fNGeant=0;fNKeys=0;fNRecs=0;fSize=0;fNFiles=0;};
  double fSize;
  int    fNGeant;
  int    fNKeys;
  int    fNRecs;
  int    fNFiles;
};

ClassImp(StIOEvent)
StIOEvent::StIOEvent():TObject(){fObj=(TObject*)(-1);};
//______________________________________________________________________________
void StIOEvent::Browse(TBrowser *b)
{
  if (b && fObj && (Long_t)fObj != (-1)) fObj->Browse(b);
}
//______________________________________________________________________________
void StIOEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class StIOEvent.

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      TObject::Streamer(R__b);
      R__b >> fObj;
   } else {
      R__b.WriteVersion(StIOEvent::IsA());
      TObject::Streamer(R__b);
      R__b << fObj;
   }
}

//_______________________________________________________________________________

Int_t StIO::Write(TFile *file, const StUKey &ukey, TObject  *obj)
{
  assert(file);
  if (!obj) return kStWarn;

  StIOEvent event;
  event.fObj = obj; event.SetUniqueID(ukey.GetSum());
  //  TFile *bakfile = gFile; 
  TDirectory *bakdir = gDirectory; file->cd();

  event.Write(ukey.GetKey(),TObject::kOverwrite|TObject::kSingleKey);
  if (gFile) gFile->Flush();
  //  gFile=bakfile; 
  gDirectory=bakdir;
  return 0;
}
//_______________________________________________________________________________
TObject *StIO::Read(TFile *file, const char *name)
{
  TObject *toread=0;
  TKey *key = 0;
  assert(file);

  TFile *bakfile = gFile; TDirectory *bakdir = gDirectory; file->cd();

  if (!gFile) { printf("<StIO::Read> No file open \n"); goto RETURN;}

  if (name[0]=='*') {
       key = (TKey*)gDirectory->GetListOfKeys()->At(0);
       if (strcmp(".StreamerList",key->GetName())==0)
       key = (TKey*)gDirectory->GetListOfKeys()->At(1);
  }
  else key = (TKey*)gDirectory->GetListOfKeys()->FindObject(name);

  if (!key) {
    if(fgDebug) printf("<StIO::Read> Key %s not found\n",name);
    goto RETURN; }

  if (name[0]!='*' && strcmp(key->GetClassName(),"StIOEvent"))
             { printf("<StIO::Read> Key %s has wrong className %s\n",
               key->GetName(),key->GetClassName());
               toread = (TObject*)(-1); goto RETURN; }

  toread = key->ReadObj(); if (!toread) toread = (TObject*)(-1);

RETURN: 
  //  gFile=bakfile; 
  gDirectory=bakdir; return toread;
}
//_______________________________________________________________________________
TObject *StIO::Read(TFile *file, const StUKey &ukey)
{
  StIOEvent *event = 0;
  TObject   *retn  = 0;
  retn = Read(file,ukey.GetKey());
  if (!retn)            return 0;
  if (retn == (TObject*)(-1)) return retn;
  if (retn->IsA()!=StIOEvent::Class()) return retn;
  event = (StIOEvent*)retn;
  retn = event->fObj;
  assert( !retn || retn==(TObject*)(-1) || event->GetUniqueID()==ukey.GetSum());
  delete event;
  return retn;
}
//_______________________________________________________________________________
Int_t StIO::GetNextKey(TFile *file, StUKey &ukey,ULong_t &handle)
{
  TObjLink *lnk; TList *lk; const char *kname; TObject *obj;
  enum { kSorted = 1};
  assert(file);

  TString tk = ukey.GetKey(); ukey = kUMAX;
  const char* prevkey = (const char*)tk;
  int lname = strlen(ukey.GetName())+1;

  lk = file->GetListOfKeys();  if(!lk) return 1;
  if (!lk->TestBit(kSorted)) {lk->Sort(); lk->SetBit(kSorted);handle = 0;}

   // Find an object in this list using its name. Requires a sequential
   // scan till the object has been found. Returns 0 if object with specified
   // name is not found. This method overrides the generic FindObject()
   // of TCollection for efficiency reasons.

  if (handle) {
     lnk = (TObjLink*)handle;
     handle = 0;
     obj = lnk->GetObject();
     if (strcmp(prevkey,obj->GetName())>0) lnk = lk->FirstLink();
  } else {
     lnk = lk->FirstLink();
  }

  while (lnk) {
    obj = lnk->GetObject();
    kname = obj->GetName();
    if (strcmp(prevkey, kname)<0) break;
    lnk = lnk->Next();
  }
  if (!lnk) return 1;
  if (strncmp(prevkey,kname,lname)) lnk = 0;
  if (!lnk) return 1;
  ukey.SetKey(kname);
  handle = (ULong_t)lnk;
  return 0;
}
//_______________________________________________________________________________
TObject *StIO::ReadNext(TFile *file, StUKey &ukey, ULong_t &handle)
{
  if(GetNextKey(file,ukey,handle)) return 0;;
  return Read(file,ukey);
}
//_______________________________________________________________________________
TString StIO::RFIOName(const char *name)
{
  TString file(name);
  TNamed *tn;
  TList *rfiomap = (TList *)gROOT->GetListOfSpecials()->FindObject(".rfiomap");
  if (file.Contains(".daq")) rfiomap = 0;       //no RFIO for .daq
  if (rfiomap) {        // check the map
    TIter next(rfiomap);
    while ((tn = (TNamed*)next())) {//matching loop
      int n = strlen(tn->GetName());
      if (!n)                                   continue;
      if (strncmp(file,tn->GetName(),n))        continue;
      file.Replace(0,0,tn->GetTitle());
      break;
    }   //end matching loop
  }  //end check the map
  return file;
}
//_______________________________________________________________________________
TFile *StIO::Open(const char *name, Option_t *option,const char *title,Int_t compress)
{
  //printf("DEBUG:: StIO::Open(%s)\n",name);

  TString file = RFIOName(name);
  if(strcmp("READ",option)==0 && !IfExi(name)) return 0;
  if(strcmp("read",option)==0 && !IfExi(name)) return 0;
  if(strcmp("Read",option)==0 && !IfExi(name)) return 0;
  TFile *tf = TFile::Open(file,option,title,compress);
  if (!tf  || !tf->IsZombie()) return tf;
  delete tf;
  return 0;
}
//_______________________________________________________________________________
Int_t StIO::IfExi(const char *name)
{
  TString file = (name);

  //printf("DEBUG:: StIO::IfExi Expanding(%s)\n",name);
  gSystem->ExpandPathName(file); 

  // Supress error message
  ErrorHandlerFunc_t dummy = &DummyErrorHandlerFunc;
  ErrorHandlerFunc_t curre = SetErrorHandler(dummy);

  //printf("DEBUG:: StIO::IfExi AccessingPathName(%s)\n",file.Data());
  // JL - This attempts to do a root auth even if xroot
#if 0  
  if (!gSystem->AccessPathName(file)){ 
    SetErrorHandler(curre); 
    return 1;

  } else 
#endif  
  {
    int l = file.Length();
    if (file(l-5,5)!=".root"){
      SetErrorHandler(curre); 
      return !gSystem->AccessPathName(file);
    } else {
      //printf("DEBUG:: Opening %s\n",name);
      // Workaround:
      //  Due the ROOT (4.04.02) TSystem::FindHelper bug one can not 
      //  use TSystem::AccessPathName method to test files on 
      //  the "rootd" servers
      TFile *tf = TFile::Open(file);
      // return old error handler
      SetErrorHandler(curre);
      //printf("DEBUG:: Done with %s\n",name);

      int z = (!tf || tf->IsZombie());
      delete tf;   
      return !z;
    }
  }
}
//===============================================================================

ClassImp(StBranch)

//_______________________________________________________________________________
StBranch::StBranch(const char *name, StTree *parent,Option_t *opt):TDataSet(name,parent)
{

  SetTitle(".StBranch");
  fNEvents=0;fUKey=name;fUKey=0;fIOMode=0;fTFile=0;fDebug=0;fHandle=0;
  SetOption(opt);

}
StBranch::~StBranch()
{
 Close();

}
//_______________________________________________________________________________
void StBranch::SetOption(Option_t *opt)
{
  if (opt) fOption = opt;
  fOption.ToUpper();
}
//_______________________________________________________________________________
void StBranch::SetIOMode(Option_t *iomode)
{

if (!iomode || !iomode[0]) return;
fIOMode = ::IntOMode(iomode[0]);
fHandle=0;
}
//_______________________________________________________________________________
Option_t *StBranch::GetIOMode()
{
IOMODE[0]=0; if (fIOMode>0) IOMODE[0] = RWU[int(fIOMode)]; return IOMODE;
}

//_______________________________________________________________________________
Int_t StBranch::SetFile(const char *file,const char *mode,int insist)
{
  fHandle=0;
  fIOMode = abs(fIOMode);
  if (fTFile && !insist) { Error("SetFile","File is already opened");return kStWarn;}
  if (file && file[0]) fFile=file;
  if (mode && mode[0]) SetIOMode(mode);
  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::UpdateFile(const char *file)
{
fHandle=0;
fIOMode = abs(fIOMode);
TString bas[2],dir[2];
TString nam(GetName()); nam.ReplaceAll("Branch","");
nam.Insert(0,"."); nam += ".root";
TString outFile = file; gSystem->ExpandPathName(outFile);
        dir[0] = gSystem->DirName (outFile);
        bas[0] = gSystem->BaseName(outFile);

if (strncmp(".none ",GetFile(),6)==0) {//FileName was corrupted by old bug
  
  TString ts(GetFile()); ts.Remove(0,6); SetFile(ts);
}

  dir[1]  = gSystem->DirName (GetFile());
  bas[1]  = gSystem->BaseName(GetFile());
//VP  SetIOMode("0");
  if (bas[0] == bas[1] || bas[0].Contains(nam)) SetIOMode("r");
  //VP?? else                                  	bas[0]=bas[1];
  for (int d=0; d<2; d++) {
    for (int b=0; b<2; b++) {
      if (!bas[b].Contains(nam)) continue;
      char *newFile = gSystem->ConcatFileName(dir[d],bas[b]);
      if (StIO::IfExi(newFile)) {        
        fFile = newFile;
        printf("<StBranch::UpdateFile> Branch=%s file %s\n",GetName(),newFile);
        delete [] newFile;
        return 0;
  } } }
  fIOMode = -abs(fIOMode);
  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::SetTFile(TFile *tfile)
{
  if (!tfile)           return 0;
  if (fTFile==tfile)    return 0;
  fHandle=0;
  if (fTFile) Close();  
  fTFile=0;
  SetFile(tfile->GetName());
  Open();
  return 0;
}

//_______________________________________________________________________________
void StBranch::Close(const char *)
{
  fHandle=0;
  if (!fIOMode) return;
  if (!fTFile)  return;
  if (strncmp(".none",GetFile(),4)==0) return;
  TFile *tf = fTFile;
  fTFile = 0;
  TString ts(tf->GetTitle());
  int idx = ts.Index("StBranch=Y");
  assert (idx>=0);
  ts.Replace(idx+9,1,"");
  tf->SetTitle(ts);
  if (ts.Contains("StBranch=Y")) return;
  tf->Close("");
  printf("** <StBranch::Close> Branch=%s \tFile=%s \tClosed **\n"
        ,GetName(),(const char*)fFile);
  delete tf;
  if (!(fIOMode&2)) return;
  ts = GetFile();
  ts.Replace(0,0,".none ");
  SetFile((const char*)ts);
}
//_______________________________________________________________________________
const char *StBranch::GetFile()
{
  TString dir(fFile);
  int kase=0;
  if (!fFile.IsNull()) kase = 2;
  if (kase && fFile[fFile.Length()-1]=='/') kase = 1;

  if (kase<2) { // Construct file name
    fFile=GetName(); fFile.ReplaceAll("Branch",""); fFile+=".root";
    StTree *tree = (StTree*)GetParent();
    if (tree) { // include base name
      const char* base = tree->GetBaseName();
      if (base) {fFile.Insert(0,"."); fFile.Insert(0,base);}}
    if (kase==1) fFile.Insert(0,dir); 
  }

   return (const char*)fFile;
}


//_______________________________________________________________________________
Int_t StBranch::Open()
{
  if (fIOMode<=0) return 0;
  if (fTFile)   return 0;
  if (strncmp(".none",GetFile(),4)==0) return 1;
  OpenTFile();
  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::WriteEvent(const StUKey &ukey)
{
  int iret;
  if (  fIOMode<=0)   return 0;
  if (!(fIOMode&2))   return 0;
  fUKey.Update(ukey,GetName());
  if (!fList)           return kStWarn; //empty
  if (!fList->First())  return kStWarn; //empty
  if(Open()) return 1;
  fNEvents++;

  TList savList;
  SetParAll(0,this,&savList);
  if (IsOption("const"))        {//Write constant branch (event independent)
    StUKey uk(fUKey); uk = (UInt_t)(-2);
    iret = StIO::Write(fTFile,uk,   fList);
    fIOMode = -fIOMode;
  } else {
    iret = StIO::Write(fTFile,fUKey,fList);
  }
  SetParAll(&savList); 

  return 0;
}

//_______________________________________________________________________________
Int_t StBranch::GetEvent(Int_t mode)
{
  TObject *obj=0;
  if (  fIOMode<=0)     return 0;
  if (!(fIOMode&1))     return 0;
  Delete(); if (fList)  delete fList; fList=0;
  if(Open()) return 1;

  if (IsOption("const"))        {//Read constant branch (event independent)
    StUKey uk(fUKey); uk = (UInt_t)(-2);
    obj = StIO::Read (fTFile,uk);  fIOMode = -fIOMode;

  } else if (mode)                {//Read next
    obj = StIO::ReadNext(fTFile,fUKey,fHandle);

  } else                        {//Read this one
    obj = StIO::Read    (fTFile,fUKey);
  }

  if (obj == 0)                 return kStEOF;
  if (obj == (TObject*)(-1))    return kStErr;
  fList = (TList*)obj;

  SetParAll(this,this,0);

  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::ReadEvent(const StUKey &ukey)
{
  if (  fIOMode<=0)     return 0;
  if (!(fIOMode&1))     return 0;
  fUKey.Update(ukey,GetName());
  return GetEvent(0);
}
//_______________________________________________________________________________
Int_t StBranch::NextEvent()
{
  if (  fIOMode<=0)     return 0;
  if (!(fIOMode&1))     return 0;
  Clear();
  return GetEvent(1);
}

//_______________________________________________________________________________
Int_t StBranch::NextEvent (StUKey &ukey)
{
  if (  fIOMode<=0)     return 0;
  if (!(fIOMode&1))     return 0;
  Clear();
  fUKey.Update(ukey,GetName());
  int iret = GetEvent(1); 
  ukey.Update(fUKey); 
  return iret;
}
//_______________________________________________________________________________
void StBranch::SetParAll(TDataSet *parNew,TDataSet *parOld,TList *savList)
{
  TDataSetIter next(parOld);
  TDataSet *son,*p;
  while ((son=next())) {
    p = son->GetParent();
    son->SetParent(parNew);
    if (savList) {
       assert(p);
       savList->AddFirst(son); savList->AddFirst(p); 
       SetParAll(son,son,savList);
    }
  }// end while
}
//_______________________________________________________________________________
void StBranch::SetParAll(TList *savList)
{
  assert(savList);
  TDataSet *son,*par;
  while ((par=(TDataSet*)savList->First())) {
    savList->Remove(par);
    son = (TDataSet*)savList->First(); assert(son);
    savList->Remove(son);
    son->SetParent(par);
  }// end while
}
//_______________________________________________________________________________
void StBranch::OpenTFile()
{
  if (fTFile) return;
  fHandle = 0;
  TFile *tf= gROOT->GetFile(GetFile());
  fTFile = tf;
  if (!fTFile) {
    fTFile = StIO::Open(GetFile(),TFOPT[int(fIOMode)],GetName());

  }
  if (!fTFile) {
    Error("OpenTFile","File %s NOT OPENED ***\n",GetFile());
    Error("OpenTFile","Branch %s desactivated ***\n",GetName());
    delete fTFile; fTFile=0; SetIOMode("0");
  } else {
    TString ts(fTFile->GetTitle());
    if (ts.Contains("StBranch="))
    {  ts.ReplaceAll("StBranch=","StBranch=Y");}
    else
    {  ts += " StBranch=Y";}
    fTFile->SetTitle(ts);
    if (tf) return;

    printf("** <StBranch::Open> Branch=%s \tMode=%s \tFile=%s \tOpened **\n"
        ,GetName(),TFOPT[int(fIOMode)],(const char*)fFile);

  }
}
//_______________________________________________________________________________
void StBranch::Clear(Option_t *)
{
  Delete();
}
//===============================================================================

ClassImp(StTree)

//_______________________________________________________________________________
StTree::StTree(const char *name):StBranch(name)
{
  SetTitle(".StTree");
}

//_______________________________________________________________________________
StTree::~StTree()
{ Close(); Delete();}

//_______________________________________________________________________________
void StTree::SetIOMode(Option_t *iomode)
{
 StBranch::SetIOMode(iomode);
 TDataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) { br->SetIOMode(iomode);}
}
//_______________________________________________________________________________
Int_t StTree::SetFile(const char *file,const char *mode,int /* insist */)
{
  if (mode && *mode) StBranch::SetIOMode(mode); 
  if (!file || !*file) 	return 0;

  if (fIOMode&1)  {	//ReadMode
    UpdateFile(file);
    
  }
  if (fIOMode&2)  {	//WriteMode
  
    SetBaseName(file);
  }
  return 0;
}

//_______________________________________________________________________________
void StTree::Clear(Option_t*)
{
 TDataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) {/*if(!br->IsOption("const"))*/ br->Clear();}
}

//_______________________________________________________________________________
Int_t StTree::Open()
{
  TDataSetIter nextA(this);StBranch *brA;
  while ((brA=(StBranch*)nextA())) {
    if (brA->GetIOMode()[0]=='0')       continue;
    if (brA->GetTFile())                continue;
    brA->Open();
  }
  return 0;
}
//_______________________________________________________________________________
Int_t StTree::UpdateFile(const char *file)
{
  TDataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) br->UpdateFile(file);
  return 0;
}

//_______________________________________________________________________________
Int_t StTree::WriteEvent(const StUKey &ukey)
{
  fUKey.Update(ukey,GetName()); Open();
  TDataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) br->WriteEvent(fUKey);
  fNEvents++;
  return 0;
}

//_______________________________________________________________________________
Int_t StTree::ReadEvent(const StUKey &ukey)
{
  Int_t iret=0,num=0;
  fUKey.Update(ukey,GetName()); Open();
  TDataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) {      //Read all branches
    if ( (br->fIOMode<0)) continue;
    if (!(br->fIOMode&1)) continue;
    iret=br->ReadEvent(fUKey);
    if (!iret) num++;
  if(iret==kStErr) return kStErr;
  }
  return (num)? 0:kStEOF;
}

//_______________________________________________________________________________
Int_t StTree::NextKey()
{
  if (Open()) return kStEOF;
  TDataSetIter next(this); StBranch *br;
  StUKey minKey = kUMAX;
  StUKey tryKey;


  while ((br=(StBranch*)next())) {
    if ( (br->fIOMode<0)) 				continue;
    if (!(br->fIOMode&1)) 				continue;
    if (br->IsOption("const"))				continue;
    tryKey.Update(fUKey,br->GetName());
    if (StIO::GetNextKey(br->fTFile,tryKey,br->fHandle))continue;
    tryKey = "";
    if (tryKey.Compare(minKey)<0) minKey=tryKey;
  }
  fUKey = minKey;
  return minKey.EOK() ? kStEOF:0;
}
//_____________________________________________________________________________
Int_t StTree::Skip(int nskip)
{  
  for (; nskip; nskip--) 
  {
    int ret = NextKey(); 
    if (ret) break;
  }

  return nskip;
}
//_______________________________________________________________________________
Int_t StTree::NextEvent(StUKey  &ukey)
{
  fUKey.Update(ukey,GetName());int iret=NextEvent(); ukey.Update(fUKey); return iret;
}
//_______________________________________________________________________________
Int_t StTree::NextEvent()
{
  int iret=0,nAkt=0;
  TDataSetIter next(this); StBranch *br=0;

  int kase = 0; StBranch *br0=0;
  while ((br=(StBranch*)next())) {
    switch(kase) {
    case 0: kase=1; br0 = br;
     	if ( (br->fIOMode<0)) 	return kStEOF;
     	if (!(br->fIOMode&1)) 	return kStEOF;
	iret = br->NextEvent();
        if (iret)		return iret;
	break;
    case 1:
     	if ( (br->fIOMode<0)) 	continue;
     	if (!(br->fIOMode&1)) 	continue;
	iret = br->ReadEvent(br0->GetUKey());
    }
    nAkt++;
  }
  
  return (nAkt) ? 0:kStEOF;
}

//_______________________________________________________________________________
void StTree::Close(const char* opt)
{
  Clear();
  fUKey = 2000;
  TDataSetIter next(this); StBranch *br;
  while ((br=(StBranch*)next())) { //branch loop
    br->fIOMode = abs(br->fIOMode);
    if (br->fIOMode&2) {
      br->Open();
      TFile *tfbr = br->GetTFile(); if(!tfbr)   continue;
      if (tfbr->IsWritable()) {
        TDataSet *par = GetParent(); SetParent(0);
        StIO::Write(tfbr,fUKey,this);
        SetParent(par);}
    }
    if ((opt && strcmp(opt,"keep")==0)) continue;
    br->Close();
  }// end branch loop
}

//_______________________________________________________________________________
StTree *StTree::GetTree(TFile *file, const char *treeName)
{
  StUKey treeKey(treeName,2000); 
  StTree *ret = (StTree*)StIO::Read(file,treeKey);
  if ((Long_t)ret == -1) return 0;
  TDataSetIter next(ret);StBranch *br=0;
  while ((br=(StBranch*)next())) {
     br->SetIOMode("0");
     br->fUKey = br->GetName();
     br->fUKey = 0;
  }
  return ret;
}

//_______________________________________________________________________________
void StTree::SetBaseName(const char *baseName,const char *dirname)
{
  const char *dot;
  fBaseName = gSystem->BaseName(baseName);
  if (dirname && *dirname) {
    TString ts(dirname); 
    if (ts[ts.Length()-1]!='/') ts += "/";
    ts += fBaseName;
    fBaseName = ts;
  }
  dot = strrchr(fBaseName.Data(),'.');
  if (!dot) 				return;
  int fz = (strstr(".fz .fzd .daq",dot)!=0);
  fBaseName.Remove(dot-fBaseName.Data());
  if (fz) 				return;
  dot = strrchr(fBaseName.Data(),'.');
  if (!dot) 				return;
  if ('0' <=dot[1] && dot[1]<='9') 	return;
  fBaseName.Remove(dot-fBaseName.Data());
}

//_____________________________________________________________________________
ClassImp(StFile)
 StFile::StFile(const char** fileList):StFileI("StFile")
{
  fDS = new TDataSet();
  fIter = -1; fKeyIter = 0;
  SetTitle(" nbranches=1 ");
  if (fileList) AddFile(fileList);
}
//_____________________________________________________________________________
 StFile::~StFile()
{
  delete fDS;   	fDS      = 0;
  fIter    = -1;
  delete fKeyIter;	fKeyIter = 0;
}
//_____________________________________________________________________________
Int_t StFile::GetNextBundle()
{
  if (!fDS)	return 1;
  if (fIter>-1 && !fDS->At(fIter)) return 1;
  return (!fDS->At(++fIter));
  
}
//_____________________________________________________________________________
Int_t StFile::GetNBundles()
{
  return fDS->GetListSize();
}
//_____________________________________________________________________________
Int_t StFile::GetNFiles()
{
  return fDS->GetListSize();
}
//_____________________________________________________________________________
TDataSet *StFile::GetFileDS(Int_t idx)
{
  TDataSet *fam = fDS->At(fIter);
  if (!fam) return 0;
  TDataSet *df = fam->At(idx);
  if (!df) return 0;
  SetInfo(df);
  return df;
}
//_____________________________________________________________________________
const char *StFile::GetFileName(Int_t idx)
{
  if (idx == -1) { idx=0; if (GetNextBundle()) return 0;}
  TDataSet *df = GetFileDS(idx);
  if (!df) return 0;
  return strstr(df->GetTitle(),"file=")+5;
}
//_____________________________________________________________________________
const char *StFile::GetFormat(Int_t idx)
{
  TDataSet *df = GetFileDS(idx);
  return GetAttr(df,"format=");
}
//_____________________________________________________________________________
const char *StFile::GetCompName(Int_t idx)
{
  TDataSet *df = GetFileDS(idx);
  return GetAttr(df,"branch=");
}
//_____________________________________________________________________________
Int_t StFile::AddFile(const char **fileList)
{
  const char *file;
  if (!fileList) return 0;
  for(int i=0; (file = fileList[i]);i++){
    if (file) AddFile(file);
  }
  return 0;
}
//_____________________________________________________________________________
Int_t StFile::AddFile(const char *file,const char *opt)
{

  //printf("DEBUG:: StFile::AddFile\n");

  TString tfile,tit,base,famy;
  if (strpbrk(file,"*\\[]#@")) return AddWild(file,opt);
  int remove = 0;
  if (opt && *opt) {
    TString ts(opt);
    if (ts.Contains("REMOVE" ,TString::kIgnoreCase)) remove=1;   
    if (ts.Contains("EXCLUDE",TString::kIgnoreCase)) remove=1;   
  }	  
	  
  //printf("DEBUG:: StFile::AddFile - ExpandPathName\n");	    
  tfile = file; gSystem->ExpandPathName(tfile);

  //printf("DEBUG:: StFile::AddFile - testing IfExi\n");	    

  if (!StIO::IfExi(tfile)) {// file does not exist
    Warning("AddFile","*** IGNORED *** File %s does NOT exist \n",
    (const char*)tfile);
    return kStWarn;}

  const char* cc = strrchr(tfile,'.');
  if (!cc || !strstr(".root .daq .dat",cc)){// No extention
    Warning("AddFile","*** IGNORED *** File %s has wrong extention \n",
    (const char *)tfile);
    return kStWarn;}

  base = gSystem->BaseName(tfile);

  famy = base;
  int dot = famy.Last('.');
  if (dot>0) famy.Replace(dot,999,"");
  dot = famy.Last('.');
  if (dot>0) famy.Replace(dot+1,999,"");

  TDataSet *dsfam = fDS->Find(famy);
  if (!dsfam && remove) return 0;
  if (!dsfam) fDS->Add((dsfam = new TObjectSet(famy,0)));

  TDataSet *twice = dsfam->Find(base);
  tit = tfile; tit.Replace(0,0," file=");
  if (twice) {
    if (!remove) {
       // compare the full path
       // if (tit == twice->GetTitle() )
          Warning("AddFile","File \"%s\" is already added from \"%s\",\n\tThe file \"%s\" is accepted anyway. \n"
                ,(const char *)base, twice->GetTitle(), (const char *)tit );
    } else       { delete twice; return 0;}
  }
  //  TDataSet *dss = dsfam->First();
  //if (dss)
  //  Warning("AddFile","Files %s and %s are from the same family \n",
  //  (const char *)tfile,dss->GetName());


  TDataSet *ds = new TDataSet(base,dsfam);
  ds->SetTitle(tit);


  if (GetDebug()) printf("<%s::AddFile> Added file %s %s\n",
                  ClassName(),ds->GetName(),ds->GetTitle());

  return 0;
}
//_____________________________________________________________________________
Int_t StFile::AddWild(const char *file,const char *opt )
{
  const char* fullname;
  TDirIter dirIter(file);
  while((fullname=dirIter.NextFile())) { AddFile(fullname,opt);}
  
  return 0;
}
//_____________________________________________________________________________
Int_t StFile::AddEvent(UInt_t r,UInt_t e)
{
  TObjectSet *dsfam = (TObjectSet*)fDS->Last();	if(!dsfam) return 1;
  TDataSet   *dsfil = dsfam->Last();		if(!dsfil) return 1;
    
  TDataSet   *dskey = (TDataSet*)dsfam->GetObject(); 
  if (!dskey) { dskey = new TDataSet("uklist");dsfam->SetObject(dskey,1);} 
  char cbuf[40];
  sprintf(cbuf,".%010u.%010u",r,e);
  new TDataSet(cbuf,dskey);
  return 0;
}  
//_____________________________________________________________________________
StUKey StFile::GetNextEvent()
{
  StUKey uk(kUMAX);
  if (!fDS) 		return uk;
  TDataSet *dsfam = fDS->At(fIter); 
  if (!dsfam) 		return uk;
  uk = 0;
  TDataSet *dskeys = (TDataSet*)dsfam->GetObject();
  if (!dskeys)		return uk;
  if (!fKeyIter) fKeyIter = new TDataSetIter(dskeys);
  TDataSet *dskey = fKeyIter->Next();
  if (!dskey){ uk = kUMAX; delete fKeyIter; fKeyIter=0;	return uk;}
  uk.SetKey(dskey->GetName());
  return uk;
}
//_____________________________________________________________________________
void StFile::SetInfo(TDataSet *ds)
{
  int i;
  TFile *tf=0;

  if (!ds) return;
  TString tit(ds->GetTitle());
  Int_t known = 0;
  if (strstr(tit,"format=")) known += 1;
  if (strstr(tit,"branch=")) known += 2;
  if (known==3) return;

  const char *fname = strstr(ds->GetTitle(),"file=")+5;
  const char *ext =   strrchr(fname,'.');
  assert(ext);

  tit.Replace(0,0," branch=NONE ");

//              DAQ
  if (strcmp(".daq",ext)==0) {
    tit.Replace(0,0," format=daq ");
    known = 3;
    goto RETN;
  }

//              DAT
  if (strcmp(".dat",ext)==0) {
    tit.Replace(0,0," format=dat ");
    known = 3;
    goto RETN;
  }

//              ROOT
  if (strcmp(".root",ext)==0) {
    int lfname = strlen(fname);
    if (strcmp(".MuDst.root",fname+lfname-11)==0){
        tit.Replace(0,0," format=mudst ");
    } else {
        tit.Replace(0,0," format=root " );
    }
    known |= 1;
//              ... now try to know branch name
    TString bran = fname;
    bran.ReplaceAll(".root","");
    i = bran.Last('.');
    if (i>0) {
      bran.Replace(0,i+1,"");
      known = 3;
      i = tit.Index("branch=NONE"); assert(i>=0);
      tit.Replace(i+7,4,bran);
      goto RETN;
    }


//              ... branch name still unknown

    tf = TFile::Open(fname,"READ");
    assert(tf && !tf->IsZombie());
    TList *kl = gFile->GetListOfKeys();
    TIter nextKey(kl);
    TKey *ky;
    while ((ky = (TKey*)nextKey())) {
      if (strcmp("StIOEvent",ky->GetClassName()))       continue;
      const char *bra=ky->GetName();
      if (strstr(bra,"tree"))                           continue;
      if (strstr(bra,"Tree"))                           continue;
      if (strstr(bra,".")==0)				continue;
      i = tit.Index("branch=NONE"); assert(i>=0);
      tit.Replace(i+7,4,bra,strcspn(bra,"."));
      break;
  } }

RETN:;
  delete tf;
  ds->SetTitle(tit);
}
//_____________________________________________________________________________
void StFile::ls(Option_t *opt)
{
   if (!fDS) return;
   if (opt && *opt ) {lsFull(opt); return;}
   TDataSetIter famIter(fDS);
   TDataSet *fam,*fil;
   int num = 0;
   while((fam=famIter.Next())) {
     if (!fam->First()) continue;
     TDataSetIter filIter(fam);
     while((fil=filIter.Next())) {
       const char *fname = strstr(fil->GetTitle(),"file=");
       if (!fname) continue;
       num++;
       printf("%3d - %s\n",num,fname+5);
     }
   }
}
//_____________________________________________________________________________
void StFile::lsFull(Option_t *opt)
{
  TList blist;
  int ibr=0,i;
  Cat *cat;
  int savIter = fIter; fIter=-1;
  int numFile=0;
  TString oldirname("_");
  while (!GetNextBundle()) {    //Bundle loop
    for (int idx=0; idx<999; idx++) {   //File loop
      const char *fil = GetFileName(idx);
      if (!fil)         break;
      numFile++;
      TString dirname = gSystem->DirName(fil);
      TString basname = gSystem->BaseName(fil);
      TString br(basname);
      i = br.Last('.'); if (i<0) continue;
      br.Replace(i,999,"");
      i = br.Last('.');
      if (i<0) { br = "undef";} else { br.Replace(0,i+1,"");};

      if (oldirname != dirname) {
        printf("\nDirName  =%s\n\n",dirname.Data());
        oldirname.Replace(0,999,dirname);}

      cat = (Cat*)(blist.FindObject(br));
      if (!cat) { cat = new Cat(); cat->SetName(br); blist.Add(cat);}
      cat->fNFiles++;

      TFile *tf = 0;
      if (opt && opt[0]=='r')	tf = StIO::Open(fil,"update");
      if (!tf) 			tf = StIO::Open(fil,"read"  );
      if (!tf) 			continue;

      TList *keys = tf->GetListOfKeys();
      int nkeys=0,nreks=0;
      double dsize=0;

      TIter nextk(keys);
      TKey *key;
      while ((key=(TKey*)nextk())) {    //Key loop
        TString keyname(key->GetName());
        i = keyname.First('.');
        if (i>0) keyname.Replace(i,9999,"");
        keyname.ReplaceAll("Branch","");
        cat = (Cat*)(blist.FindObject(keyname));
        if (!cat) {
          cat = new Cat();
          cat->SetName(keyname);
          blist.Add(cat);
        }
        nkeys++;
        cat->fNKeys++;
        if (keyname==br) { nreks++;  cat->fNRecs++;};
        dsize +=      key->GetObjlen();
        cat->fSize += key->GetObjlen();
        TString clasname(key->GetClassName());
        cat = (Cat*)(blist.FindObject(clasname));
        if (!cat) {
          cat = new Cat();
          cat->SetName(clasname);
          blist.Add(cat);
        }
        cat->fNKeys++;
        cat->fSize += key->GetObjlen();

      }// End key loop
      dsize/=1000000.;
      printf("%4d BR=%-7s NK=%-4d NR=%-4d SZ=%8.2fM File= %s\n",
             numFile,br.Data(),nkeys,nreks,dsize,basname.Data());
      delete tf;

//  printf("%4d %s\n",numFile,fil);
    } //*end File loop*/
  }//*end Bundle loop*/

  printf("\n\n  In Total ==================================================\n");
  blist.Sort();
  TIter nextBr(&blist);
  ibr = 0;
  while ( (cat = (Cat*)nextBr())){
    ibr++;
    printf("%4d BR=%-10s NK=%-4d NR=%-4d SZ=%8.2fM NFiles %4d\n",
             ibr,
             cat->GetName(),
             cat->fNKeys,
             cat->fNRecs,
             cat->fSize*1.e-6,
             cat->fNFiles);
  }
  blist.Delete();
  fIter = savIter;
}
//_____________________________________________________________________________
const char *StFile::GetAttr(TDataSet *ds,const char *att)
{
  static TString brName;
  if (!ds) return 0;
  SetInfo(ds);
  const char *bn = strstr(ds->GetTitle(),att);
  if (!bn) return 0;
  bn += strlen(att);
  int n = strcspn(bn," ");
  brName.Replace(0,999,bn,n);
  return (const char*)brName;
}
