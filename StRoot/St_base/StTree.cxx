#include <stdio.h>
#include "Stypes.h"
#include "StTree.h"
#include "TRegexp.h"
#include "TKey.h"
#include "TRFIOFile.h"

const char* TFOPT[9] = {"0","READ","RECREATE","UPDATE",
                        "0","READ","RECREATE","UPDATE",0};
const char  RWU[] = "0rwu0rnu0rcu";
char IOMODE[] = "0";

static inline Int_t IntOMode(char ciomode)
{
char *c=(char *)strchr(RWU,tolower(ciomode));
return (c) ? (c-RWU)&3 : 0;
}
//              Local functions
static TString GetBranchByFile(const Char_t *file);
static int AreSimilar(const Char_t *fileA, const Char_t *fileB);

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
  TFile *bakfile = gFile; TDirectory *bakdir = gDirectory; file->cd();

  event.Write(ukey.GetKey(),TObject::kOverwrite|TObject::kSingleKey);
  gFile->Flush();
  gFile=bakfile; gDirectory=bakdir;
  return 0;
}
//_______________________________________________________________________________
TObject *StIO::Read(TFile *file, const Char_t *name)
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

  if (!key)  { printf("<StIO::Read> Key %s not found\n",name); goto RETURN; }

  if (name[0]!='*' && strcmp(key->GetClassName(),"StIOEvent"))
             { printf("<StIO::Read> Key %s has wrong className %s\n",
               key->GetName(),key->GetClassName());
               toread = (TObject*)(-1); goto RETURN; }

  toread = key->ReadObj(); if (!toread) toread = (TObject*)(-1);

RETURN: gFile=bakfile; gDirectory=bakdir; return toread;
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
Int_t StIO::GetNextKey(TFile *file, StUKey &ukey)
{
  TObjLink *lnk; TList *lk; const char *kname; TObject *obj;

  assert(file);

  TString tk = ukey.GetKey(); ukey = kUMAX;
  const char* prevkey = (const char*)tk;
  int lname = strlen(ukey.GetName())+1;

  lk = file->GetListOfKeys();  if(!lk) return 1;

   // Find an object in this list using its name. Requires a sequential
   // scan till the object has been found. Returns 0 if object with specified
   // name is not found. This method overrides the generic FindObject()
   // of TCollection for efficiency reasons.

  lnk = lk->FirstLink();
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
  return 0;
}
//_______________________________________________________________________________
TObject *StIO::ReadNext(TFile *file, StUKey &ukey)
{
  if(GetNextKey(file,ukey)) return 0;;
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
  TString file = RFIOName(name);
  TFile *tf = TFile::Open(file,option,title,compress);
  if (!tf  || !tf->IsZombie()) return tf;
  delete tf;
  return 0;
}
//_______________________________________________________________________________
Int_t StIO::IfExi(const char *name)
{
  TString file = RFIOName(name);

  if (strncmp(file,"rfio:",5))
     return !gSystem->AccessPathName(file);
  else
     return !::rfio_access((const char*)file, kFileExists);

}
//===============================================================================

ClassImp(StBranch)

//_______________________________________________________________________________
StBranch::StBranch(const Char_t *name, StTree *parent,Option_t *opt):TDataSet(name,parent)
{

  SetTitle(".StBranch");
  fNEvents=0;fUKey=name;fUKey=0;fIOMode=0;fTFile=0;fDebug=0;
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
}
//_______________________________________________________________________________
Option_t *StBranch::GetIOMode()
{
IOMODE[0]=0; if (fIOMode>0) IOMODE[0] = RWU[fIOMode]; return IOMODE;
}

//_______________________________________________________________________________
Int_t StBranch::SetFile(const Char_t *file,const Char_t *mode,int insist)
{
  fIOMode = abs(fIOMode);
  if (fTFile && !insist) { Error("SetFile","File is already opened");return kStWarn;}
  if (file && file[0]) fFile=file;
  if (mode && mode[0]) SetIOMode(mode);
  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::UpdateFile(const Char_t *file)
{
fIOMode = abs(fIOMode);
TString outFile = file; gSystem->ExpandPathName(outFile);
TString outDir  = gSystem->DirName (outFile);
TString outBas  = gSystem->BaseName(outFile);

if (strncmp(".none",GetFile(),4)==0) {//FileName was corrupted by old bug
  TString ts("/.nowhere/");     ts += outBas;
  ts.ReplaceAll(".root","");    ts.Replace(ts.Last('.')+1,999,"");
  ts += GetName();              ts.ReplaceAll("Branch","");
  ts +=".root";                 SetFile(ts);
}

TString intDir  = gSystem->DirName (GetFile());
TString intBas  = gSystem->BaseName(GetFile());
Char_t * newFile = gSystem->ConcatFileName(outDir,intBas);
SetIOMode("0");
if (intBas == outBas) SetIOMode("r");
if (intBas == outBas || outBas.IsNull())        goto RETN00;
if (!StIO::IfExi(newFile))                      goto RETN99;
RETN00: fFile = newFile;
printf("<StBranch::UpdateFile> Branch=%s file %s\n",GetName(),newFile);
RETN99: delete [] newFile;
return 0;
}
//_______________________________________________________________________________
Int_t StBranch::SetTFile(TFile *tfile)
{
  if (!tfile)           return 0;
  if (fTFile==tfile)    return 0;
  if (fTFile) Close();
  fTFile=0;
  SetFile(tfile->GetName());
  Open();
  return 0;
}

//_______________________________________________________________________________
void StBranch::Close(const char *)
{
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
  if (fFile.IsNull()) { // Construct file name
    fFile=GetName(); fFile.ReplaceAll("Branch",""); fFile+=".root";
    StTree *tree = (StTree*)GetParent();
    if (tree) { // include base name
      const char* base = tree->GetBaseName();
      if (base) {fFile.Insert(0,"."); fFile.Insert(0,base);}}}

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
  if (fIOMode<=0)       return 0;
  if (!(fIOMode & 2))   return 0;
  fUKey.Update(ukey,GetName());
  if (!fList)           return kStWarn; //empty
  if (!fList->First())  return kStWarn; //empty
  if(Open()) return 1;
  fNEvents++;

  TList *savList = new TList;
  SetParAll(0,savList);
  iret= StIO::Write(fTFile,fUKey,fList);
  SetParAll(savList); delete savList;
  return 0;
}

//_______________________________________________________________________________
Int_t StBranch::GetEvent(Int_t mode)
{
  TObject *obj=0;
  if (fIOMode<=0)       return 0;
  if (!(fIOMode&1))     return 0;
  Delete(); if (fList)  delete fList; fList=0;
  if(Open()) return 1;

  if (IsOption("const"))        {//Read constant branch (event independent)
    StUKey uk(fUKey); uk = (UInt_t)(-2);
    obj = StIO::Read (fTFile,uk);  fIOMode = -fIOMode;
    if (!obj)                   return kStErr;}

  else if (mode)                {//Read next
    obj = StIO::ReadNext(fTFile,fUKey);

  } else                        {//Read this one
    obj = StIO::Read    (fTFile,fUKey);
  }

  if (obj == 0)                 return kStEOF;
  if (obj == (TObject*)(-1))    return kStErr;
  fList = (TList*)obj;

  SetParAll(this,0);

  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::ReadEvent(const StUKey &ukey)
{
  if (fIOMode<=0)       return 0;
  if (!(fIOMode&1))     return 0;
  fUKey.Update(ukey,GetName());
  return GetEvent(0);
}
//_______________________________________________________________________________
Int_t StBranch::NextEvent()
{
  if (fIOMode<=0)       return 0;
  if (!(fIOMode&1))     return 0;
  Clear();
  return GetEvent(1);
}

Int_t StBranch::NextEvent (StUKey &ukey)
{
  if (fIOMode<=0)       return 0;
  if (!(fIOMode&1))     return 0;
  Clear();
  fUKey.Update(ukey,GetName());
  int iret = GetEvent(1); 
  ukey.Update(fUKey); 
  return iret;
}
//_______________________________________________________________________________
void StBranch::SetParAll(TDataSet *par,TList *savList)
{
  TDataSetIter next(this);
  TDataSet *son,*p;
  while ((son=next())) {
    p = son->GetParent();
    if (savList) {assert(p);savList->Add(p);}
    son->SetParent(par);
  }// end while
}
//_______________________________________________________________________________
void StBranch::SetParAll(TList *savList)
{
  assert(savList);
  TDataSetIter next(this);
  TDataSet *son,*p;
  while ((son=next())) {
    p = (TDataSet*)savList->First(); assert(p);
    son->SetParent(p);
    savList->Remove(p);
  }// end while
}
//_______________________________________________________________________________
void StBranch::OpenTFile()
{
  if (fTFile) return;
  TFile *tf= gROOT->GetFile(GetFile());
  fTFile = tf;
  if (!fTFile) fTFile = StIO::Open(GetFile(),TFOPT[fIOMode],GetName());
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
        ,GetName(),TFOPT[fIOMode],(const char*)fFile);

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
StTree::StTree(const Char_t *name):StBranch(name)
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
Int_t StTree::SetFile(const Char_t *file,const Char_t *mode,int insist)
{
  TDataSetIter next(this);StBranch *br;
  if (insist) {while ((br=(StBranch*)next())) br->SetFile(file,mode); return 0;}

  TString br1Name = ::GetBranchByFile(file);
  TString br2Name;
  if (!br1Name.IsNull()) br2Name = br1Name + "Branch";

  const char *brName,*curFile,*oldFile=0;

  while ((br=(StBranch*)next())) { //loop over branches
    curFile = br->GetFile();
    brName = br->GetName();
    if (br1Name == brName)              oldFile = curFile;
    if (br2Name == brName)              oldFile = curFile;
    if (AreSimilar(file,curFile))       oldFile = curFile;
    if (oldFile) break;
  }
  if (!oldFile || !oldFile[0]) oldFile = " ";

  while ((br=(StBranch*)next())) { //loop over branches
    if(strcmp(brName,br->GetName()) && strcmp(oldFile,br->GetFile())) continue;
    br->SetFile(file,mode);
    printf("<%s(%s/%s)::SetFile> file %s is replaced by %s\n",
           ClassName(),GetName(),br->GetName(),oldFile,file);
  }
  return 0;
}

//_______________________________________________________________________________
void StTree::Clear(Option_t*)
{
 TDataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) {if(!br->IsOption("const")) br->Clear();}
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
Int_t StTree::UpdateFile(const Char_t *file)
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
    iret=br->ReadEvent(fUKey);
    if (!iret) num++;
  if(iret==kStErr) return kStErr;
  }
  return (num)? 0:kStEOF;
}

//_______________________________________________________________________________
Int_t StTree::NextEvent(StUKey  &ukey)
{
  fUKey.Update(ukey,GetName());int iret=NextEvent(); ukey.Update(fUKey); return iret;
}

//_______________________________________________________________________________
Int_t StTree::NextEvent()
{
  int iret=0;
  if (Open()) return kStEOF;
  TDataSetIter next(this); StBranch *br; int fst = 1;
  while ((br=(StBranch*)next())) {
    if (! br->fIOMode<0) continue;
    if (! br->fIOMode&1) continue;
      if (fst && !br->IsOption("const")) {  //Read only 1st branch
      iret = br->NextEvent(fUKey);
      if(iret) return iret;
      fst=0;
      continue;
    }
    iret = br->ReadEvent(fUKey);
    if (iret==kStErr) return iret;
  }

  return 0;
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
  if ((Long_t)ret == -1) ret = 0;
  if (ret) ret->SetIOMode("r");
  return ret;
}

//_______________________________________________________________________________
void StTree::SetBaseName(const char *baseName)
{
  fBaseName = baseName;
  const char *dot = strrchr(baseName,'.');
  if (!dot) return;
  const char *sla = strrchr(baseName,'/');
  if (sla && dot<sla) return;
  fBaseName.Remove(dot-baseName);
}

//_______________________________________________________________________________
static TString GetBranchByFile(const Char_t *file)
{
  int dot;
  TString ts = file;
  gSystem->BaseName(ts);
  dot = ts.Last('.'); if (dot<0) return ts.Replace(0,999,"");
  ts.Replace(dot,999,"");
  dot = ts.Last('.');
  if (dot<0) dot = ts.Last('_');
  if (dot<0) return ts.Replace(0,999,"");
  return ts.Replace(0,dot+1,"");
}

//_______________________________________________________________________________
static int AreSimilar(const Char_t *fileA, const Char_t *fileB)
{
  TString A(fileA); gSystem->BaseName(A); A.Replace(0,3,"");
  TString B(fileB); gSystem->BaseName(B); B.Replace(0,3,"");

  int i,n;
  n = A.Length();
  for(i=0;i<n;i++) { if (isdigit(A[i])) A.Replace(i,1," ");}
  A.ReplaceAll(" ","");
  n = B.Length();
  for(i=0;i<n;i++) { if (isdigit(B[i])) B.Replace(i,1," ");}
  B.ReplaceAll(" ","");
  return A==B;
}


//_____________________________________________________________________________
ClassImp(StFile)
 StFile::StFile(const char** fileList):StFileI("StFile")
{
  fDS = new TDataSet();
  fIter = 0; fKeyIter = 0;
  SetTitle(" nbranches=1 ");
  AddFile(fileList);
}
//_____________________________________________________________________________
 StFile::~StFile()
{
  delete fDS;   	fDS      = 0;
  delete fIter; 	fIter    = 0;
  delete fKeyIter;	fKeyIter = 0;
}
//_____________________________________________________________________________
Int_t StFile::GetNextBundle()
{
  if (!fIter) fIter = new TDataSetIter(fDS);
  return !(fIter->Next());
}
//_____________________________________________________________________________
void StFile::RefreshIter()
{
  if (!fIter) return;

  TDataSetIter *it = new TDataSetIter(fDS);
  for (; *(*it) && *(*it)!=*(*fIter);it->Next()) {}
  delete fIter;
  fIter = it;
}
//_____________________________________________________________________________
Int_t StFile::GetNBundles()
{
  if (!fIter) fIter = new TDataSetIter(fDS);
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
  TDataSet *fam = (TDataSet *)**fIter;
  if (!fam) return 0;
  TDataSet *df = fam->At(idx);
  if (!df) return 0;
  SetInfo(df);
  return df;
}
//_____________________________________________________________________________
const Char_t *StFile::GetFileName(Int_t idx)
{
  if (idx == -1) { idx=0; if (GetNextBundle()) return 0;}
  TDataSet *df = GetFileDS(idx);
  if (!df) return 0;
  return strstr(df->GetTitle(),"file=")+5;
}
//_____________________________________________________________________________
const Char_t *StFile::GetFormat(Int_t idx)
{
  TDataSet *df = GetFileDS(idx);
  return GetAttr(df,"format=");
}
//_____________________________________________________________________________
const Char_t *StFile::GetCompName(Int_t idx)
{
  TDataSet *df = GetFileDS(idx);
  return GetAttr(df,"branch=");
}
//_____________________________________________________________________________
Int_t StFile::AddFile(const Char_t **fileList)
{
  const Char_t *file;
  if (!fileList) return 0;
  for(int i=0; (file = fileList[i]);i++) AddFile(file);
  return 0;
}
//_____________________________________________________________________________
Int_t StFile::AddFile(const Char_t *file,const Char_t *branch)
{
  TString tfile,tit,base,famy;
  if (strstr(file,"*")) return AddWild(file);


  tfile = file; gSystem->ExpandPathName(tfile);

  if (!StIO::IfExi(tfile)) {// file does not exist
    Warning("AddFile","*** IGNORED *** File %s does NOT exist \n",
    (const Char_t*)tfile);
    return kStWarn;}

  const char* cc = strrchr(tfile,'.');
  if (!cc || !strstr(".xdf .root .daq",cc)){// No extention
    Warning("AddFile","*** IGNORED *** File %s has wrong extention \n",
    (const Char_t *)tfile);
    return kStWarn;}

  base = gSystem->BaseName(tfile);

  famy = base;
  int dot = famy.Last('.');
  if (dot>0) famy.Replace(dot,999,"");
  dot = famy.Last('.');
  if (dot>0) famy.Replace(dot+1,999,"");

  TDataSet *dsfam = fDS->Find(famy);
  if (!dsfam) fDS->Add((dsfam = new TObjectSet(famy,0)));

  if (dsfam->Find(base))
    Warning("AddFile","File %s added twice \n",(const Char_t *)tfile);

  //  TDataSet *dss = dsfam->First();
  //if (dss)
  //  Warning("AddFile","Files %s and %s are from the same family \n",
  //  (const Char_t *)tfile,dss->GetName());

  tit = tfile; tit.Replace(0,0," file=");

  if (branch) {tit.Replace(0,0,branch); tit.Replace(0,0," br=");}

  TDataSet *ds = new TDataSet(base,dsfam);
  ds->SetTitle(tit);


  RefreshIter();
  if (GetDebug()) printf("<%s::AddFile> Added file %s %s\n",
                  ClassName(),ds->GetName(),ds->GetTitle());

  return 0;
}
//_____________________________________________________________________________
Int_t StFile::AddWild(const Char_t *file)
{
  TString tfile,tdir,tname,tbase,fullname;
  const char *name; char *cc;
  tfile = file;
  tdir  = gSystem->DirName(tfile);
  tbase = gSystem->BaseName(tfile);
  gSystem->ExpandPathName(tdir);


  void *dir = gSystem->OpenDirectory(tdir);
  if (!dir) {
    Warning("AddWild","*** IGNORED Directory %s does NOT exist ***\n",
    (const Char_t *)tdir);
    return kStWarn;}

  while ((name = gSystem->GetDirEntry(dir))) {
//              skip some "special" names
    if (strcmp(name,"..")==0 || strcmp(name,".")==0) continue;
    tname = name;

    cc = gSystem->ConcatFileName(tdir,name);
    fullname = cc; delete [] cc;

    Long_t idqwe,sizeqwe,flags,modtimeqwe;
    gSystem->GetPathInfo(fullname,&idqwe,&sizeqwe,&flags,&modtimeqwe);
    if (flags&1 || flags&2 || !flags&4) continue;

//              prepare simple regular expression
    TRegexp rexp(tbase,kTRUE);
    int len=0;
    if (rexp.Index(tname,&len)!=0)      continue;
    if (len!=tname.Length())            continue;
    AddFile(fullname);
  }
  gSystem->FreeDirectory(dir);
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
  if (!fIter) 		return uk;
  TDataSet *dsfam = **fIter;
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

//              .XDF
  if (strcmp(".xdf",ext)==0) {
    tit.Replace(0,0," format=xdf ");
    known = 3;
    goto RETN;
  }
//              DAQ
  if (strcmp(".daq",ext)==0) {
    tit.Replace(0,0," format=daq ");
    known = 3;
    goto RETN;
  }

//              ROOT
  if (strcmp(".root",ext)==0) {
    tit.Replace(0,0," format=root ");
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
    assert(tf);
    TList *kl = gFile->GetListOfKeys();
    TIter nextKey(kl);
    TKey *ky;
    while ((ky = (TKey*)nextKey())) {
      if (strcmp("StIOEvent",ky->GetClassName()))       continue;
      const char *bra=ky->GetName();
      if (strstr(bra,"tree"))                           continue;
      if (strstr(bra,"Tree"))                           continue;
      i = tit.Index("branch=NONE"); assert(i>=0);
      tit.Replace(i+7,4,bra,strcspn(bra,"."));
  } }

RETN:;
  delete tf;
  ds->SetTitle(tit);
}
//_____________________________________________________________________________
void StFile::ls(Option_t *opt)
{

class Cat : public TNamed {
public:
Cat(){fNGeant=0;fNKeys=0;fNRecs=0;fSize=0;fNFiles=0;};
double fSize;
int    fNGeant;
int    fNKeys;
int    fNRecs;
int    fNFiles;
};


  TList blist;
  int ibr=0,i;
  Cat *cat;

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

}
//_____________________________________________________________________________
const Char_t *StFile::GetAttr(TDataSet *ds,const char *att)
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




