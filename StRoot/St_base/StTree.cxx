#include "StTree.h"

const char* TFOPT[9] = {"0","READ","RECREATE","UPDATE",
                        "0","READ","RECREATE","UPDATE",0};
const char  RWU[] = "0rwu0rnu0rcu";
char IOMODE[] = "0";

inline Int_t IntOMode(char ciomode)
{
char *c=strchr(RWU,tolower(ciomode));
return (c) ? (c-RWU)&3 : 0;
}

ClassImp(StIOEvent)
StIOEvent::StIOEvent():TObject(){};

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
TString &StIO::MakeKey(const Char_t *name, ULong_t  ukey)
{ 
  char ubuf[12];
  TString *tk = new TString(name); *tk +=".";
  sprintf(ubuf,"%010lu",ukey);     *tk +=ubuf;
  return *tk;
}

//_______________________________________________________________________________

Int_t StIO::Write(TFile *file, const Char_t *name, ULong_t  ukey, TObject  *obj)
{
  assert(file);
  if (!obj) return 1;  

  StIOEvent event;
  event.fObj = obj; event.SetUniqueID(ukey);
  TFile *bakfile = gFile; TDirectory *bakdir = gDirectory; file->cd();
  
  TList *lk = gDirectory->GetListOfKeys();  
  if(lk) lk->ResetBit(kStSorted);

  event.Write((const char*)MakeKey(name,ukey),TObject::kOverwrite|TObject::kSingleKey);
  gFile->Flush();
  gFile=bakfile; gDirectory=bakdir;
  return 0;  
}
//_______________________________________________________________________________
TObject *StIO::Read(TFile *file, const Char_t *name, ULong_t  ukey)
{
  assert(file);

  StIOEvent event;
  event.fObj = 0;
  
  TFile *bakfile = gFile; TDirectory *bakdir = gDirectory; file->cd();
  event.Read((const char*)MakeKey(name,ukey));
  gFile=bakfile; gDirectory=bakdir;
  
  assert(event.GetUniqueID()==ukey);
  return event.fObj;
}
//_______________________________________________________________________________
ULong_t StIO::GetNextKey(TFile *file, const Char_t *name, ULong_t ukey)
{
  TObjLink *lnk; TList *lk; const char *kname; TObject *obj;

  assert(file);
  
  TString tk = MakeKey(name,ukey); ukey = kUMAX; 
  const char* prevkey = (const char*)tk;
  int lname = strlen(name)+1;
  
  lk = file->GetListOfKeys();  if(!lk) goto RETURN;
  if (!lk->TestBit(kStSorted)) {lk->Sort(); lk->SetBit(kStSorted);}

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

  if (strncmp(prevkey,kname,lname)) lnk = 0;
  if (!lnk) goto RETURN; 
  ukey = strtoul(kname+lname,0,0);

RETURN:;
  return ukey;  
}
//_______________________________________________________________________________
TObject *StIO::ReadNext(TFile *file, const Char_t *name, ULong_t  &ukey)
{
  ukey = GetNextKey(file,name,ukey);
  if (ukey==kUMAX) return 0;
  return Read(file,name,ukey);
}


//===============================================================================

ClassImp(StBranch)

//_______________________________________________________________________________
StBranch::StBranch(const Char_t *name, StTree *parent):St_DataSet(name,parent)
{

  SetTitle(".StBranch");
  fNEvents=0;fUKey=0;fIOMode=0;fTFile=0;fTFileOwner=0,fDebug=0;
}
StBranch::~StBranch()
{
 Close();
 if (fTFile && fTFileOwner) delete fTFile;
}
//_______________________________________________________________________________
void StBranch::SetOption(Option_t *opt)
{ 
  fOption = opt;
}    
//_______________________________________________________________________________
void StBranch::SetIOMode(Option_t *iomode)
{ 
fIOMode = ::IntOMode(iomode[0]);
}    
//_______________________________________________________________________________
Option_t *StBranch::GetIOMode()
{ 
IOMODE[0] = RWU[fIOMode]; return IOMODE;
}    
  
//_______________________________________________________________________________
Int_t StBranch::SetFile(const Char_t *file)
{ 
  if (!fTFile) { fFile=file; return 0;}
  Error("SetFile","File is already opened");
  return 1;
}

//_______________________________________________________________________________
Int_t StBranch::SetTFile(TFile *tfile)
{ 
  if (!tfile) 		return 0;
  if (fTFile==tfile) 	return 0;
  if (!fTFile) { fTFile=tfile; fFile = fTFile->GetName(); fTFileOwner=0; return 0;}

  Error("SetTFile","TFile is already attached");
  return 1;
}

//_______________________________________________________________________________
void StBranch::Close(const char *)
{ 
  if (!fIOMode) return;
  if (!fTFile) 	return;
  fTFile->Close("");
  printf("** <StBranch::Close> Branch=%s \tFile=%s \tClosed **\n"
        ,GetName(),(const char*)fFile); 
}
//_______________________________________________________________________________
Int_t StBranch::Open()
{
  if (!fIOMode) return 0;
  if (fTFile)   return 0;
  if (fFile.IsNull()) { // Construct file name
    fFile=GetName(); fFile+=".root";
    StTree *tree = (StTree*)GetParent();
    if (tree) { // include base name
      const char* base = tree->GetBaseName();
      if (base) {fFile.Insert(0,"."); fFile.Insert(0,base);}}}

  OpenTFile();

  return 0;
}  
//_______________________________________________________________________________
Int_t StBranch::WriteEvent(ULong_t ukey)
{
  int iret;
  if (!(fIOMode & 2)) return 0;
  SetUKey(ukey);
  if (!fList) 		return 1;	//empty
  if (!fList->First()) 	return 1;	//empty
  Open(); fNEvents++;
  
  TList *savList = new TList;
  SetParAll(0,savList);
  iret= StIO::Write(fTFile,GetName(),fUKey,fList);
  SetParAll(savList); delete savList;
  return 0;
}  
  
//_______________________________________________________________________________
Int_t StBranch::GetEvent(Int_t mode)
{
  if (!(fIOMode&1)) return 0;
  Delete(); if (fList) delete fList; fList=0; 
  Open(); 
  if (mode) { fList = (TList*)StIO::ReadNext(fTFile,GetName(),fUKey);
  } else    { fList = (TList*)StIO::Read    (fTFile,GetName(),fUKey);}
  SetParAll(this,0);
  return (!fList);
}  
//_______________________________________________________________________________
Int_t StBranch::ReadEvent(ULong_t ukey)
{   
  if (!(fIOMode&1)) return 0;
  SetUKey(ukey); 
  return GetEvent(0);
}
//_______________________________________________________________________________
Int_t StBranch::NextEvent()
{   
  if (!(fIOMode&1)) return 0;
  Clear();
  return GetEvent(1);
}

Int_t StBranch::NextEvent (ULong_t &ukey)
{
  if (!(fIOMode&1)) return 0;
  Clear();
  fUKey=ukey; int iret = GetEvent(1); ukey=fUKey; return iret;
}
//_______________________________________________________________________________
void StBranch::SetParAll(St_DataSet *par,TList *savList)
{
  St_DataSetIter next(this);
  St_DataSet *son,*p;
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
  St_DataSetIter next(this);
  St_DataSet *son,*p;
  while ((son=next())) {
    p = (St_DataSet*)savList->First(); assert(p);
    son->SetParent(p);  
    savList->Remove(p);
  }// end while  
}
//_______________________________________________________________________________
void StBranch::OpenTFile()
{
int maxIOMode=fIOMode;
StBranch *tree,*bran;

  for (int iter=0; iter <2; iter++) {// 2 iters. 1st to find, 2nd to fill
    tree = (StBranch*)GetParent();
    if (tree) {	//loop all the branches
      St_DataSetIter next(tree);
      while((bran=(StBranch*)next())) {// 
        if (bran == this)			continue;
        if (!bran->fTFile) 			continue;
        if (fFile != bran->fTFile->GetName()) 	continue;
        if (iter==0) if (bran->fIOMode > maxIOMode) maxIOMode = bran->fIOMode;
        else         bran->fTFile = fTFile;
  } } 
  if (iter) break;

  fTFileOwner=1; 
  fTFile = new TFile(fFile,TFOPT[maxIOMode],GetName());
  printf("** <StBranch::Open> Branch=%s \tMode=%s \tFile=%s \tOpened **\n"
        ,GetName(),TFOPT[maxIOMode],(const char*)fFile);
  } 

}
//_______________________________________________________________________________
void StBranch::Clear(Option_t *)
{
  if (fList) fList->Clear();
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
 St_DataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) { br->SetIOMode(iomode);}
} 

//_______________________________________________________________________________
void StTree::Clear(Option_t*)
{
 St_DataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) { br->Clear();}
} 

//_______________________________________________________________________________
Int_t StTree::Open()
{  
  int iret=0;

  St_DataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) { 
    if (br->Open()) iret++;
  }
  return iret;
}

//_______________________________________________________________________________
Int_t StTree::WriteEvent(ULong_t  ukey)
{  
  SetUKey(ukey);
  St_DataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) br->WriteEvent(fUKey);
  fNEvents++;
  return 0;
}  
  
//_______________________________________________________________________________
Int_t StTree::ReadEvent(ULong_t  ukey)
{  
  SetUKey(ukey);
  St_DataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) br->ReadEvent(fUKey);
  return 0;
}  
  
//_______________________________________________________________________________
Int_t StTree::NextEvent(ULong_t  &ukey)
{  
  SetUKey(ukey);int iret=NextEvent(); ukey = fUKey; return iret;
} 

//_______________________________________________________________________________
Int_t StTree::NextEvent()
{
  St_DataSetIter next(this); int num=0; StBranch *br;
  while ((br=(StBranch*)next())) {
    if (!num++) {br->NextEvent(fUKey); if(fUKey==kUMAX) return 1; continue;}    
    br->ReadEvent(fUKey);}
  return 0;
}  
  
//_______________________________________________________________________________
void StTree::Close(const char* opt)
{  
  TString treeKey(GetName()); treeKey += ".tree";
  Clear();
  St_DataSetIter next(this); StBranch *br;
  while ((br=(StBranch*)next())) {
    if (!br->fIOMode) 				continue;
    TFile *tfbr = br->GetTFile(); if(!tfbr) 	continue;
    if (!tfbr->IsOpen())			continue;
    if (br->fIOMode&2 && tfbr->IsWritable()) {
      St_DataSet *par = GetParent(); SetParent(0);
      StIO::Write(tfbr,(const char*)treeKey,2000,this);
      SetParent(par);}
    if (!(opt && strcmp(opt,"keep")==0)) br->Close();
  }
}  

//_______________________________________________________________________________
StTree *StTree::GetTree(TFile *file, const char *treeName)
{
  StTree *ret;
  TString treeKey(treeName); treeKey += ".tree";
  ret = (StTree*)StIO::Read(file,(const char*)treeKey,2000);
  if (ret) ret->SetIOMode("r");
  return ret;
}

//_______________________________________________________________________________
void StTree::SetBaseName(const char *baseName)
{
  fBaseName = baseName;
  fBaseName = gSystem->BaseName((const char*)fBaseName);
  int idot  = fBaseName.Index(".");
  if (idot>0) fBaseName.Remove(idot);
}  
  
  
  
  
