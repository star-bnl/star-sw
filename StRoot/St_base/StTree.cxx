#include "Stypes.h"
#include "StTree.h"
#include "TRegexp.h"
#include "TKey.h"


const char* TFOPT[9] = {"0","READ","RECREATE","UPDATE",
                        "0","READ","RECREATE","UPDATE",0};
const char  RWU[] = "0rwu0rnu0rcu";
char IOMODE[] = "0";

static inline Int_t IntOMode(char ciomode)
{
char *c=(char *)strchr(RWU,tolower(ciomode));
return (c) ? (c-RWU)&3 : 0;
}
//		Local functions
static TString GetBranchByFile(const Char_t *file);
static int AreSimilar(const Char_t *fileA, const Char_t *fileB);

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
  if (!obj) return kStWarn;  

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
TObject *StIO::Read(TFile *file, const Char_t *name)
{
  TObject *toread=0;
  TKey *key = 0;
  assert(file);

  TFile *bakfile = gFile; TDirectory *bakdir = gDirectory; file->cd();

  if (!gFile) { printf("<StIO::Read> No file open \n"); goto RETURN;}
    
  if (name[0]=='*') 
       key = (TKey*)gDirectory->GetListOfKeys()->First();
  else key = (TKey*)gDirectory->GetListOfKeys()->FindObject(name);

  if (!key)  { printf("<StIO::Read> Key %s not found\n",name); goto RETURN; }

  toread = key->ReadObj();

RETURN: gFile=bakfile; gDirectory=bakdir; return toread;
}
//_______________________________________________________________________________
TObject *StIO::Read(TFile *file, const Char_t *name, ULong_t  ukey)
{
  StIOEvent *event = 0;  
  TObject   *retn  = 0;
  event = (StIOEvent*)Read(file,(const char*)MakeKey(name,ukey));
  if (!event) 		return 0;
  retn = event->fObj; 
  assert( !retn || retn==(TObject*)(-1) || event->GetUniqueID()==ukey);
  delete event;
  return retn;
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
  while( !isdigit(kname[lname])){lname++;} 
  ukey = strtoul(kname+lname,0,10);

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
  fNEvents=0;fUKey=0;fIOMode=0;fTFile=0;fDebug=0;
}
StBranch::~StBranch()
{
 Close();

}
//_______________________________________________________________________________
void StBranch::SetOption(Option_t *opt)
{ 
  fOption = opt;
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
IOMODE[0] = RWU[fIOMode]; return IOMODE;
}    
  
//_______________________________________________________________________________
Int_t StBranch::SetFile(const Char_t *file,const Char_t *mode,int insist)
{ 
  if (fTFile && !insist) { Error("SetFile","File is already opened");return kStWarn;}
  if (file && file[0]) fFile=file; 
  if (mode && mode[0]) SetIOMode(mode);
  return 0;
}
//_______________________________________________________________________________
Int_t StBranch::UpdateFile(const Char_t *file)
{
TString outFile = file; gSystem->ExpandPathName(outFile);
TString outDir  = gSystem->DirName (outFile);
TString outBas  = gSystem->BaseName(outFile);

if (strncmp(".none",GetFile(),4)==0) {//FileName was corrupted by old bug
  TString ts("/.nowhere/"); 	ts += outBas; 	
  ts.ReplaceAll(".root","");	ts.Replace(ts.Last('.')+1,999,"");
  ts += GetName();	   	ts.ReplaceAll("Branch","");
  ts +=".root";			SetFile(ts);
}

TString intDir  = gSystem->DirName (GetFile());
TString intBas  = gSystem->BaseName(GetFile());
Char_t * newFile = gSystem->ConcatFileName(outDir,intBas);
SetIOMode("0");
if (intBas == outBas) SetIOMode("r");
if (intBas == outBas || outBas.IsNull()) 	goto RETN00;
if (gSystem->AccessPathName(newFile)) 		goto RETN99;
RETN00: fFile = newFile;
printf("<StBranch::UpdateFile> Branch=%s file %s\n",GetName(),newFile); 
RETN99: delete [] newFile;
return 0;
}
//_______________________________________________________________________________
Int_t StBranch::SetTFile(TFile *tfile)
{ 
  if (!tfile) 		return 0;
  if (fTFile==tfile) 	return 0;
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
  if (!fTFile) 	return;
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
  if (fIOMode&2) return;
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
  if (!fIOMode) return 0;
  if (fTFile)   return 0;
  if (strncmp(".none",GetFile(),4)==0) return 1;
  OpenTFile();
  return 0;
}  
//_______________________________________________________________________________
Int_t StBranch::WriteEvent(ULong_t ukey)
{
  int iret;
  if (!(fIOMode & 2)) return 0;
  SetUKey(ukey);
  if (!fList) 		return kStWarn;	//empty
  if (!fList->First()) 	return kStWarn;	//empty
  if(Open()) return 1; 
  fNEvents++;
  
  TList *savList = new TList;
  SetParAll(0,savList);
  iret= StIO::Write(fTFile,GetName(),fUKey,fList);
  SetParAll(savList); delete savList;
  return 0;
}  
  
//_______________________________________________________________________________
Int_t StBranch::GetEvent(Int_t mode)
{
  TObject *obj=0;
  if (!(fIOMode&1)) return 0;
  Delete(); if (fList) delete fList; fList=0; 
  if(Open()) return 1; 
  if (mode) { obj = StIO::ReadNext(fTFile,GetName(),fUKey);
  } else    { obj = StIO::Read    (fTFile,GetName(),fUKey);}

  if (obj == 0) 		return kStWarn;
  if (obj == (TObject*)(-1))	return kStErr;
  fList = (TList*)obj;

  SetParAll(this,0);
  
  return 0;
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
  if (fTFile) return;
  TFile *tf= gROOT->GetFile(GetFile());
  fTFile = tf;
  if (!fTFile) fTFile = new TFile(GetFile(),TFOPT[fIOMode],GetName());
  if (fTFile->IsZombie()) {
    Error("OpenTFile","File %s NOT OPENED ***\n",fTFile->GetName());
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
 St_DataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) { br->SetIOMode(iomode);}
} 
//_______________________________________________________________________________
Int_t StTree::SetFile(const Char_t *file,const Char_t *mode,int insist)
{
  St_DataSetIter next(this);StBranch *br;
  if (insist) {while ((br=(StBranch*)next())) br->SetFile(file,mode); return 0;}

  TString br1Name = ::GetBranchByFile(file);
  TString br2Name;
  if (!br1Name.IsNull()) br2Name = br1Name + "Branch";

  const char *brName,*curFile,*oldFile=0;

  while ((br=(StBranch*)next())) { //loop over branches
    curFile = br->GetFile();
    brName = br->GetName();
    if (br1Name == brName) 		oldFile = curFile;
    if (br2Name == brName) 		oldFile = curFile;
    if (AreSimilar(file,curFile))	oldFile = curFile;
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
 St_DataSetIter next(this);StBranch *br;
 while ((br=(StBranch*)next())) { br->Clear();}
} 

//_______________________________________________________________________________
Int_t StTree::Open()
{  
  St_DataSetIter nextA(this);StBranch *brA;
  while ((brA=(StBranch*)nextA())) { 
    if (brA->GetIOMode()[0]=='0') 	continue;
    if (brA->GetTFile()) 		continue;
    brA->Open();
  }
  return 0;
}
//_______________________________________________________________________________
Int_t StTree::UpdateFile(const Char_t *file)
{
  St_DataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) br->UpdateFile(file); 
  return 0;
}
 
//_______________________________________________________________________________
Int_t StTree::WriteEvent(ULong_t  ukey)
{  
  SetUKey(ukey); Open();
  St_DataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) br->WriteEvent(fUKey);
  fNEvents++;
  return 0;
}  
  
//_______________________________________________________________________________
Int_t StTree::ReadEvent(ULong_t  ukey)
{  
  Int_t iret=0;
  SetUKey(ukey); Open();
  St_DataSetIter next(this);StBranch *br;
  while ((br=(StBranch*)next())) {	//Read all branches 
    iret=br->ReadEvent(fUKey); 
    if(iret==kStErr) return kStErr;
  }
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
  int iret=0;
  if (Open()) return kStEOF;
  St_DataSetIter next(this); int num=0; StBranch *br;
  while ((br=(StBranch*)next())) {
    if (! br->fIOMode&1) continue;
    if (!num++) {	//Read only 1st branch
      iret = br->NextEvent(fUKey); 
      if(iret) return iret;
      continue;
    }    
    iret = br->ReadEvent(fUKey);
    if (iret==kStErr) return iret;
  }
  
  return (num) ?0 : kStEOF;
}  
  
//_______________________________________________________________________________
void StTree::Close(const char* opt)
{  
  TString treeKey(GetName());
  Clear();
  St_DataSetIter next(this); StBranch *br;
  while ((br=(StBranch*)next())) { //branch loop
    if (br->fIOMode&2) {  
      br->Open();
      TFile *tfbr = br->GetTFile(); if(!tfbr) 	continue;
      if (tfbr->IsWritable()) {
	St_DataSet *par = GetParent(); SetParent(0);
	StIO::Write(tfbr,(const char*)treeKey,2000,this);
	SetParent(par);}
    }
    if ((opt && strcmp(opt,"keep")==0)) continue;
    br->Close();
  }// end branch loop
}  

//_______________________________________________________________________________
StTree *StTree::GetTree(TFile *file, const char *treeName)
{
  StTree *ret; ULong_t u = 2000;
  TString treeKey(treeName);
  ret = (StTree*)StIO::Read(file,(const char*)treeKey,u);
  if(!ret) {ret = (StTree*)StIO::Read(file,"bfc.tree",u);}
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
 StFile::StFile(const char** fileList):St_DataSet("StFile")
{
  SetTitle(" nbranches=1 ");
  AddFile(fileList);
}
//_____________________________________________________________________________
Int_t StFile::AddFile(const Char_t **fileList)
{ 
  const Char_t *file;
  if (!fileList) return 0;
  for(int i=0; file = fileList[i];i++) AddFile(file);
  return 0;
}
//_____________________________________________________________________________
Int_t StFile::AddFile(const Char_t *file,const Char_t *branch)
{ 
  TString tfile,tit,base,famy;
  if (strstr(file,"*")) return AddWild(file);
  
  
  tfile = file; gSystem->ExpandPathName(tfile);
  
  if (gSystem->AccessPathName(tfile)) {// file does not exist
    Warning("AddFile","*** IGNORED *** File %s does NOT exist \n",
    (const Char_t*)tfile);
    return kStWarn;}

  const char* cc = strrchr(tfile,'.');
  if (!cc || !strstr(".xdf .root .daq",cc)){// No extention
    Warning("AddFile","*** IGNORED *** File %s has wrong extention \n",
    (const Char_t *)tfile);
    return kStWarn;}

  base = gSystem->BaseName(tfile);
  if (Find(base)) 
    Warning("AddFile","File %s added twice \n",(const Char_t *)tfile);

  famy = base; 
  int dot = famy.Last('.');
  if (dot>0) famy.Replace(dot,999,""); 
  dot = famy.Last('.');
  if (dot>0) famy.Replace(dot+1,999,""); 
  
  St_DataSetIter next(this);
  St_DataSet *dss;
  while((dss = next())) {
    if (strncmp(famy,dss->GetName(),famy.Length())) 	continue;
    Warning("AddFile","Files %s and %s are from the same family \n",
    (const Char_t *)tfile,dss->GetName());
  }
  
    



  tit = tfile; tit.Replace(0,0," file=");

  if (branch) {tit.Replace(0,0,branch); tit.Replace(0,0," br=");}

  St_DataSet *ds = new St_DataSet(base,this);
  ds->SetTitle(tit);
    

  printf("<%s::AddFile> Added file %s %s\n",
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
// 		skip some "special" names
    if (strcmp(name,"..")==0 || strcmp(name,".")==0) continue;
    tname = name;

    cc = gSystem->ConcatFileName(tdir,name);
    fullname = cc; delete [] cc;

    Long_t idqwe,sizeqwe,flags,modtimeqwe;
    gSystem->GetPathInfo(fullname,&idqwe,&sizeqwe,&flags,&modtimeqwe); 
    if (flags&1 || flags&2 || !flags&4) continue;
    
//		prepare simple regular expression
    TRegexp rexp(tbase,kTRUE);
    int len=0; 
    if (rexp.Index(tname,&len)!=0) 	continue;
    if (len!=tname.Length())		continue;
    AddFile(fullname);
  }
  gSystem->FreeDirectory(dir);
  return 0;
}
//_____________________________________________________________________________
const Char_t * StFile::NextFileName()
{
  St_DataSet *ds;
  while ((ds = First())){
    TString ts(ds->GetTitle());
    if (ts.Index("status=USED")>=0) {delete ds; continue;}
    ts.Replace(0,0," status=USED "); ds->SetTitle(ts); break;}
  if (!ds) return 0;
  SetInfo();
  return strstr(ds->GetTitle(),"file=")+5;
}
//_____________________________________________________________________________
void StFile::SetInfo()
{
  TFile *tf=0;
  St_DataSet *ds = First();
  if (!ds) return;
  TString tit(ds->GetTitle());  
  Int_t known = 0;
  if (strstr(tit,"format=")) known += 1;
  if (strstr(tit,"branch=")) known += 2;
  if (known==3) return;  
  
  const char *fname = strstr(ds->GetTitle(),"file=")+5;
  const char *ext =   strrchr(fname,'.');
  assert(ext);

//		.XDF
  if (strcmp(".xdf",ext)==0) {
    tit.Replace(0,0," format=xdf ");
    tit.Replace(0,0," branch=NONE");
    known = 3;
  } 
//		DAQ
  if (strcmp(".daq",ext)==0) {
    tit.Replace(0,0," format=daq ");
    tit.Replace(0,0," branch=NONE");
    known = 3;
  } 
  
  if (known!=3) {
    assert (!strcmp(".root",ext));

    tf = new TFile(fname,"READ");
    assert(!tf->IsZombie());
    TList *kl = gFile->GetListOfKeys();
    TIter nextKey(kl);
    TKey *ky;
    while ((ky = (TKey*)nextKey())) {
      if (strcmp("StIOEvent",ky->GetClassName())==0) {	//it is post mdc2
	if (!(known&1)) {tit.Replace(0,0," format=root ");known|=1;}
	if (!(known&2)) {
          const char *bra=ky->GetName();
          if (strstr(bra,"tree")) 	continue;
          if (strstr(bra,"Tree")) 	continue;
	  tit.Replace(0,0,bra,strcspn(bra,"."));tit.Replace(0,0,"branch=");
        }
	known =3; break;
      }
      if (!strcmp("TTree" ,ky->GetClassName())
       && !strcmp("Output",ky->GetName()) ) {//it is mdc2
	if (!(known&1)) tit.Replace(0,0," format=mdc2");
	if (!(known&2)) tit.Replace(0,0," branch=tree");
	known =3; break;
      }

      if (known==3) break;
      if (strcmp("TBranchObject",ky->GetClassName())==0) {//it is mdc2
	if (!(known&1)) {tit.Replace(0,0," format=mdc2");}
	if (!(known&2)) {tit.Replace(0,0,ky->GetName());tit.Replace(0,0," branch=");;}
	known =3; 
      }
      if (known==3) break;
    }
  }
  delete tf;
  if (!known&1) tit.Replace(0,0,"format=unknown");   
  if (!known&2) tit.Replace(0,0,"branch=unknown");   
  ds->SetTitle(tit);
}
//_____________________________________________________________________________
const Char_t *StFile::GetAttr(const char *att) 
{
  static TString brName;
  St_DataSet *ds = First();
  if (!ds) return 0;
  SetInfo();
  const char *bn = strstr(ds->GetTitle(),att);
  if (!bn) return 0;
  bn += strlen(att);
  int n = strcspn(bn," ");
  brName.Replace(0,999,bn,n);
  return (const char*)brName;
}
//_____________________________________________________________________________
Int_t StFile::GetNBranches() 
{ 
  const char *cc = strstr(GetTitle(),"nbranches=")+10;
  return atoi(cc);
}




