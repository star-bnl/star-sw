
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTreeMaker class, Star IO                                      	//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "TError.h"
#include "St_DataSetIter.h"
#include "StTreeMaker.h"
#include "StObject.h"
TableImpl(dst_bfc_status);

ClassImp(StTreeMaker)
static void RuncoHist(StTree *tree);


//_____________________________________________________________________________
StTreeMaker::StTreeMaker(const char *name, const char *ioFile,const char *treeName )
:StIOInterFace(name,"0")
{
  fFile = ioFile; fIOMode="0";fTree=0;fFinished=0;
  fBfcStatus = new St_dst_bfc_status("BfcStatus",100);
  AddConst(fBfcStatus);
  fTreeName = treeName;
  if (! fTreeName ) {
    TDataSet *parent = GetParent();
    if (parent) {
      StIOInterFace *grandparent = (StIOInterFace *) parent->GetParent();
      if (grandparent) fTreeName = grandparent->GetTreeName();
    }
  }
  if ( !fTreeName ) {
    Warning("StTreeMaker", "%s default treeName == bfcTree is used",name);
    fTreeName = "bfcTree";
  }
}
//_____________________________________________________________________________
StTreeMaker::~StTreeMaker(){
}
//_____________________________________________________________________________
Int_t StTreeMaker::Open(const char*)
{
  int i;
  
  assert(strchr("rwu",fIOMode[0]));
  if(fTree) return 0;

  if (fTreeName.IsNull()) SetTreeName();

  if (GetDebug()) 
    printf("<%s(%s)::Init> TreeName = %s\n",ClassName(),GetName(),GetTreeName());


  if (fIOMode[0]=='r') { //Read mode

//  		ReadReadReadReadReadReadReadReadReadReadReadRead


    if (!fTree) {//Make tree
      class TFilePtr {
         private:
           TFile *fFile;
         public:
           TFilePtr(TFile *f): fFile(f){;}
           ~TFilePtr(){ delete fFile;};
           operator TFile *(){ return fFile; }
           Bool_t IsZombie() const { return fFile?fFile->IsZombie():kTRUE;}
      };
      TFilePtr tf = TFile::Open(fFile,"read","BFC StTree file");
      if (tf.IsZombie()) {
	Error("Init","Wrong input file %s\n",(const char*)fFile);
	return kStErr;
      }

      
      fTree = StTree::GetTree(tf,GetTreeName()); assert(fTree);
      fTree->SetIOMode("0");
      if (GetDebug()) 
	printf("<%s(%s)::Init> FOUND Tree %s in file %s\n",
	ClassName(),GetName(),
	fTree->GetName(),fFile.Data());

//	1st branch must be mentioned in fFile
      TString firstBr(fFile);
      i = firstBr.Last('.'); assert(i>0);
      firstBr.Replace(i,999,"");
      i = firstBr.Last('.'); assert(i>0);
      firstBr.Replace(0,i+1,"");

      St_DataSet *fst = fTree->Find(firstBr);            
      if (!fst) {//not found, add "Branch" to name
        firstBr+="Branch";
        fst = fTree->Find(firstBr);}
      if (fst) {
        ((StBranch*)fst)->SetIOMode("r");
        fst->Shunt(0); fTree->AddFirst(fst);
        printf("<%s(%s)::Init> Branch %s is MAIN in tree\n",ClassName(),GetName(),fst->GetName());
      }
    }

//   
//    	Several default branches
//    SetBranch("histBranch" ,0,"r","const");
//    SetBranch("runcoBranch",0,"r","const");

//

    UpdateTree(0);
    fTree->SetFile(fFile,"r");
    fTree->SetUKey(0);
      AddData(fTree);
//		Register for outer world
      SetOutput(fTree);
//      fTree->SetFile(fFile,"r");
    
  } else            { //Write mode  

//		WriteWriteWriteWriteWriteWriteWriteWriteWriteWrite

    TString BaseName,FileName;
    
//  	Try to get it from Read  StTreeMaker       
    fTree = 0;
    if (fIOMode[0]=='u') fTree = (StTree*)GetDataSet(GetTreeName());
    if (fTree) {		// Fantastic, we found it!!!

      StMaker *mk = GetMaker(fTree);  assert(mk);
      if (GetDebug()) 
        printf("<%s(%s)::Init> FOUND Tree %s.%s\n",
        ClassName(),GetName(),
        mk->GetName(),fTree->GetName());

    } else {			// Create new tree

      fTree = new StTree(GetTreeName()); 

    }//end of new tree
    

    UpdateTree(0);
    fTree->SetFile(fFile,"w");
    fTree->SetUKey(0);
    fTree->Close("keep");
  } 
//	Treat special branches
  RuncoHist(fTree);

  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Init()
{
  return Open();
}
//_____________________________________________________________________________
Int_t   StTreeMaker::Skip(int nskip)
{ 
  if(!fTree) return 0;
  return fTree->Skip(nskip);
}

//_____________________________________________________________________________
Int_t StTreeMaker::Make(){
   
  fNIO++;
  if (fIOMode[0]=='r')  { //Read mode
    int iret=0,ntry=13;
    while(1999) {
      iret = MakeRead();
      if (iret!=kStErr && ntry--) break;
      Warning("Make","%d *** ReadError ***\n",ntry);
    }
    return iret;

  } else 		{ //Write mode

    return MakeWrite();

  }
}
//_____________________________________________________________________________
Int_t StTreeMaker::MakeRead(const StUKey &RunEvent){

  TDataSet *xrefMain[2];
  int iret;
  xrefMain[0] = StXRefManager::GetMain();


  if (!RunEvent.IsNull())	iret = fTree->ReadEvent(RunEvent);
  else                  	iret = fTree->NextEvent(        );

  StEvtHddr *hddr = GetEvtHddr();
  if (Debug()) hddr->Print("");
  if (iret) return iret;
  St_DataSetIter nextBr(fTree);
  StBranch *br ;
  while ((br = (StBranch*)nextBr())){
    SetOutput(br);
    TString tsBr(br->GetName());
    if (tsBr.Contains("Branch")) {
      TString tsName = tsBr;
      tsName.ReplaceAll("Branch","");
      if (!br->Find(tsName)) SetOutput(tsName,br);
    }
    int lv=1; if (strncmp("runco",br->GetName(),5)==0) lv = 2;  
    SetOutputAll(br,lv);
//		copy StEVTHddr (RunEvent)
    if (!hddr) 		continue;
    StEvtHddr *runevt = (StEvtHddr*)br->FindByName("RunEvent");
    if (!runevt)	continue;
    *hddr = *runevt;
//
  }
  xrefMain[1] = StXRefManager::GetMain();
//		New XREF main object?
  if (xrefMain[1] && xrefMain[1]!=xrefMain[0] && xrefMain[1]->GetParent()==0 )
     AddData(xrefMain[1]);
  return iret;
}    
//_____________________________________________________________________________
Int_t StTreeMaker::MakeWrite()
{

//		Fill branches
  MakeBfcStatus();
  if (!GetFailedMaker())  UpdateTree(1);
  UpdateHddr();

//		Write StTree
  Int_t k1 = GetRunNumber();
  Int_t k2 = GetEventNumber();
  if (k1 <= 0 || k2 <= 0) { k1 = GetNumber(); k2=0; }  
  StUKey ukey(k1,k2);
  fTree->WriteEvent(ukey);	
  fTree->Clear(); 
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::MakeBfcStatus()
{
  fBfcStatus->SetNRows(0);
  int nrows = 0;
  TDataSetIter nextMk(GetParentChain(),999);
  TDataSet *ds = 0;
  EDataSetPass kont=kContinue;
  const char *name=0;
  while ((ds = nextMk(kont))) 
  {
    name = ds->GetName();
    kont = kContinue;
    if (strcmp(".make",name)==0) 		continue;
    kont = kPrune;
    if (name[0]=='.') 				continue;
    kont = kContinue;
    if (!ds->InheritsFrom(StMaker::Class()))	continue;
    int ret = ((StMaker*)ds)->GetMakeReturn();
    //    if (!ret) 					continue;
    fBfcStatus->SetNRows(++nrows);
    char * mkName = (*fBfcStatus)[nrows-1].maker_name;   
    mkName[0]=0; strncat(mkName,name,11);
    (*fBfcStatus)[nrows-1].status = ret;
  }

  return 0;
}
//_____________________________________________________________________________
void StTreeMaker::UpdateHddr()
{
//		Fill branches by Run/Event info

  StBranch *br;
  Option_t *opt=0;
  St_DataSet *hd = GetEvtHddr();
  StMaker *failmk = GetFailedMaker();
  TString ts;
  if (failmk ) {
    ts = "discarded by ";
    ts += failmk->ClassName();
    ts += "::";
    ts += failmk->GetName();
    hd->SetTitle(ts);
  }
    
  St_DataSetIter nextBr(fTree);
  
  while ((br = (StBranch *)nextBr())) {	//Looop over branches
    if ((opt = br->GetOption()) && strstr(opt,"CONST")) continue;
    if (!failmk && !br->First())			continue;
    St_DataSet *ds = br;
    if (ds->First() && ds->First() == ds->Last()) ds = ds->First();
    if (ds->Find("RunEvent"))			continue; 
    St_DataSet *hdd = (St_DataSet*)hd->Clone();
    hdd->SetName ("RunEvent");
    hdd->SetTitle(ts);
    ds->AddFirst(hdd);
    if (fBfcStatus->GetNRows()) br->Add(fBfcStatus);
  }
}          

//_____________________________________________________________________________
void StTreeMaker::UpdateTree(Int_t flag)
{
//		Fill branches

  StBranch *br;
  const char* logs;int nlog,isSetBr; 
  St_DataSet *upd,*updList,*dat,*ds;
  const char *cc;

  TString updName,updTitl,updFile,updMode,updOpt,tlog;
  
  updList= Find(".branches");
  if (!updList) return;
  St_DataSetIter updNext(updList);
  while ((upd=updNext())) {//loop updates
    updTitl = upd->GetTitle();
    updName = upd->GetName();
    updFile = ""; updMode = ""; updOpt = "";
    isSetBr = (updTitl.Index("SetBranch:")==0);
    if (isSetBr && flag!=0)	continue;

    if (isSetBr) {//SetBranch block
      cc = strstr(updTitl,"file="); 
      if (cc) updFile.Replace(0,0,cc+5,strcspn(cc+5," "));
      cc = strstr(updTitl,"mode="); 
      if (cc) updMode.Replace(0,0,cc+5,strcspn(cc+5," "));
      cc = strstr(updTitl,"opt="); 
      if (cc) updOpt.Replace (0,0,cc+4,strcspn(cc+4," "));
  
      if (!updFile.IsNull())	delete upd;	//delete SetBranch with concrete filename
    } //endif SetBranch block*
    
    if (updName[0]=='*') { //Wild Card
        if (!updMode.IsNull() || !updFile.IsNull()) fTree->SetFile(updFile,updMode,1);  
        continue;}
        
    br = (StBranch*)fTree->Find(updName);
    if (!br) {br = new StBranch(updName,fTree);br->SetIOMode(fIOMode);}
    if (!updMode.IsNull() || !updFile.IsNull()) br->SetFile(updFile,updMode);  
    if (!updOpt.IsNull()) br->SetOption((const char*)updOpt);  
    
    if (flag==0) 		continue;    

    logs = (const char*)updTitl;nlog=0;
    while(1999) //loop over log names
    {
      logs += nlog + strspn(logs+nlog," "); nlog = strcspn(logs," ");if(!nlog) break; 
      tlog.Replace(0,999,logs,nlog); 
      dat = GetDataSet(tlog);  if (!dat) continue;      
      if (dat->InheritsFrom(StMaker::Class())) dat = dat->Find(".data"); 
      if (!dat) continue;
      if (*dat->GetName()!='.' && !dat->InheritsFrom(StBranch::Class())) 
      {
        br->Add(dat);
      } else { 
        St_DataSetIter nextDs(dat);
        while((ds = nextDs())) br->Add(ds);
      }//end of datasets
    }//end of log names

  }//end of updates

}
//_____________________________________________________________________________
Int_t StTreeMaker::Finish()
{ 
  if (fFinished) return 0;
  fFinished = 1999;
  if (!fTree)    return 0;
  if (fIOMode[0]!='r')  { //write  mode
    St_DataSetIter  nextBr(fTree);
    StBranch *br;
    fTree->Clear(); 
    while ((br = (StBranch*)nextBr())) {
      if (strncmp("hist" ,br->GetName(),4)
      &&  strncmp("runco",br->GetName(),5)) continue;
      FillHistBranch(br);
    }
    fTree->WriteEvent((ULong_t)(-2));	
    fTree->Clear(); 
  }
  Close(); 
  StIOInterFace::Finish();
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Save()
{ 
  St_DataSetIter  nextBr(fTree);
  StBranch *br,*brSave=0;
  TString saveName,saveDir,regPath;
  char *savePath; 
  fTree->Clear(); 
  while ((br = (StBranch*)nextBr())) {
    if (strncmp("hist",br->GetName(),4)) continue;
    brSave=br;
    FillHistBranch(br);
  }
  if (!brSave) return 0;
  regPath = brSave->GetFile();
  if (!regPath.Contains(".root")) return 0;
  saveDir  = gSystem->DirName (regPath);
  saveName = gSystem->BaseName(regPath);
  saveName.Replace(0,0,"save.");
  savePath = gSystem->ConcatFileName(saveDir,saveName);
  brSave->Close();
  brSave->SetFile((const char*)savePath);
  brSave->WriteEvent((ULong_t)(-2));	
  fTree->Close("keep");
  brSave->Close();
  brSave->Clear(); 
  brSave->SetFile((const char*)regPath); 
  delete [] savePath;
  brSave->Open();  
  
   
  return 0;
}
//_____________________________________________________________________________
void StTreeMaker::Close(Option_t *)
{ 
  if (fTree) {
    m_DataSet->Remove(fTree); 	fTree->Close(); 
    delete fTree; 		fTree=0;}
  SetFile("");
}
//_____________________________________________________________________________
void StTreeMaker::Clear(Option_t *opt)
{
  if (opt){/*touch*/}
  if (fTree) {m_DataSet->Remove(fTree);fTree->Clear();}
  m_DataSet->Delete();
  if (fTree) m_DataSet->Add(fTree);
}


//_____________________________________________________________________________
//_____________________________________________________________________________
void StTreeMaker::FillHistBranch(StBranch *histBr)
{
  int nAkt = 0;
  StMaker *top,*upp;
  St_DataSet *ds,*par,*dothist,*dotrcp;  
  const char *bname = histBr->GetName();

  top = this;
  while((upp=GetMaker(top))) top = upp;
  
  St_DataSetIter nextDs(top,999);
  while ((ds= nextDs())) { //loop over all stru
    par = ds->GetParent();
    if (!par)				continue;
    if (strcmp(".make",par->GetName()))	continue;

    TString ts(ds->GetName());
    if (strncmp(bname,"hist"   ,4)==0) ts +="Hist";
    if (strncmp(bname,"runco",  5)==0) ts +="Runco";


    St_ObjectSet *os = new St_ObjectSet(ts);
    ts = ((StMaker*)ds)->GetCVS();
    if (ts.Contains("StMaker.h") 
    && ds->IsA() != StMaker::Class()) {// GetCVS not overloaded
       ds->Warning("StMaker::Init","GetCVS is not overloaded");
       printf("  Please add into file %s the following line: \n",ds->IsA()->GetDeclFileName());
       printf("  virtual const char *GetCVS() const\n");
       printf("  {static const char cvs[]=\"Tag %sName:$ %sId:$ built \"__DATE__\" \" __TIME__ ; return cvs;}\n\n","$","$");  
     }

    os->SetTitle(ts);
    histBr->Add(os);

    if (strncmp(bname,"hist"   ,4)==0) {//Hist Branch
      dothist = ds->Find(".hist");
      if (!dothist) 		{delete os;continue;}
      TList *tl = (TList*)((St_ObjectSet*)dothist)->GetObject();
      if (!tl || !tl->First())	{delete os;continue;}	
      nAkt++;
      os->SetObject(tl,0);}
      
    if (strncmp(bname,"runco",5)==0) {//Run Control Branch
      dotrcp = ds->Find(".runco");
      if (!dotrcp)			continue;
      nAkt++;
      os->Update(dotrcp); dotrcp->Delete();}
      

  }
  histBr->SetIOMode("0");
  if (nAkt) histBr->SetIOMode("w");


  //UpdateTree(2);
}

static void RuncoHist(StTree *tree)
{
//	special function to preset options. Keep backward compat
  if(!tree) return;
  St_DataSetIter nextBr(tree);
  StBranch *br=0;
  while ((br=(StBranch*)nextBr())) {//loop over branches
    if (strcmp(br->GetName(),"histBranch" )==0) br->SetOption("const");
    if (strcmp(br->GetName(),"runcoBranch")==0) br->SetOption("const");
  }  
}  
  
