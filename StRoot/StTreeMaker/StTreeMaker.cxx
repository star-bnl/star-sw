//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTreeMaker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTreeMaker.h"
#include "TInterpreter.h"


ClassImp(StTreeMaker)
static void RuncoHist(StTree *tree);


//_____________________________________________________________________________
StTreeMaker::StTreeMaker(const char *name, const char *ioFile,const char *treeName )
:StIOInterFace(name,"0")
{
  fFile = ioFile; fIOMode="0";fTree=0;fFinished=0;

  if ( treeName ) {
    fTreeName=treeName;
  } else {
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
  assert(strchr("rwu",fIOMode[0]));
  if(fTree) return 0;

  if (fTreeName.IsNull()) SetTreeName();

  if (GetDebug()) 
    printf("<%s(%s)::Init> TreeName = %s\n",ClassName(),GetName(),GetTreeName());


  if (fIOMode[0]=='r') { //Read mode

//  		ReadReadReadReadReadReadReadReadReadReadReadRead


    if (!fTree) {//Make tree

      TFile tf(fFile,"read","BFC StTree file");
      if (tf.IsZombie()) {
	Error("Init","Wrong input file %s\n",(const char*)fFile);
	return kStErr;
      }

      fTree = StTree::GetTree(&tf,GetTreeName()); assert(fTree);
      if (GetDebug()) 
	printf("<%s(%s)::Init> FOUND Tree %s in file %s\n",
	ClassName(),GetName(),
	fTree->GetName(),fFile.Data());

      AddData(fTree);
//		Register for outer world
      SetOutput(fTree);
      fTree->UpdateFile(fFile);
    }

//   
//    	Several default branches
    SetBranch("histBranch" ,0,"r","const");
    SetBranch("runcoBranch",0,"r","const");

//

    UpdateTree(0);
    fTree->SetUKey(0);
    
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
      if (!fFile.IsNull()) fTree->SetBaseName(fFile);

//    		Several default branches
      if ( fTreeName=="bfcTree") { 
        if (!Find(".branches/dstBranch"  )) SetBranch("dstBranch"  ,0,"w");
        if (!Find(".branches/histBranch" )) SetBranch("histBranch" ,0,"w","const");
        if (!Find(".branches/runcoBranch")) SetBranch("runcoBranch",0,"w","const");
      }

//   	Set filename for runcoBranch
    StBranch *h = (StBranch*)fTree->Find("histBranch");
    StBranch *r = (StBranch*)fTree->Find("runcoBranch");
    if (h && r) r->SetFile(h->GetFile(),0,1);
      

    }//end of new tree
    

    UpdateTree(0);
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
Int_t StTreeMaker::Make(){
   
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
Int_t StTreeMaker::MakeRead(){
  int iret = fTree->NextEvent();
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
  }
  gInterpreter->ProcessLine("if (gROOT->GetClass(\"StRegistry\"))StRegistry::Init();");
  return iret;
}    
//_____________________________________________________________________________
Int_t StTreeMaker::MakeWrite()
{

//		Fill branches

  UpdateTree(1);

//		Write StTree
  ULong_t ukey = GetNumber();
  fTree->WriteEvent(ukey);	
  fTree->Clear(); 
  return 0;
}
//_____________________________________________________________________________
void StTreeMaker::UpdateTree(Int_t flag)
{
//		Fill branches

  StBranch *br;
  const char* logs;int nlog,isSetBr,isHist; 
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
    if (!br && fIOMode!="r" && fIOMode!="0") br = new StBranch(updName,fTree);
      
    if (!br) 				continue;
    if (!updMode.IsNull() || !updFile.IsNull()) br->SetFile(updFile,updMode);  
    if (!updOpt.IsNull()) br->SetOption((const char*)updOpt);  
    
    if (flag==0) 		continue;    
    if (*br->GetIOMode()=='0')	continue;

    isHist = (updName=="hist"); 
    if ( (flag==1) != (!isHist)) 	continue;


    logs = (const char*)updTitl;nlog=0;
    while(1999) //loop over log names
    {
      logs += nlog + strspn(logs+nlog," "); nlog = strcspn(logs," ");if(!nlog) break; 
      tlog.Replace(0,999,logs,nlog); 
      dat = GetDataSet(tlog);  if (!dat) continue;      
      if (dat->InheritsFrom(StMaker::Class())) dat = dat->Find(".data"); 
      if (!dat) continue;
      if (*dat->GetName()!='.') 
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
  if (fIOMode[0]!='r')  { //write  mode
    St_DataSetIter  nextBr(fTree);
    StBranch *br;
    fTree->Clear(); 
    while ((br = (StBranch*)nextBr())) {
      if (strncmp("hist",br->GetName() ,4)
      &&  strncmp("runco",br->GetName(),5)) continue;
      FillHistBranch(br);
    }
    fTree->WriteEvent((ULong_t)(-2));	
    fTree->Clear(); 
  }
  Close(); return 0;
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
  if (fTree) {fTree->Close(); delete fTree; fTree=0;}
  SetFile("");
}
//_____________________________________________________________________________
void StTreeMaker::Clear(Option_t *opt)
{
  if (opt){/*touch*/}
  if (fTree) fTree->Clear();
}


//_____________________________________________________________________________
//_____________________________________________________________________________
void StTreeMaker::FillHistBranch(StBranch *histBr)
{
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
    if (ts.Contains("StMaker.h")) {// GetCVS not overloaded
       ds->Warning("StMaker::Init","GetCVS is not overloaded");
       printf("  Please add into file %s the following line: \n",ds->DeclFileName());
       printf("  virtual const char *GetCVS()\n");
       printf("  {static const char cvs[]=\"Tag %sName:$ %sId:$ built \"__DATE__\" \"__TIME__ ; return cvs;}\n\n","$","$");  
     }

    os->SetTitle(ts);
    histBr->Add(os);

    if (strncmp(bname,"hist"   ,4)==0) {//Hist Branch
      dothist = ds->Find(".hist");
      if (!dothist)			continue;
      TList *tl = (TList*)((St_ObjectSet*)dothist)->GetObject();
      if (!tl || !tl->First())		continue;
      os->SetObject(tl,0);}
      
    if (strncmp(bname,"runco",5)==0) {//Run Control Branch
      dotrcp = ds->Find(".runco");
      if (!dotrcp)			continue;
      os->Update(dotrcp); dotrcp->Delete();}
      

  }
  UpdateTree(2);
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
  
