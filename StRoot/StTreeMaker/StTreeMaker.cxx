//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTreeMaker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTreeMaker.h"
#include "St_particle_Table.h"

ClassImp(StTreeMaker)

//_____________________________________________________________________________
StTreeMaker::StTreeMaker(const char *name, const char *ioFile,const char *treeName )
:StIOInterFace(name,"0")
{
  fFile = ioFile; fIOMode="0";fTreeName=treeName;fTree=0;
}
//_____________________________________________________________________________
StTreeMaker::~StTreeMaker(){
}
//_____________________________________________________________________________
Int_t StTreeMaker::Init()
{
  assert(strchr("rw",fIOMode[0]));
  assert(!fTree);

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
    Open();
    
  } else            { //Write mode  

//		WriteWriteWriteWriteWriteWriteWriteWriteWriteWrite

    TString BaseName,FileName;
    
//  	Try to get it from Read  StTreeMaker       
    fTree = (StTree*)GetDataSet(GetTreeName());
    if (fTree) {		// Fantastic, we found it!!!

      StMaker *mk = GetMaker(fTree);  assert(mk);
      if (GetDebug()) 
        printf("<%s(%s)::Init> FOUND Tree %s.%s\n",
        ClassName(),GetName(),
        mk->GetName(),fTree->GetName());

    } else {			// Create new tree

      fTree = new StTree(GetTreeName()); 
      if (!fFile.IsNull()) fTree->SetBaseName(fFile);

//    		?????? Compatibility ???????
      if ( fTreeName=="bfcTree") SetBranch("dstBranch");

    }//end of new tree
    

    Open();
    fTree->Close("keep");
  } 
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Open(const char *)
{
  UpdateTree();
  fTree->SetUKey(0);
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Make(){
   
  if (fIOMode[0]=='r')  { //Read mode
    return MakeRead();
  } else 		{ //Write mode
    return MakeWrite();
  }
}
//_____________________________________________________________________________
Int_t StTreeMaker::MakeRead(){
  int iret = fTree->NextEvent();
  St_DataSetIter nextBr(fTree);
  StBranch *br ;
  while ((br = (StBranch*)nextBr())) SetOutputAll(br);
  return iret;
}    
//_____________________________________________________________________________
Int_t StTreeMaker::MakeWrite()
{

//		Fill branches

  StBranch *br;St_DataSet *dat,*ds,*inBr,*intoBrs;
  const char* logs;int nlog;
  TString tlog;
  
  
  intoBrs = Find(".branches");
  if (!intoBrs) return kStWarn;
  St_DataSetIter intoNext(intoBrs);
  
  while ((inBr=intoNext())) {//loop intoBR
    br = (StBranch*)fTree->Find(inBr->GetName());
    if (!br) 	continue;

    logs = inBr->GetTitle();nlog=0;
    while(1999) //loop over log names
    {
      logs += nlog + strspn(logs+nlog," "); nlog = strcspn(logs," ");if(!nlog) break; 
      tlog.Replace(0,999,logs,nlog); 
      dat = GetDataSet(logs);  if (!dat) continue;      
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
  }//end of intoBR

//		Write StTree
  ULong_t ukey = GetNumber();
  fTree->WriteEvent(ukey);	
  fTree->Clear(); 
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Finish()
{ 
  Close(); return 0;
}
//_____________________________________________________________________________
void StTreeMaker::Close(Option_t *)
{ 
  fTree->Close(); fTree->SetUKey(0);
}
//_____________________________________________________________________________
void StTreeMaker::Clear(Option_t *opt)
{
  if (opt){/*touch*/}
  fTree->Clear();
}


//_____________________________________________________________________________
void StTreeMaker::PrintInfo(){
  if (GetDebug()) printf("StTreeMaker\n"); //  %s %s \n",GetName(), GetTitle());
}

//_____________________________________________________________________________
void StTreeMaker::UpdateTree()
{
  StBranch *br;
  St_DataSet *upd,*updList;
  const char *updName,*updTitl,*cc;
  TString updFile,updMode;
      
  if (!fTree) return;
  updList = Find(".branches");
  if (!updList) return;
  St_DataSetIter nextUpd(updList);
  while ((upd = nextUpd()))
  {
    updTitl = upd->GetTitle();
    if (strncmp("SetBranch:",updTitl,10))	continue;
    updName = upd->GetName();
    updFile = ""; cc = strstr(updTitl,"file="); 
    if (cc) updFile.Replace(0,0,cc+5,strcspn(cc+5," "));
    updMode = ""; cc = strstr(updTitl,"mode="); 
    if (cc) updMode.Replace(0,0,cc+5,strcspn(cc+5," "));
    delete upd;
  
    if (updName[0]=='*') { //Wild Card
      if (!updMode.IsNull()) fTree->SetIOMode(updMode);  
      if (!updFile.IsNull()) fTree->SetFile  (updFile);  
      continue;
    } //endif *
    
    br = (StBranch*)fTree->Find(updName);
    if (!br && fIOMode=="w") br = new StBranch(updName,fTree);
    if (!br) 					continue;
    if (!updMode.IsNull()) br->SetIOMode(updMode);  
    if (!updFile.IsNull()) br->SetFile  (updFile);  
  }//end updates  
  
}








