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
:StMaker(name)
{
  fFileName = ioFile; fIOMode="0";fTreeName=treeName;fTree=0;
}
//_____________________________________________________________________________
StTreeMaker::~StTreeMaker(){
}
//_____________________________________________________________________________
Int_t StTreeMaker::Open(const char *file)
{
  assert(strchr("rw",fIOMode[0]));

  if (fTreeName.IsNull()) SetTreeName();
  if (file) fFileName = file;

  if (GetDebug()) 
    printf("<%s(%s)::Init> TreeName = %s\n",ClassName(),GetName(),GetTreeName());


  if (fIOMode[0]=='r') { //Read mode

//  		ReadReadReadReadReadReadReadReadReadReadReadRead


    if (!fTree) {//Make tree

      TFile tf(fFileName,"read","BFC StTree file");
      if (tf.IsZombie()) {
	Error("Init","Wrong input file %s\n",(const char*)fFileName);
	return kStErr;
      }

      fTree = StTree::GetTree(&tf,GetTreeName()); assert(fTree);
      if (GetDebug()) 
	printf("<%s(%s)::Init> FOUND Tree %s in file %s\n",
	ClassName(),GetName(),
	fTree->GetName(),fFileName.Data());

      AddData(fTree);
//		Register for outer world
      SetOutput(fTree);
  }
    fTree->UpdateFile(fFileName);
    fTree->SetUKey(0);
    
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
      if (!fFileName.IsNull()) fTree->SetBaseName(fFileName);

//    		?????? Compatibility ???????
      if ( fTreeName=="bfcTree") SetBranch("dstBranch");

//		Update Tree from .branches
      St_DataSet *brin;
      St_DataSet *branches = Find(".branches");
      St_DataSetIter nextBr(branches);
      while((brin=nextBr())) {	//branches loop
	const char *brName = brin->GetName();
        StBranch *br = (StBranch*)fTree->Find(brName);
        if (!br) { br = new StBranch(brName,fTree); br->SetIOMode("w");}
        
        const char *brTitle = brin->GetTitle();
        if (strncmp("SetBranch:",brTitle,10)) continue;
        TString ts; const char *cc;int ncc;
        cc = strstr(brTitle,"file=");
        if (cc) { //File name found
          ncc = strcspn(cc+5," "); ts.Replace(0,999,cc+5,ncc); br->SetFile(ts);}
        cc = strstr(brTitle,"mode=");
        if (cc) { //IO mode found
          ncc = strcspn(cc+5," "); ts.Replace(0,999,cc+5,ncc); br->SetIOMode(ts);}
        delete brin;
      }//end of branches loop
    }//end of new tree
    

    fTree->Open();
    fTree->Close("keep");
  } 
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Init()
{
  assert(!fTree);
  return Open();
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
  return Close();
}
//_____________________________________________________________________________
Int_t StTreeMaker::Close()
{ 
  fTree->Close(); return 0;
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
   StBranch *BranchOfMaker(StMaker* mk);

//_____________________________________________________________________________
void StTreeMaker::SetBranch(const Char_t *brName,const Char_t *file,const Char_t *mode)
{
StBranch *br;

  if (fTree) {//Tree already exists
     br = (StBranch*)fTree->Find(brName);
     if (!br) br = new StBranch(brName);
     if (file) br->SetFile(file);
     if (mode) br->SetIOMode(mode);
  } else { //Tree adoes not exist yet
    TString ts("SetBranch:");

    if (file) { ts += " file="; ts += file;}
    if (mode) { ts += " mode="; ts += mode;}
    IntoBranch(brName,ts);
  }
}
//_____________________________________________________________________________
void StTreeMaker::IntoBranch(const Char_t *brName,const Char_t *logNames)
{
 AddAlias(brName,logNames,".branches");  
}







