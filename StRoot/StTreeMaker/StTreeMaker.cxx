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
StTreeMaker::StTreeMaker(const char *name, const char *ioFile):StMaker(name)
{fFileName = ioFile; fIOMode="0";
}
//_____________________________________________________________________________
StTreeMaker::~StTreeMaker(){
}
//_____________________________________________________________________________
Int_t StTreeMaker::Init()
{
  assert(strchr("rw",fIOMode[0]));
  if (fIOMode[0]=='r') { //Read mode
    TFile *tf = new TFile(fFileName,"read","BFC StTree file");
    fTree = StTree::GetTree(tf,"bfc");
    AddData(fTree);
    fTree->SetUKey(0);
    
  } else            { //Write mode  

    TString BaseName,FileName;

    fTree = new StTree("bfc"); 
    if (!fFileName.IsNull()) fTree->SetBaseName(fFileName);
    
    fTopMaker = GetMaker(this);
    fMakers   = fTopMaker->Find(".make");
    assert (fMakers);
    fDefaultBranch = GetInput(".default");
//		Create structure of branches
    St_DataSetIter next(fMakers);
    StMaker *mk;
    StBranch *br;
    while ((mk=(StMaker*)next())) {//loop over makers
      br = BranchOfMaker(mk,1999);
      if (!br) 	continue;
      printf("** <%s::%s> maker \"%s\" \tassigned to branch \"%s\" **\n"
            ,ClassName(),"Init",mk->GetName(),br->GetName());
    }
    
    fTree->SetIOMode("w");
    fTree->Close("keep");
  } 
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
  return fTree->NextEvent();
}    
//_____________________________________________________________________________
Int_t StTreeMaker::MakeWrite(){

//		Fill branches
  St_DataSetIter next(fMakers);
  StMaker *mk;

  StBranch *br;St_DataSet *dat,*ds;
  while ((mk=(StMaker*)next())) {//loop over makers
    br = BranchOfMaker(mk,1999);
    if (!br) 	continue;
    dat = mk->Find(".data"); assert(dat);
    St_DataSetIter nextDs(dat);
    while((ds = nextDs())) br->Add(ds);
  }
//		Write StTree
  ULong_t ukey = fTopMaker->GetNumber();
  fTree->WriteEvent(ukey);	
  fTree->Clear(); 
  return 0;
}
//_____________________________________________________________________________
Int_t StTreeMaker::Finish()
{ 
  fTree->Close(); return 0;
}

//_____________________________________________________________________________
   StBranch *StTreeMaker::BranchOfMaker(StMaker* mk, int doit)
{
  const char *mkName;
  TString brName; 

  StBranch *br;
  if (mk==this) 		return 0;
  mkName = mk->GetName();
  if (!strcmp("db",mkName)) 	return 0;
  brName = GetInput(mkName);
  if (brName.IsNull()) brName = fDefaultBranch;
  if (brName.IsNull()) 		return 0;
  br = (StBranch*)fTree->Find((const char*)brName); 	
  if (!doit) return br;
  if (!br) br = new StBranch(brName,fTree);
  return br;
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
