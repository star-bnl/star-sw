//*CMZ :          23/02/99  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@bnl.gov)   03/07/98
//
//  
//  
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_io_Maker class for Makers                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "St_io_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TSystem.h"
#include "TFile.h"
#include "TTree.h"
#include "TClass.h"

class StIOHeader : public TObject 
{
 private:
    TString    m_BranchName;
    TObject   *m_DataSet;
    TBranch   *m_Branch; 

 public:
  StIOHeader(const Char_t *name, TBranch *branch, TObject *obj=0) : m_BranchName(name),
                                                                    m_Branch(branch),
                                                                    m_DataSet(obj){SetAddress();}
  StIOHeader(TString &name, TBranch *branch, TObject *obj=0)      : m_BranchName(name),
                                                                    m_Branch(branch),
                                                                    m_DataSet(obj){SetAddress();}    
  StIOHeader(TString &name, TTree *tree)                          : m_BranchName(name),m_DataSet(0)
  {
    if (!m_DataSet) m_DataSet = new St_DataSet;
    if (tree) m_Branch = tree->Branch(m_BranchName.Data(),m_DataSet->IsA()->GetName(),&m_DataSet, 4000,0); 
  }
  StIOHeader(TBranch *branch)                                     : m_BranchName(branch?branch->GetName():""),
                                                                    m_DataSet(0),m_Branch(branch){SetAddress();}
  StIOHeader(TBranch *branch,TString &name)                       : m_BranchName(name),
                                                                    m_DataSet(0),
                                                                    m_Branch(branch){SetAddress();}

        void     SetAddress();
        TObject *ShuntData() { TObject *obj = m_DataSet; m_DataSet = 0; return obj;}
        TBranch *GetBranch() const { return m_Branch;}
        Int_t    GetEvent(Int_t nevent=0) { return m_Branch ? m_Branch->GetEvent(nevent):0;}
        Int_t    Fill(TObject *obj) {
#ifndef tree
            if (m_Branch) { m_DataSet = obj; return m_Branch->Fill();}
#else
            if (m_Branch) { m_DataSet = obj; SetAddress(); }
#endif
            return 0;
        }
        void     SetData(TObject *data){ m_DataSet = data;}
  const Text_t  *GetName() const { return m_BranchName;}
};

//_____________________________________________________________________________
inline void StIOHeader::SetAddress(){ 
 if (m_Branch)  m_Branch->SetAddress(&m_DataSet); 
}

ClassImp(St_io_Maker)

//_____________________________________________________________________________
St_io_Maker::St_io_Maker(const char *name, const char *title,TTree *tree):StMaker(name,title){
   m_ListOfBranches = 0;
   SetTree(tree);
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_io_Maker::~St_io_Maker(){
  m_ListOfBranches->Delete();
  SafeDelete(m_ListOfBranches);
}
//_____________________________________________________________________________
void St_io_Maker::Add(const Char_t *dataName, const Char_t *fileName)
{
  // fileName for this "dataName"  (= "maker name" by default)
  // Check whether we have Maker("dataName");
  StMaker *maker =  gStChain->Maker(dataName);
  if (!maker) return;

  // Get tree
  TTree *tree = GetTree();
  if (!tree) tree = MakeTree(gStChain->GetName(),gStChain->GetTitle());
  if (!CreateBranchList()) return;

  // Create branch  
  Int_t buffersize = 4000;
  m_BranchName = dataName;
  m_BranchName += "_Branch";
  TBranch *b = 0;
#if 0
  b = tree->Branch(m_BranchName.Data(),m_DataSet->ClassName(), &m_DataSet, buffersize,0); 
#endif
  TString dataname(dataName);
  //  StIOHeader *o =  new StIOHeader(dataName,tree);
  StIOHeader *o =  new StIOHeader(dataname,tree);
  m_BranchName += ".root";
  b = o->GetBranch();
  if (fileName && strlen(fileName)) b->SetFile(fileName);
  else                              b->SetFile(m_BranchName);
  m_ListOfBranches->Add(o);
//  m_ListOfBranches->Add(new StIOHeader(dataName,b));
}

//_____________________________________________________________________________
void St_io_Maker::Add(TBranch *branch,const Char_t *,const Char_t *fileName)
//void St_io_Maker::Add(TBranch *branch,const Char_t *dataName,const Char_t *fileName)
{

// Check whether we have Maker("dataName");
//  StMaker *maker =  gStChain->Maker(dataName);
//  if (!maker) return;

  if (!branch)  return;
  if (!CreateBranchList()) return;
//  TString name = dataName;
//  name.ReplaceAll("_Branch","");
  StIOHeader *o =  new StIOHeader(branch);
  if (fileName && strlen(fileName)) branch->SetFile(fileName);
  m_ListOfBranches->Add(o);
}
 
//_____________________________________________________________________________
void St_io_Maker::Add(TString &dataName,const Char_t *fileName)
{
  Add(dataName.Data(),fileName);
}
//_____________________________________________________________________________
void St_io_Maker::Clear(Option_t *option="")
{
  TTree *tree = GetTree();
  if (tree) tree->AutoSave();
  StMaker::Clear();
}
//_____________________________________________________________________________
St_DataSet *St_io_Maker::DataSet(const Char_t *set) 
{
   if (!m_ListOfBranches) return 0;
   TIter next(m_ListOfBranches);
   StIOHeader *obj = 0;
   while(obj = (StIOHeader *)next())  
   {
      // Find St_DataSet pointer 
      TString name = obj->GetName();
      name.ReplaceAll("_Branch","");
      if (strcmp(name.Data(),set)==0) break;
   }

   if (obj && obj->GetEvent(g_Chain->Event())) 
         return (St_DataSet *)(obj->ShuntData());
   return  0;
}

//_____________________________________________________________________________
Int_t St_io_Maker::GetEvent(Int_t nevent)
{
  Int_t i = NextEventGet(nevent);
  printf(" =========== >>>>>>> %d bytes have been read\n",i);
  return i;
}
//_____________________________________________________________________________
Int_t St_io_Maker::Init(){

// Maker tree
   TTree *tree = GetTree();
   if (!tree) MakeTree(GetName(),GetTitle());

// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_io_Maker::Finish()
{
 if (strcmp(GetName(),"Output")==0) {
   TTree *tree = GetTree();
   if (tree) 
       SetActive();
       tree->Write();
 }
 return 0;
}
//_____________________________________________________________________________
Int_t St_io_Maker::Make()
{
  // Write out all datasets for all StMaker's
  if (strcmp(GetName(),"Output")==0)  {
     NextEventPut();
     return kStOK;
  }
  else 
       return  NextEventGet(g_Chain->Event()) ? kStOK : kStErr;

}
//_____________________________________________________________________________
Int_t St_io_Maker::NextEventGet(Int_t nevent)
{
 // - Prepares the list of branches to be read when called for the first time
 //
 //   + This method creates a list of the branches to be read and 
 //   + Changes the names of the branch file names.
 //     It assumes the branch files should be resided at the same directory as
 //     TTree file is
 //
 // - Reads next event.
 //
#ifdef tree
  TTree *tree = GetTree();
  if (!tree)   return 0;
#endif

  if (!m_ListOfBranches) {
#ifndef tree
    TTree *tree = GetTree();
    if (!tree)   return 0;
#endif
    // Let's create it from the TTree if any
    TBranch *nextb = 0;
    TObjArray *branches = tree->GetListOfBranches();
    if (!branches)       return 0;
    TIter next(branches);
    while (nextb = (TBranch *)next())  
    {
       const Char_t *treePathName   = gSystem->DirName(tree->GetCurrentFile()->GetName());
       const Char_t *branchFileName = gSystem->BaseName(nextb->GetFileName());
       Char_t *fileForThisBranch    = gSystem->ConcatFileName(treePathName,branchFileName);
       Add(nextb,"",fileForThisBranch);
       delete [] fileForThisBranch;
       printf(" St_io_Maker::NextEventGet ----> %s from %s \n", nextb->GetName(),nextb->GetFileName());
    }
  }
  Int_t counter = 0;
#ifdef tree
  if (SetActive()) 
  {
    counter = tree->GetEvent(nevent);
#else
  {
#endif
    if (m_ListOfBranches) 
    {
      counter = -1;
      TIter next(m_ListOfBranches);
      StIOHeader *obj = 0;
      while(obj = (StIOHeader *)next())  {
       // determinate the recepient
        TString name = obj->GetName();
        name.ReplaceAll("_Branch","");
        StMaker *maker = gStChain->Maker(name.Data());
        if (maker) {
#ifndef tree
           if (counter == -1) counter = 0;
           counter += obj->GetEvent(nevent);
#endif
           maker->SetDataSet((St_DataSet *)obj->ShuntData());
        }
      }
    }
  }
  return counter;
}

//_____________________________________________________________________________
Int_t St_io_Maker::NextEventPut()
{
#ifdef tree
  TTree *tree = GetTree();
  if (!tree) return 0;
#endif

  Int_t counter = 0;
  if (!m_ListOfBranches) return -1;
#ifdef tree
  if (SetActive()) 
#endif
  {
    TIter next(m_ListOfBranches);
    StIOHeader *obj = 0;
    while(obj = (StIOHeader *)next())  {
      // Collect data
      TString name = obj->GetName();
      name.ReplaceAll("_Branch","");
      St_DataSet *dataSet = gStChain->DataSet(name);
      // Fill branch buffer
#ifndef tree
      counter += obj->Fill(dataSet);
#else
      obj->Fill(dataSet);
#endif
    }
#ifdef tree
    counter = tree->Fill();
#endif
  }
  printf(" =========== >>>>>>> %d bytes have been written\n",counter);

  return counter;
}
//_____________________________________________________________________________
TTree *St_io_Maker::MakeTree(const char* name, const char*title)
{
//  Create a ROOT tree
//  Loop on all makers to create the Root branch (if any)

   if (m_Tree) return m_Tree;

   m_Tree = new TTree(name,title);
}
//_____________________________________________________________________________
void St_io_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_io_Maker.cxx,v 1.6 1999/02/24 16:06:35 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
Int_t St_io_Maker::SetActive()
{
  //  SetActive()
  //
  // Disactivate all branches of the current tree then 
  // Mark all branches of this maker as active ones
  //
  // Return: number of active branches if any
  //
  Int_t numberActive = 0;
  TTree *tree = GetTree();
  if (!tree) return numberActive;

  tree->SetBranchStatus("*",kFALSE);
  TIter next(m_ListOfBranches);
  StIOHeader *obj = 0;
  while(obj = (StIOHeader *)next())  
  {
    TString name = obj->GetName();
    name.ReplaceAll("_Branch","");
    StMaker *maker = g_Chain->Maker(name);
    if (maker) {
       tree->SetBranchStatus(name,kTRUE);
       numberActive++;
    }
  }
  return numberActive;
}

