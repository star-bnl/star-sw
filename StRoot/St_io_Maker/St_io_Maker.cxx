//  
//  
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_io_Maker class for Makers                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_io_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TTree.h"

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
    if (tree) m_Branch = tree->Branch(m_BranchName.Data(),"St_DataSet", &m_DataSet, 4000,0); 
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
            if (m_Branch) { m_DataSet = obj; SetAddress(); return m_Branch->Fill();}
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
   SetTree(tree);
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_io_Maker::~St_io_Maker(){
  m_ListOfBranches->Delete();
  SafeDelete(m_ListOfBranches);
}
//_____________________________________________________________________________
void St_io_Maker::Add(const Char_t *dataName)
{

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
  StIOHeader *o =  new StIOHeader(dataName,tree);
  m_BranchName += ".root";
  b = o->GetBranch();
  b->SetFile(m_BranchName);
  m_ListOfBranches->Add(o);
//  m_ListOfBranches->Add(new StIOHeader(dataName,b));
}

//_____________________________________________________________________________
void St_io_Maker::Add(TBranch *branch,const Char_t *dataName)
{

// Check whether we have Maker("dataName");
//  StMaker *maker =  gStChain->Maker(dataName);
//  if (!maker) return;

  if (!branch)  return;
  if (!CreateBranchList()) return;
//  TString name = dataName;
//  name.ReplaceAll("_Branch","");
  StIOHeader *o =  new StIOHeader(branch);
  m_ListOfBranches->Add(o);
}

//_____________________________________________________________________________
void St_io_Maker::Add(TString &dataName){
  Add(dataName.Data());
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
Int_t St_io_Maker::Make(){
  // Write out all datasets for all StMaker's
  if (strcmp(GetName(),"Output")==0)  {
     NextEventPut();
     return kStOK;
  }
  else
     return NextEventGet(g_Chain->Event())? kStOK : kStErr;

}
//_____________________________________________________________________________
Int_t St_io_Maker::NextEventGet(Int_t nevent)
{
#ifdef tree
  TTree *tree = GetTree();
  if (!tree)   return 0;
#endif

  if (!m_ListOfBranches) {
#ifndef tree
    TTree *tree = GetTree();
    if (!tree)   return 0;
#endif
    // Let' s create it from the TTree if any
    TBranch *nextb = 0;
    TObjArray *branches = tree->GetListOfBranches();
    if (!branches)       return 0;
    TIter next(branches);
    while (nextb = (TBranch *)next())  { printf(" St_io_Maker::NextEventGet ----> %s \n", nextb->GetName()); Add(nextb);}
  }
  Int_t counter = 0;
#ifdef tree
  if (SetActive()) 
  {
    counter = tree->GetEvent(nevent);
#else
  {
#endif
    TIter next(m_ListOfBranches);
    StIOHeader *obj = 0;
    while(obj = (StIOHeader *)next())  {
     // determinate the recepient
      TString name = obj->GetName();
      name.ReplaceAll("_Branch","");
      StMaker *maker = gStChain->Maker(name);
      if (maker) {
#ifndef tree
          counter += obj->GetEvent(nevent);
#endif
          maker->SetDataSet((St_DataSet *)obj->ShuntData());
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
  printf("* $Id: St_io_Maker.cxx,v 1.2 1999/01/19 04:58:49 fine Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
Int_t St_io_Maker::SetActive()
{
  // Disactivate all branches of the currebt tree then 
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

