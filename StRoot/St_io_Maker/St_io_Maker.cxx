//  
//  
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_io_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_io_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TTree.h"

class StBranchObject : public TObject 
{
 private:
    TString    m_BranchName;
    TObject   *m_DataSet;
    TBranch   *m_Branch; 

 public:
  StBranchObject(const Char_t *name, TBranch *branch, TObject *obj=0) : m_BranchName(name),m_Branch(branch),m_DataSet(obj){SetAddress();}
  StBranchObject(TString &name, TBranch *branch, TObject *obj=0) : m_BranchName(name),m_Branch(branch),m_DataSet(obj){SetAddress();}    
  StBranchObject(TString &name, TTree *tree) : m_BranchName(name),m_DataSet(0){
    if (tree) m_Branch = tree->Branch(m_BranchName.Data(),"St_DataSet", &m_DataSet, 4000,0); 
  }
  StBranchObject(TBranch *branch) : m_BranchName(branch?branch->GetName():""),m_DataSet(0),m_Branch(branch){SetAddress();}
  StBranchObject(TBranch *branch,TString &name) : m_BranchName(name),m_DataSet(0),m_Branch(branch){SetAddress();}

        void     SetAddress();
        TObject *ShuntData() { TObject *obj = m_DataSet; m_DataSet = 0; return obj;}
        TBranch *GetBranch() const { return m_Branch;}
        Int_t    GetEvent(Int_t nevent=0) { return m_Branch ? m_Branch->GetEvent(nevent):0;}
        Int_t    Fill(TObject *obj) {
            m_DataSet = obj; if (m_Branch) {SetAddress(); return m_Branch->Fill();}
            return 0;
        }
        void     SetData(TObject *data){ m_DataSet = data;}
  const Text_t  *GetName() const { return m_BranchName;}
};


//_____________________________________________________________________________
inline void StBranchObject::SetAddress(){ 
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
  StBranchObject *o =  new StBranchObject(dataName,tree);
  m_BranchName += ".root";
  b = o->GetBranch();
  b->SetFile(m_BranchName);
  m_ListOfBranches->Add(o);
//  m_ListOfBranches->Add(new StBranchObject(dataName,b));
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
  StBranchObject *o =  new StBranchObject(branch);
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
 TTree *tree = GetTree();
 if (tree) tree->Write();
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
  if (!m_ListOfBranches) {
    // Let' s create it from the TTree of any
    TTree *tree = GetTree();
    if (!tree)           return 0;
    TBranch *nextb = 0;
    TObjArray *branches = tree->GetListOfBranches();
    if (!branches)       return 0;
    TIter next(branches);
    while (nextb = (TBranch *)next())  Add(nextb);
  }
  TIter next(m_ListOfBranches);
  StBranchObject *obj = 0;
  Int_t counter = 0;
  while(obj = (StBranchObject *)next())  {
   // determinate the recepient
    TString name = obj->GetName();
    name.ReplaceAll("_Branch","");
    StMaker *maker = gStChain->Maker(name);
    if (maker) {
     // GetEvent
        counter += obj->GetEvent(nevent);
        maker->SetDataSet((St_DataSet *)obj->ShuntData());
    }
  }
  return counter;
}

//_____________________________________________________________________________
Int_t St_io_Maker::NextEventPut()
{
  if (!m_ListOfBranches) return -1;
  TIter next(m_ListOfBranches);
  StBranchObject *obj = 0;
  Int_t counter = 0;
  while(obj = (StBranchObject *)next())  {
    // Collect data
    TString name = obj->GetName();
    name.ReplaceAll("_Branch","");
    St_DataSet *dataSet = gStChain->DataSet(name);
    // Fill branch buffer
    counter += obj->Fill(dataSet);
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
  printf("* $Id: St_io_Maker.cxx,v 1.1 1999/01/16 01:01:51 fine Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

