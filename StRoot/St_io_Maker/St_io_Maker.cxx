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
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "St_io_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TSystem.h"
#include "TObjString.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClass.h"

class StIOHeader : public TObject 
{
 private:
    TString     m_BranchName;
    TObject    *m_DataSet;
    TBranch    *m_Branch; 

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
St_io_Maker::St_io_Maker(const char *name, const char *title,Bool_t split,TTree *tree)
 :StIOInterFace(name), fSplit(split)
{
   SetTitle(title);
   m_ListOfBranches = 0;
   SetTree(tree);
   m_FileIterator = 0;
   m_ListOfFiles  = 0;
   m_TreeRootFile = 0;
   m_OffSet       = 0;       // Event offset for multi-volumes tree's
   m_Entries      = 0;       // Number of the events of the current tree.
   m_Tree         = 0;
   SetMaxEvent();
}
//_____________________________________________________________________________
void St_io_Maker::Destructor()
{
 
  if (m_FileIterator) { 
    delete m_FileIterator;
    m_FileIterator = 0;
  }

  if (m_ListOfFiles) { 
    m_ListOfFiles->Delete();
    delete m_ListOfFiles;
    m_ListOfFiles = 0;
  }

  DestroyBranchList();
  if (m_TreeRootFile) {
    if (m_Tree) { 
       m_Tree = 0; 
//??????       fgStChain->SetTree(m_Tree);
    }
    delete m_TreeRootFile;
    m_TreeRootFile = 0;
  }
   m_OffSet       = 0;       // Event offset for multi-volumes tree's
   m_Entries      = 0;       // Number of the events of the current tree.
   m_Tree         = 0;
   SetMaxEvent();
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
  if (fSplit) {
    if (fileName && strlen(fileName)) b->SetFile(fileName);
    else                              b->SetFile(m_BranchName);
  }
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
void St_io_Maker::AddFile(const Char_t *fileName)
{
  // Create  the list of the ROOT file with TTree object to read from
  if (fileName && strlen(fileName)) {
    if (!m_ListOfFiles) m_ListOfFiles = new TObjArray;
    if(m_ListOfFiles->FindObject(fileName)) return; // file is already here
    TObjString *obj = new TObjString(fileName);
    cout << " File:" << obj->String() << endl;
    m_ListOfFiles->Add(new TObjString(fileName));
  }
}
//_____________________________________________________________________________
Int_t St_io_Maker::AddFilesFromFile(const Char_t *fileName)
{
  //
  // Create the list of input files supplied from the plain text file.
  // Format: one line == one file name (using env variable is allowed)
  //
  // Return: the number of the files available.
  // 
  char *expandedFileName = gSystem->ExpandPathName(fileName);
  Int_t fileCounter = 0;
  if (expandedFileName) {
    if (gSystem->AccessPathName(expandedFileName) == kFALSE) {
       FILE *inputList = fopen(expandedFileName,"r");
       if (inputList) {
         while (feof(inputList)){
           Char_t file[512];
           if(fgets(file,512,inputList)) { AddFile(file); fileCounter++; }
           else perror("St_io_Maker::AddFilesFromFile");
         }
       }     
    }
    delete [] expandedFileName;
  }
  return fileCounter;
  
}
//_____________________________________________________________________________
void St_io_Maker::Clear(Option_t *)
{
  TTree *tree = GetTree();
  if (tree) {
    TFile *file =  tree->GetCurrentFile();
    if (file && file->IsWritable()) {
      tree->AutoSave();
      file->Flush();     
      // Flush all separate file
      if (fSplit) {
        TBranch *nextb = 0;
        TObjArray *branches = tree->GetListOfBranches();
        if (branches) {
          TIter next(branches);
          while (nextb = (TBranch *)next())  
                 if(nextb->GetFile()) nextb->GetFile()->Flush();
        }
      }
    }
  }
  StMaker::Clear();
}
//_____________________________________________________________________________
void St_io_Maker::BuildBranchList(TTree *tree)
{
  if (!tree) return;

  TBranch *nextb = 0;
  TObjArray *branches = tree->GetListOfBranches();
  if (!branches)   return;
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
//_____________________________________________________________________________
static Int_t GetEntries(StIOHeader *obj)
{
  // Returns the number of entries from the selected TBranch
  if (obj) {
    TBranch *b = obj->GetBranch();
    if (b) return b->GetEntries();
  }
  return 0;
}

//_____________________________________________________________________________
Bool_t St_io_Maker::IsNewTree(Int_t nevent)
{
  // Returns the condition whether the new TTree file should be opened
   return  ( m_Entries != -1)
         &&  m_ListOfFiles 
         && ( nevent-m_OffSet == TMath::Min(GetMaxEvent(),m_Entries) ) ;
}

//_____________________________________________________________________________
void St_io_Maker::DestroyBranchList()
{
  //
  // Destroy the list of the current TTree branches
  // (to prepare reading the next TTree or from this class dtor)

  if (m_ListOfBranches) {
      m_ListOfBranches->Delete();
      delete m_ListOfBranches;
      m_ListOfBranches = 0;
  }
}

//_____________________________________________________________________________
Int_t St_io_Maker::GetEvent(Int_t nevent)
{
  // returns the nuber of the bytes been read from the "nevent" event
  Int_t i = NextEventGet(nevent);
  printf(" =========== >>>>>>> %d bytes have been read\n",i);
  return i;
}
//_____________________________________________________________________________
Int_t St_io_Maker::Init(){

// Maker tree
   TTree *tree = GetTree();
   if (!tree) MakeTree(GetName(),GetTitle());

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
 else { // delete this list to be able to read the next file
   SafeDelete(m_ListOfBranches)
 }

 Destructor();
 return 0;
}
//_____________________________________________________________________________
TTree *St_io_Maker::GetTree(){
//  return m_Tree ? m_Tree : fgStChain->Tree();
  return m_Tree;
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
       return  NextEventGet(GetNumber()) ? kStOK : kStErr;

}
//_____________________________________________________________________________
Int_t St_io_Maker::NextEventGet(Int_t nEvent)
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

  Int_t nevent = nEvent-1;
  if (Debug()) cout << "NextEventGet:  " << nevent << " : " << m_OffSet << " : " << m_Entries << endl;

  if (  IsNewTree( nevent ) ) DestroyBranchList();

  if (!m_ListOfBranches) {
    // Let's create it from the TTree if any
    BuildBranchList(SetNextTree());
    TTree *tree = GetTree();
    if (!tree) return 0;
  }
  Int_t counter = 0;
  if (m_ListOfBranches)  {
     counter = -1;
     TIter next(m_ListOfBranches);
     StIOHeader *obj = 0;
     while(obj = (StIOHeader *)next())  {
//VP       // determinate the recepient
//VP        TString name = obj->GetName();
//VP        name.ReplaceAll("_Branch","");
//VP        StMaker *maker = gStChain->Maker(name.Data());
//VP        if (maker) {
           if (counter == -1) counter = 0;
           if (m_Entries == -1) m_Entries = GetEntries(obj);
           counter += obj->GetEvent(nevent-m_OffSet);
//VP           maker->SetDataSet((St_DataSet *)obj->ShuntData());
//VP        }

     St_DataSet *getDs,*psPar,*par,*son=0;
     getDs = (St_DataSet *)obj->ShuntData();
//		Fix of very weird BUG in written structure
     if (getDs) {
       son = getDs->First();
       if (son){
         psPar = son->GetParent();
         if (psPar!=getDs) {
         TObject ** to=  (TObject **)((int*)getDs+8);
         if (*to && strcmp((*to)->GetName(),"TList")==0){ *to=0; delete getDs;}
         getDs=psPar;}}
      
         AddData(getDs);     
      }
    }
  }
  return counter;
}

//_____________________________________________________________________________
Int_t St_io_Maker::NextEventPut()
{
  // Write next event out 
  // returns the number of the bytes written

  Int_t counter = 0;
  if (!m_ListOfBranches) return -1;
  {
    TIter next(m_ListOfBranches);
    StIOHeader *obj = 0;
    while(obj = (StIOHeader *)next())  {
      // Collect data
      TString name = obj->GetName();
      name.ReplaceAll("_Branch","");
      St_DataSet *dataSet = GetDataSet(name);
      // Fill branch buffer
      counter += obj->Fill(dataSet);
    }
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
   return m_Tree;
}
//_____________________________________________________________________________

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
    StMaker *maker = fgStChain->Maker(name);
    if (maker) {
       tree->SetBranchStatus(name,kTRUE);
       numberActive++;
    }
  }
  return numberActive;
}

//_____________________________________________________________________________
TTree *St_io_Maker::SetNextTree()
{
  // Switches this maker to readin next TFile if any.
  if (m_ListOfFiles) 
  {
    Bool_t treeFound = kFALSE;
    if (!m_FileIterator) m_FileIterator = new TIter(m_ListOfFiles);
    while (!treeFound) {
      treeFound = kTRUE;
      TObjString *s = 0;
      if (m_TreeRootFile) {
        // destroy the list of the branches of this file if any
        DestroyBranchList();
        delete m_TreeRootFile;
        m_TreeRootFile = 0;
      }
      m_Tree = 0; // This object is deleted by deleting the ROOT file above
      while( !m_TreeRootFile && (s = (TObjString *)m_FileIterator->Next()) )
      {
         const Char_t *fileName = s->String();
         cout << "Opening next root file :" << fileName << endl;
         m_TreeRootFile = new TFile(fileName);
         if (m_TreeRootFile->IsZombie()) { 
           SafeDelete(m_TreeRootFile); 
           cout << " BAD file: " << fileName << endl;
         }
       }
       if (m_TreeRootFile) {
         m_Tree =  (TTree *) m_TreeRootFile->Get("Output");
         if (m_Tree) {
            m_Tree->Print();
            // Calclulate next and current offset
            m_OffSet += TMath::Min(m_Entries,GetMaxEvent());
            m_Entries = -1;         
         }
         else {
            cout << "there is no tree in this file " << endl;
            // Try the next file if any
            treeFound = kFALSE;
         }
       }
     }
//====     fgStChain->SetTree(m_Tree);
  }  
  return GetTree();
}
