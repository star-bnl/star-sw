#include "TFile.h"
#include "TTree.h"
#include "St_DataSet.h"
#include "St_DataSetTree.h"

ClassImp(St_DataSetTree)

//______________________________________________________________________________
St_DataSetTree::St_DataSetTree(const char* filename, St_DataSet *dataset)
{
  if (!filename || strlen(filename) == 0) {
    Error("St_DataSetTree"," too few parameters to define Tree");
  }
//  fSet = dataset;
  fFile = new TFile(filename,"RECREATE");  
  fTree = 0;
}

//______________________________________________________________________________
St_DataSetTree::~St_DataSetTree(){
  Close();
  if (fSet) delete [] fSet;
  fSet = 0;
}
//______________________________________________________________________________
void St_DataSetTree::Close(){
  TFile *sav = 0;
  if (gFile != fFile) {
    TFile *sav = gFile;
    fFile->cd();
  }

  if (fFile) {
    fFile->Close();
    delete fFile;
    fFile = 0;
  }
  else if (fTree) 
     delete fTree;
  fTree = 0;
  if (sav) sav->cd();

}
//______________________________________________________________________________
void St_DataSetTree::MakeTree(St_DataSet *dataset)
{
  if (!dataset) return;
  // Make the ROOT TTree for the current St_DataSet object
  if (!fFile) return;
  TFile *sav = 0;
  if (!fTree)  {
    if (gFile != fFile) {
     sav = gFile;
     fFile->cd();
    }
    fTree =  new TTree(dataset->GetName(),dataset->GetTitle());
    TList *list = dataset->GetList();
    if (!list) return;
    TIter next(list);
    Int_t size = list->GetSize();
    printf(" Size = %d\n",size);
    fSet  = new St_DataSet*[size+1]; 

    St_DataSet *set = 0;
    Int_t i=0;

   // Make branch for the mother set
    set = dataset;
    fSet[i]=set;
    printf(" Name = %s; ClassName=%s\n",set->GetName(),set->ClassName());
    fTree->Branch(set->GetName(),set->ClassName(),&fSet[i]);
    i++;
    while (set = (St_DataSet *)next()) {
           fSet[i]=set;
           printf(" Name = %s; ClassName=%s\n",set->GetName(),set->ClassName());
           fTree->Branch(set->GetName(),set->ClassName(),&fSet[i]);
           i++;
    }
    printf(" %d branches have been prepared\n", i);
    if (sav) sav->cd();
  }
  else
     Error("MakeTree()","Can not create a second tree for the current iterator");      
}

//______________________________________________________________________________
void St_DataSetTree::FillTree(St_DataSet *dataset){
  if (!dataset) return;
  TFile *sav = 0;
  if (fTree) {
    if (gFile != fFile) {
      sav = gFile;
      fFile->cd();
    }
    TIter next(dataset->GetList());
    St_DataSet *set = dataset;
    Int_t i=0;
    fSet[i]=set;
    printf(" Name = %s; ClassName=%s\n",set->GetName(),set->ClassName());
//    fTree->SetBranchAddress(set->GetName(),&fSet[i]);
    i++; 
//    TList list;
    while (set = (St_DataSet *)next()) {
           fSet[i]=set;
           printf(" Name = %s, Title = %s;\n",set->GetName(),set->GetTitle());
           set->Dump();
//           set->SetParent(0);
//           list.Add(set);
//           fTree->SetBranchAddress(set->GetName(),&fSet[i]);
           i++;
    }
    printf(" %d branches have been written out \n", i);
    fTree->Fill();
    if (sav) sav->cd();
//    list.Delete();
  }
}
