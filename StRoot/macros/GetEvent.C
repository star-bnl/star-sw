 //*CMZ :          23/02/99  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@bnl.gov)   03/07/98
//
//  
//  
//
// Macro to test St_io_Maker capabilities
//
// It is a simple demo how one can read back the events stored with St_io_maker("Output")
//                                               Date: 18/01/98 BNL V.Fine
//
TBrowser *b = 0;
class StChain;
class St_DataSet;
StChain  *chain=0;
TFile *root_file=0;
TTree  *tree=0;
St_DataSet *set = 0;
void Load()
{
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_emc_Maker");
};

void GetEvent(Int_t numberOfEvent=4,
	      Int_t firstEventNumber=150,
	      const Char_t *RootFileName = "/disk00001/star/auau200//hijing135/jetq_on/b9_12/year_1b/hadronic_on/tfs/set0076_02_160evts.root",
	      const Char_t *testedMakerName = "dst")
{
  cout << "Usage:   \tGetEvent(Int_t numberOfEvent=\t"<<numberOfEvent<<"," << endl; 
  cout << "\t \t Int_t firstEventNumber=\t"<<firstEventNumber<<","<< endl;
  cout << "\t \t const Char_t *RootFileName = \t\""<<RootFileName<< "\","<< endl;
  cout << "\t \t const Char_t *testedMakerName = \t\""<<testedMakerName<<"\"); " << endl;
//=================  NAME to TEST ==================
//  const Char_t *testedMakerName = "geant";
//==================================================

  if (gClassTable->GetID("StChain") < 0) Load();
  if (!root_file && RootFileName) root_file  =  new TFile(RootFileName);

  if (!chain) {
    chain = new StChain("bfc");
    St_io_Maker *in    = new St_io_Maker("Input","all");
//  St_TLA_Maker   *geant = new St_TLA_Maker(testedMakerName,"event/geant/Event");
//  in->MakeDoc(); 

    if (root_file) tree=(TTree *)root_file->Get("Output");
  }
  if (tree) {
    tree->Print();
    chain->SetTree(tree);
    TObjArray *list = tree->GetListOfBranches();
     if (list) {
       TIter next(list);
       TBranch *nextb = 0;
       while (nextb = (TBranch *)next()) 
          cout << "Branch: <"<< nextb->GetName() << ">;"
               << "  File: <"<< nextb->GetFileName() << ">;"
               << " Entries: " << nextb->GetEntries()
               << "; Last event number: "<< nextb->GetEventNumber() << endl;    
     }
  }

  int iInit = chain->Init();
  chain->PrintInfo();
  for (Int_t i= firstEventNumber; i <  firstEventNumber+numberOfEvent; i++)
  {
    if (!chain->Make(i)) 
    {
      cout << "------------- Event #" << i << endl;
      set = chain->DataSet(testedMakerName);
      if (set) set->ls("*");
    }
    else {
     set = chain->DataSet(testedMakerName);
     if (set) set->ls("*");
     else
        cout << "------------- Wrong Event #" << i << endl;
    }
  }
  if (b) delete b;
  if (set) b = new TBrowser("DST", set);
}
