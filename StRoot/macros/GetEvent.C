//  
//  
//
// Macro to test St_io_Maker capabilities
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
TFile *root_out=0;

void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("St_xdfin_Maker");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_TLA_Maker");
};

void GetEvent() {
//=================  NAME to TEST ==================
  const Char_t *testedMakerName = "geant";
//==================================================

  if (gClassTable->GetID("StChain") < 0) Load();

  const Char_t *FileOut = "/afs/rhic/star/data/samples/hijet-g2t.root";
  if (FileOut) root_out  =  new TFile(FileOut,"UPDATE");

  if (chain) delete chain;
  chain = new StChain("bfc");

  St_io_Maker *in    = new St_io_Maker("Input","all");
  St_TLA_Maker    *geant = new St_TLA_Maker(testedMakerName,"event/geant/Event");
//  in->MakeDoc();

  TTree *tree = 0;
  if (root_out) tree=(TTree *)root_out->Get("Output");
  if (tree) {
    chain->SetTree(tree);
    TObjArray *list = tree->GetListOfBranches();
     if (list) {
       TIter next(list);
       TBranch *nextb = 0;
       while (nextb = (TBranch *)next()) cout << "Branch: "<< nextb->GetName() << endl;    
     }
  }

  int iInit = chain->Init();
  chain->PrintInfo();
  chain->Make(0);
  chain->DataSet(testedMakerName)->ls("*");
}
