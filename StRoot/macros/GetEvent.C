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
    gSystem->Load("global");
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
};

void GetEvent(const Char_t *testedMakerName = "geant",Int_t numberOfEvent=1,Int_t firstEventNumber=0) {
 cout << "\tUsage: GetEvent(const Char_t *testedMakerName = \"" << testedMakerName << "\""
                      << ", Int_t numberOfEvent=" << numberOfEvent
                      <<",Int_t firstEventNumber="<<firstEventNumber<< ")" 
      << endl << endl;
//=================  NAME to TEST ==================
//  const Char_t *testedMakerName = "geant";
//==================================================

  if (gClassTable->GetID("StChain") < 0) Load();

  const Char_t *FileOut = "/afs/rhic/star/data/samples/hijet-g2t.root";
  if (FileOut) root_out  =  new TFile(FileOut,"UPDATE");

  if (chain) delete chain;
  chain = new StChain("bfc");

  St_io_Maker *in    = new St_io_Maker("Input","all");
 //  St_TLA_Maker   *geant = new St_TLA_Maker(testedMakerName,"event/geant/Event");
 // St_TLA_Maker    *dst   = new St_TLA_Maker("dst","event/geant/Event");
   in->MakeDoc(); 

  TTree *tree = 0;
  if (root_out) tree=(TTree *)root_out->Get("Output");
  if (tree) {
    tree->Print();
    chain->SetTree(tree);
    TObjArray *list = tree->GetListOfBranches();
     if (list) {
       TIter next(list);
       TBranch *nextb = 0;
       while (nextb = (TBranch *)next()) 
          cout << "Branch: <"<< nextb->GetName() << ">"
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
      chain->DataSet(testedMakerName)->ls("*");
    }
    else {
     St_DataSet *set = chain->DataSet(testedMakerName);
     if (set) set->ls("*");
     else
        cout << "------------- Wrong Event #" << i << endl;
    }
  }
}
