{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("global");
  gSystem->Load("St_global");
  gSystem->Load("St_dst_Maker");
  const Char_t *FileOut = "clone.root";
  TFile       *root = new TFile(FileOut);
  StChain *chain = new StChain("bfc");
 St_dst_Maker        *dst_maker = new St_dst_Maker("dst","event/data/global/dst");
//  int iInit = chain.Init();
//  if (iInit) chain.Fatal(iInit,"on init");
//  chain.Make(1); 
//  chain.FillClone();
cout << "Make" << endl;

  TTree *dst_tree = (TTree *) root->Get("dst_Tree");
  Int_t Nevents=dst_tree->GetEntries();
cout << "Nev = " << Nevents << endl;
//  TBranch *dst_branch = dst_tree->GetBranch("dst");
  chain->SetTree(dst_tree);
//  StMaker *makers[10];
//  makers[0]=0;
//  TObjArray *list = dst_branch->GetListOfBranches();
//  TIter next(list);
//  TBranch *br =0;
//  Int_t i =0 ;
//  void *addr[40];
//while (br = (TBranch *) next()){
//  cout << br->GetName() << endl;
//  br->SetAddress(&addr[i++]);
// }
//  dst_branch->SetAddress(&makers[0]);
  chain->SetBranches();
  dst_tree->GetEvent(1);
}

						  
