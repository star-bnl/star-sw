{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("global");
  gSystem->Load("St_global");
  gSystem->Load("St_dst_Maker");
  const Char_t *FileOut = "clone.root";
  TFile       *root = new TFile(FileOut);
  TTree *dst_tree = (TTree *) root->Get("dst_Tree");
  TObjArray *listOfBranches = dst_tree->GetListOfBranches();
   // print the full list of branches
  if (listOfBranches) {
      TIter next(listOfBranches);
      TBranch *nextbranch = 0;
      cout << "Size of the branches list is " << listOfBranches->GetSize() << endl;
      while (nextbranch = (TBranch *)next())  cout << " Branch: " << nextbranch->GetName() << endl;
      
  }
  else {
    cout << "No branches found " << endl;
  }
 
    
  Int_t Nevents=dst_tree->GetEntries();
  cout << "Nevents = " << Nevents << endl;
  TBranch *dst_branch = dst_tree->GetBranch("dst_Branch");
  if (!dst_branch) 
      cout << "No \"dst_Branch\" found " << endl;
  StChain chain("bfc");
  St_dst_Maker        *dst_maker = new St_dst_Maker("rdst","event/data/global/rdst");
  int iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");
  
// st_maker);
  dst_branch->SetAddress(0);
  dst_branch->GetEvent(1);
}

						  
