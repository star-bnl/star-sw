void NoPV(Char_t *files = "*/*.root") {
  TDirIter Dir(files);
  //  TTreeIter iter(TreeName);
  //  iter.AddFile(files);
  TFile *f = 0;
  Char_t *file = 0;
  Int_t NFiles = 0;
  while ( (file = (Char_t *) Dir.NextFile()) ) {   
    f = new TFile(file);
    TH3F *PVxyz= (TH3F *) f->Get("PVxyz");
    cout << "#\t" << NFiles << "\t" << f->GetName() << "\t";
    if (! PVxyz) {
      cout << " no PVxyz" << endl;
    } else {
      Double_t entries = PVxyz->GetEntries();
      cout << " PVxyz has " << entries <<endl;
    }
    NFiles++;
    delete f;
  }
}


