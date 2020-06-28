void kfpCount(const Char_t *files = "/gpfs01/star/subsys-tpc/fisyak/kfp/*/*/*dEdx*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  //  TTreeIter iter;
  Double_t total = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    //    iter.AddFile(file); 
    NFiles++;
    TFile *f = new TFile(file);
    if (! f) continue;
    TH1F *z = (TH1F *) f->Get("/Particles/KFParticlesFinder/PrimaryVertexQA/z");
    if (z) {
      Double_t nevents = z->GetEntries()/1e6;
      total += nevents;
      cout << Form("%-100s %6.2fM",f->GetName(),nevents) << endl;
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files and total = " << total << "M" << endl; 
  if (! NFiles) return;
}
