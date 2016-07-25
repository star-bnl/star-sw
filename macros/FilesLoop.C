void FilesLoop(const Char_t *files = "Must*.root", const Char_t *histN = "dZ") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TFile *f = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    NFiles++;
    if (f) delete f;
    f = new TFile(file);
    if (! f) continue;
    TH2F *dZ = (TH2F *) f->Get(histN);
    if (! dZ) continue;
    TH1D *proj = dZ->ProjectionY();
    proj->Fit("gaus","q");
    TF1 *gaus = proj->GetFunction("gaus");
    if (! gaus) continue;
    cout << file << Form("\t%8.3f +/- %8.3f",100*gaus->GetParameter(1), 100*gaus->GetParError(1)) << endl;
  }
}
