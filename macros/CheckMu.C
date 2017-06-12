/* 
   root.exe -q -b  CheckMu.C | tee CheckMu.log
 */
//________________________________________________________________________________
void CheckMu(const Char_t *files="*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  TFile *f = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    cout << "new file = " << file << endl;
    f = new TFile(file);
    if (! f) {delete f; continue;}
    TString name(f->GetName());
    TH1 *mu = f->Get("mu");
    if (! mu) {
      cout << name.Data() << "\tdoes not have mu" << endl;
    } else if (mu->GetEntries() <= 0.0) {
      cout << name.Data() << "\thas empty mu" << endl;
    } else {
      cout << name.Data() << "\thas " << mu->GetEntries() << " entries" << endl;
    }
    delete f;
  }
}
