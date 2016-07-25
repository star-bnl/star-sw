void PrintDetectors(const Char_t *vers="y2006") {
  gSystem->Load("libsim_Tables.so");
  TString file("$STAR/StarDb/VmcGeometry/Detectors.");
  file += vers;
  file += ".root";
  TFile *f = new TFile(file);
  if (! f) {cout << "Can't open file " << file << endl; return;}
  TDataSet *set = (TDataSet *) f->Get("Detectors");
  if (! set) {cout << "Can't find Detectors in file " << file << endl; return;}
  TDataSetIter next(set,99);
  TDataSet *d = 0;
  Int_t k = 0;
  while ((d = next())) {
    if (TString(d->GetName()) != "Path") continue;
    St_det_path *table = (St_det_path *) d;
    Int_t N = table->GetNRows();
    det_path_st *path = table->GetTable();
    k++;
    cout << k << "\t";
    for (Int_t i = 0; i < N; i++, path++) {
      cout << path->VName << "[" << path->Ncopy << "]";
      if (i != N-1) cout << "/";
    }
    cout << endl;
  }
}
