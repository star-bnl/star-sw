void chdir(const Char_t *dir = "") {
  TString Dir(dir);
  Dir += ".root";
  if (Dir == TString(gSystem->BaseName(gDirectory->GetName()))) {cout << gDirectory->GetName() << endl; return;}
  TList *files = gROOT->GetListOfFiles();
  if (! files) {cout << "No root files " << endl; return;}
  TIter  next(files);
  TFile *f = 0;  
  while ((f = (TFile *) next())) {
    if (Dir == TString(gSystem->BaseName(f->GetName()))) 
      {f->cd(); cout << "cd " << gDirectory->GetName() << endl; return;}
    //    cout << gSystem->BaseName(f->GetName()) << " does not match with requested " << Dir << endl;
  }
}
//________________________________________________________________________________
void rdir() {
  TList *files = gROOT->GetListOfFiles();
  if (! files) {cout << "No root files " << endl; return;}
  TIter  next(files);
  TFile *f = 0;  
  while ((f = (TFile *) next())) {cout << "dir \t" << gSystem->BaseName(f->GetName())  << endl;}
}
//________________________________________________________________________________
void rpwd() {
  cout << "dir \t" << gDirectory->GetName()  << endl;
  //  cout << "dir \t" << gSystem->BaseName(gDirectory->GetName())  << endl;
}
