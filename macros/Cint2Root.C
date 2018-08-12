void Cint2Root(TString topDir = ".") {
  // set loop optimization level 
  //  gROOT->ProcessLine(".O4"); 
  // gather all files from the same top directory into one chain
  // topDir must end with "/"
  TFileSet dirs(topDir);
  TDataSetIter next(&dirs,0);
  TDataSet *set = 0; 
  TFile *f = 0;
  Int_t k = 0;
  while ( (set = next()) ) {           
    TString name = set->GetName();
    TString title = set->GetTitle();
    TString path = set->Path();
    if (title != "file") continue;
    if (! name.EndsWith(".C")) continue;
    cout << set->Path() << "\t" <<name.Data() << endl;
  }
}
