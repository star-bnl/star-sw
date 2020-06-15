/* 
   cd StarDb
   root.exe 'bfc.C(-1)' Cint2Root.C
*/
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
    TString rootf(path);
    rootf.ReplaceAll(".C",".root");
#if 1
    if (!gSystem->AccessPathName(rootf,kReadPermission)) {
      //      cout << path.Data() << "\t" << rootf.Data() << " already exists" << endl;
      continue;
    }
#endif
    cout << path.Data() << "\tCreate " << rootf.Data() << endl;
    cout << "LoadTable:" << path.Data() << endl;
    TString command(".L "); command += path;
    TInterpreter::EErrorCode ee;
    gInterpreter->ProcessLine(command,&ee);
    assert(!ee);
    TDataSet *newdat = (TDataSet *) gInterpreter->Calc("CreateTable()",&ee);
    assert(!ee);
    command.ReplaceAll(".L ",".U ");
    gInterpreter->ProcessLine(command,&ee);
    assert(!ee);
    if (! newdat) {
      cout << "\tFail to make TDataSet from " << path.Data() << endl;
      continue;
    }
    TFile *f = new TFile(rootf,"recreate");
    newdat->Write();
    delete f;
    delete newdat;
    delete gGeoManager;
    gGeoManager = 0;
  }
}
