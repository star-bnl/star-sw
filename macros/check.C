void check(TString topDir = "/star/rcf/scratch/fisyak/Hist205/") {
  gSystem->Load("libStar"); 
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
    if (strcmp(set->GetTitle(),"file") || 
        !(strstr(set->GetName(),".root"))) continue;
    if (
	strstr(set->GetName(),"2237040_0170") ||
	strstr(set->GetName(),"2241019_0050") ||
	strstr(set->GetName(),"2241019_0060") 
	) continue;
    TString name(topDir); 
    name += set->GetName(); 
    k++;
    cout << "Open file: " << k << " " << name.Data();
#if 1
    f = new TFile(name.Data());// cout << "f" << f ;
    if (f) {
      TProfile2D *z = (TProfile2D *)f->Get("Z");
      if (z) {
	Int_t nx = z->GetNbinsX();
	Int_t ny = z->GetNbinsY();
	Int_t iok = 1;
	for (int i = 1; i <= nx; i++) 
	  for (int j = 1; j <= ny; j++) 
	    if (! TMath::Finite(z->GetBinContent(i,j))) {
	      cout << " =========== Broken file: i/j = " << i << "/" << j << endl; iok = 0; goto ENDL;
	    }
      ENDL:
	if (iok) cout << "Bin Cont[5,15]:" << z->GetBinContent(5,15) << endl;
      }
      else { cout << " no Z" << endl;}
    }
    else { cout << " Empty " << endl;}
    delete f;
#endif
  }
}
