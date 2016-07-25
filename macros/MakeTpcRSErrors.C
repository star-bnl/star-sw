void MakeTpcRSErrors() {
  gROOT->LoadMacro("TpcT.C+");
  //  const Char_t *Files[2] = {"SpX","SpZ"};  
  const Char_t *Files[2] = {"FitP","FitP"};
  for (Int_t iXZ = 1; iXZ < 2; iXZ++) {
    //     TString file(Files[iXZ]);
    //     file += "2010.root";
    //     TFile *f = new TFile(file);
    //     if (! f) continue;
    for (Int_t sec = 0; sec < 2; sec++) {
      for (Int_t row = 0; row < 2; row++) {
	//	for (Int_t N = 0; N < 22; N++) {
	for (Int_t MS = 0; MS < 2; MS++) {
	  for (Int_t prompt = 0; prompt < 2; prompt++) {
	    //	      MDFerrorParameterization(Files[i],sec,row,N,MS,prompt);
	    MDFerrorParameterization2("FitP",iXZ,sec,row,MS,prompt);
	  }
	}
	//	}
      }
    }
  }
}
