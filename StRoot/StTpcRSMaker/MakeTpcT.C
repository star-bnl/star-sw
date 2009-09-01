void MakeTpcT(Double_t tanCut = 0.1, Int_t NpadCut = 3, Double_t pMomin = 0.4, Double_t pMomax = 1.0) {
  gInterpreter->ProcessLine(".L ~/public/macros/TpcT.C+");
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter  next(files);
  TFile *fin = 0;
  Int_t i = 0;
  TChain Chain("TpcT");
  TString Fout("");
  TTree *TpcTree = 0;
  TFile *fout = 0;
  while ((fin = (TFile *) next())) {
    cout << "Source file " << i++ << ": " << fin->GetName();
    TpcTree = (TTree *) fin->Get("TpcT");
    Fout = fin->GetName();
    if (TpcTree) {
      Chain.Add(fin->GetName(),-1);
      cout << "\t has been added as " << Fout;
    } else {
      if (Fout.EndsWith("_Plot.root")) {
	fin->Close();
	fout = new TFile(Fout.Data(),"update");
	cout << "\t has been set " << Fout << " for update";
      }
    }
    cout << endl;
  }
  if (Fout != "") {
    Fout.ReplaceAll(".root",Form("Tan%f_Npad_%i_pT%3.1f_%3.1f_PlotS.root",tanCut,NpadCut,pMomin, pMomax));
    if (! fout) fout = new TFile(Fout.Data(),"recreate");
    TpcT t(&Chain);
    t.Loop(-1,tanCut, NpadCut,pMomin, pMomax);
    fout->Write();
    delete fout;
  }
}
