void getHist(const Char_t *file= "st_physics_4041002_raw_0020001.hist.root", 
	     const Char_t *maker = "tpc_hit_mover") {
  gROOT->LoadMacro("bfc.C");
  bfc(1,"in,NoDefault",file);
  TDataSet *HistSet = chain->DataSet("hist");
  if (! HistSet) return;
  TString hBN(maker);
  hBN += "Hist";
  TDataSet *QAH = HistSet->Find(hBN.Data());
  if (! QAH) return;
  TList *dList =  (TList *)QAH->GetObject();
  if (! dList) return;
  if (dList) cout << " FindHists - found hist. in histBranch, with name:  " << maker <<  endl;
  TIter next(dList);
  TH1 *hist = 0;
  while (hist = (TH1*) next()) {
    cout << "Got " << hist->GetName() << "\t" << hist->GetTitle() << endl;
  }
}
