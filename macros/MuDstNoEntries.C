Long64_t MuDstNoEntries(const Char_t *muFile = "../daq_Cosmic/st_cosmic_adc_23018048_raw_0000003.MuDst.root") {
  Long64_t no = 0;
  TFile *f = new TFile(muFile);
  if (! f) return no;
  TTree *tree = (TTree *) f->Get("MuDst");
  if (! tree) return no;
  n = tree->GetEntriesFast();
  cout << muFile << " contains " << n << " events" << endl;
  return n;
}
