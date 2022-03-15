/*
  root.exe TdEdxNB.C 
...
  Bichsel_He3_14_1_100O.root entries 13
...
 */
void TdEdxNB() {
  TDirIter Dir("*.root");
  Char_t *file = 0;
  Int_t NFiles = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    TFile *f = new TFile(file);
    if (! f) continue;
    TH2F *TdEdxN = (TH2F *) f->Get("TdEdxN");
    if ( TdEdxN ) {
      TdEdxN->GetXaxis()->SetRange(90,120);
      TdEdxN->GetYaxis()->SetRange(500,550);
      Double_t entries = TdEdxN->ProjectionY()->GetEntries();
      if (entries > 1) {
	cout << f->GetName() << " entries " << entries << endl;
      }
    }
    delete f;
  }
}
