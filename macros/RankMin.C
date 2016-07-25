Double_t RankMin() {
  Double_t rank = -1;
  const Char_t *histNames[3] = {"RankGood","RankBad","RankBadT"};
  TH1D *hist[3];
  Double_t *dint[3];
  Int_t nx = 0;
  Int_t i1 = 0;
  Int_t i2;
  Int_t icut = -1;
  for (Int_t h = 0; h < 3; h++) {
    hist[h] = (TH1D*) gDirectory->Get(histNames[h]);
    if (h <2 && ! hist[h]) return rank;
    if (! h) {
      nx = hist[h]->GetNbinsX();
      i2 = nx+1;
      icut = hist[h]->GetXaxis()->FindBin(rank);
    }
    dint[h] = hist[h]->GetIntegral();
    cout << Form("%-20s",histNames[h]) << " Total no. of entries " << hist[h]->Integral(i1,i2) << endl;
  }
  // Find icut from condition that backgound < 1% from signal
  while(1) {
    Double_t b = hist[1]->Integral(icut,i2);
    Double_t s = hist[0]->Integral(icut,i2);
    if (s <= 0) break;
    Double_t r = b/s;
    cout << icut << " b = " << b << " s = " << s << " r = " << r << endl;
    if (r < 0.01) break;
    icut++;
    if (icut > nx) break;
  }
  rank = hist[0]->GetXaxis()->GetBinUpEdge(icut);
  Double_t GT = hist[0]->Integral();
  Double_t GC = hist[0]->Integral(icut,i2);
  Double_t BC = hist[1]->Integral(icut,i2);
  Double_t BT = hist[2]->Integral(icut,i2);
  cout << "At rank min = " << rank << " Triggered Vertex Efficiency " << GC/GT << " Good Vertices "
       << "with background from pile-up " << BC/GC << " and the triggered bunch crossing pile-up " << BT/GC << endl;
  return rank;
}
