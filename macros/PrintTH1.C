void PrintTH1(const TH1 *hist=0) {
  if (! hist) return;
  Int_t nx = hist->GetNbinsX();
  cout << "\tDouble_t corr[" << nx << "] = {";
  for (Int_t ix = 1; ix <= nx; ix++) {
    Double_t cont = hist->GetBinContent(ix);
    cout << Form("%7.4f",cont);
    if (ix != nx) cout << ",";
    //    if (ix == 12) cout << endl << "\t";
  }
  cout << "};" << endl;
}
