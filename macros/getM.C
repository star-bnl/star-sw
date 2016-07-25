Int_t getM(TH1 *Time_pfx){
  //  Int_t ix = Time_pfx->GetMaximumBin();
  Int_t ix = Time_pfx->GetMinimumBin();
  UInt_t i = Time_pfx->GetBinLowEdge(ix);
  TDatime t;
  t.Set(i);
  Char_t *date = t.AsString();
  cout << "min/max Value :" << Time_pfx->GetBinContent(ix)
       << " at bin/low x: " << ix << "/" << i
       << " Time: " << date << endl;
  return ix;
}
