void PrintTH1(const TH1 *hist=0) {
  if (! hist) return;
  Int_t nx = hist->GetNbinsX();
#ifndef __SUMMARY_ONLY__
  cout << "\tDouble_t corr[" << nx << "] = {";
  for (Int_t ix = 1; ix <= nx; ix++) {
    Double_t cont = hist->GetBinContent(ix);
    cout << Form("%7.4f",cont);
    if (ix != nx) cout << ",";
    //    if (ix == 12) cout << endl << "\t";
  }
  cout << "};" << endl;
#endif
  if (nx == 24) {
    for (Int_t we = 0; we <2 ; we++) {
      hist->Fit("pol0","erq","",12*we+0.5,12*(we+1)+0.5); 
      TF1 *pol0 = hist->GetListOfFunctions()->FindObject("pol0");
      if (pol0) {
	if (we == 0) cout << "\t" << hist->GetName() << "\tWest: ";
	else         cout << "\tEast: ";
	cout << Form("  %6.4f +/- %6.4f",pol0->GetParameter(0), pol0->GetParError(0));
      }
    }
    cout << endl;
  }
}
