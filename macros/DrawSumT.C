TCanvas *c1 = 0;
void DrawSumT(const Char_t *var="run") {
  TNtuple *SumT = (TNtuple *) gDirectory->Get("SumT");
  if (! SumT) return;
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->Clear();
  c1->Divide(1,2);
  const Char_t *plot[2] = {"SecRow3CI","SecRow3CO"};
  for (Int_t i = 0; i < 2; i++) {
    c1->cd(i+1);
    TString Plot(plot[i]);
    Plot += ":"; Plot += var;
    for (Int_t j = 0; j < 2; j++) {
      SumT->SetMarkerColor(j+1);
      TString Cut(plot[i]);
      Cut += ">-10"; 
      TString same("");
      if (j) {
	Cut += "&&abs("; Cut += plot[i]; 
	if (i == 0) Cut += ")>0.030";
	else        Cut += ")>0.018";
	same = "same";
      }
      cout << "Plot: " << Plot << "\tCut: " << Cut << "\tsame: " << same << endl;
      SumT->Draw(Plot,Cut,same);
    }
  }
}
