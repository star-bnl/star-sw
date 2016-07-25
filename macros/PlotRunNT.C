void PlotRunNT(Char_t *opt = "All", Char_t *var = "run", Int_t nx = 100000, Double_t xmin = 8e4, Double_t xmax = 18e4, Char_t *name = "") {
  TNtuple  *RunNT = (TNtuple  *) gDirectory->Get("RunNT");
  if (! RunNT) return;
  TString plot(Form("dv%s:%s>>%s%s%s(%i,%f,%f)",opt,var,opt,var,name,nx,xmin,xmax));
  TString  cut(Form("(ddv%s>0&&ddv%s<1",opt,opt));
  Int_t badRuns[6] = {131090,131091,158125,158126,158127,172042};//131090|131091|158125|158126|158127|172042
  TString Var(var);
  if (Var.Contains("day")) {
    Int_t N = sizeof(badRuns)/sizeof(Int_t);
    for (Int_t i = 0; i < N; i++) {
      cut += Form("&&run != %i",badRuns[i]);
    }
  }
  cut += Form(")/(ddv%s*ddv%s)",opt,opt);
  cout << "plot: " << plot << endl;
  cout << "cut : " << cut  << endl;
  
  TString Opt(opt);
  RunNT->SetMarkerColor(1);
  if (Opt.Contains("East")) RunNT->SetMarkerColor(2);
  if (Opt.Contains("West")) RunNT->SetMarkerColor(3);
  RunNT->Draw(plot,cut,"profw");
}
