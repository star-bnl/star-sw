//____________________________________________________________
void PlotK3(const Char_t *fN = "PadResponseFunctionInner") {
  gROOT->LoadMacro("TpcT.C+");
  TpcT t;
  t.MakeFunctions();
  TF1 *func = gROOT->GetFunction(fN);
  if (! func ) return;
  TF1 *gaus = gROOT->GetFunction("gaus");
  if (! gaus) return;
  gaus->FixParameter(1,0);
  TH1D *hist = new TH1D("hist","hist",100,-func->GetXmax(),func->GetXmax());
  const Int_t N = 20;
  const Double_t k3min = 0.1;
  const Double_t k3max = 2.1;
  Double_t dK3 = (k3max - k3min)/N;
  TH1D *out = new TH1D(Form("h%s",fN),fN, N, k3min - 0.5*dK3, k3max+0.5*dK3);
  for (Int_t i = 0; i < N; i++) {
    hist->Reset();
    Double_t K3 = k3min + dK3*i;
    func->SetParameter(3,K3);
    hist->Reset();
    hist->FillRandom(fN,10000);
    hist->Fit(gaus);
    out->SetBinContent(i+1,gaus->GetParameter(2));
    out->SetBinError(i+1,gaus->GetParError(2));
  }
  cout << " ========================================================= " << endl;
}
