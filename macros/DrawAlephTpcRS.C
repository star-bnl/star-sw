Double_t beta(Double_t bgL10) {
  Double_t bg = TMath::Power(10., bgL10);
  Double_t bg2 = bg*bg;
  Double_t gamma = TMath::Sqrt(bg2 + 1);
  return bg/gamma;
}
//________________________________________________________________________________
void DrawAlephTpcRS() {
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  const Int_t NHYP = 9;
  const Char_t *names[10]   = {"electron", "muon", "pion", "kaon", "proton", "deuteron", "triton",   "He3",  "alpha", "pionMIP"};
  Double_t      xmin[10]    = {         3,      0,      0,   -0.1,     -0.6,        -1.,       -1,    -0.8,     -0.8,        -1};
  Double_t      xmax[10]    = {         6,    4.5,    4.5,    3.7,      3.3,        2.9,      2.8,     2.9,      2.9,       6.5};
  Int_t            n[10]    = {         3,      1,      0,      8,        8,          7,        9,       8,        9,        -1};
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  if (! c2 ) c2 = new TCanvas("c2","c2");
  TH1F *frame = c1->DrawFrame(-1.,-0.2,6.5,0.2);
  frame->SetTitle("Deviation of log_{10} (dE/dx) from #pi fit for mass hypothesis versus log_{10} (#beta #gamma)");
  frame->SetXTitle("log_{10} (#beta #gamma)");
  frame->SetYTitle("Deviation of log_{10} (dE/dx)");
  gStyle->SetOptStat(0);
  TLegend *l = new TLegend(0.6,0.7,0.8,0.9);
  Double_t pars[10];
  ofstream out;
  out.open("AlephParameterization.h");
  for (Int_t h = 0; h < NHYP; h++) {
    TString line;
    FitP->SetMarkerColor(h+1);
    c2->cd();
    FitP->Draw(Form("difdEdxAL10:bgL10>>%s",names[h]),Form("(hyp==%i)/(ddEdxL10**2)",h),"profgsame");
    TH1 *h1 = (TH1 *) gDirectory->Get(names[h]);
    if (! h1) continue;
    l->AddEntry(h1,names[h]);
#if 0
    TF1 *f = gROOT->GetListOfFunctions()->FindObject(Form("pol%i",n[h]));
    if (! f) {
      f = new TF1(Form("pol%i",n[h]),Form("pol%i",n[h]),0,1);
    }
    if (! f) continue;
    f->SetLineColor(h+1);
    c2->cd();
    h1->Fit(f,"er","",xmin[h],xmax[h]);
    f->GetParameters(pars);
    line = Form("/* %8s */{ %2i, %2i, %5.1f, %5.1f, {",names[h], h, n[h]+1, xmin[h], xmax[h]);
    for (Int_t i = 0; i < 10; i++) {
      if (i) line += ",";
      if (i <= n[h]) line += Form("%12.5f", pars[i]);
      else           line += Form("           0", pars[i]);
    }
    line += "}},";
    cout << line.Data() << endl;
    out << line.Data() << endl;
#endif
    c1->cd();
    h1->SetStats(0);
    h1->Draw("same");
  }
  c1->cd();
  l->Draw();
  out.close(); 
}
