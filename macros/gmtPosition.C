void gmtPosition() {
  Double_t   z[8] = {77.768, 2.729, -77.768,  -2.729, 77.768, 2.729, -77.768,  -2.729};
  Double_t phi[8] = {   -60,   -60,     -60,     -60,     30,    30,      30,      30};
  double inch = 2.54;
  Int_t n = 8;
  for (Int_t i = 0; i < n; i++) z[i] *= inch;
  TCanvas *vC1 = new TCanvas("GMT","GMT");
  TH1F *frame = vC1->DrawFrame(-210,-70,210,40);
  TGraph  *grxy = new TGraph(n,z,phi);
  
  frame->SetTitle("GMT");
  frame->SetXTitle("z (cm)");
  frame->SetYTitle("#phi (^{o})");
  grxy->Draw("xp");
}
