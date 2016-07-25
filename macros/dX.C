void dX() {
  TH3F *SecRow3Npi = (TH3F *) gDirectory->Get("SecRow3Npi");
  TH3F *SecRow3Ne = (TH3F *) gDirectory->Get("SecRow3Ne");
  TH3F *SecRow3NK = (TH3F *) gDirectory->Get("SecRow3NK");
  TH3F *SecRow3NP = (TH3F *) gDirectory->Get("SecRow3NP");
  TH3F *SecRow3Nd = (TH3F *) gDirectory->Get("SecRow3Nd");
  TH3F *SecRow3s[5] = {SecRow3Npi, SecRow3NP, SecRow3NK, SecRow3Ne, SecRow3Nd};
  TProfile *SecRows[5];
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1) c1 = new TCanvas("c1","c1");
  else      c1->Clear();
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  c1->DrawFrame(0.5,1.5,45.5,3.0);
  if (! c2) c2 = new TCanvas("c2","c2");
  else      c2->Clear();
  c2->DrawFrame(0.5,0.0,45.5,1.2);
  for (Int_t i = 0; i < 5; i++) {
    if (!  SecRow3s[i]) continue;
    SecRows[i] = ((TH2D *) SecRow3s[i]->Project3D("zy"))->ProfileX(); SecRows[i]->SetMarkerColor(i+1);
    c1->cd();
    SecRows[i]->Draw("same");
    if (i) {
      Int_t nx = SecRows[i]->GetNbinsX();
      Double_t xmin = SecRows[i]->GetXaxis()->GetXmin();
      Double_t xmax = SecRows[i]->GetXaxis()->GetXmax();
      TH1D *rat = new TH1D(Form("%sR",SecRows[i]->GetName()),"",nx,xmin,xmax);
      rat->SetMarkerColor(i);
      for (Int_t j = 1; j <= nx; j++) {
	Double_t v = SecRows[i]->GetBinContent(j) - SecRows[0]->GetBinContent(j);
	Double_t e = TMath::Sqrt(TMath::Power(SecRows[i]->GetBinError(j),2) + TMath::Power(SecRows[0]->GetBinError(j),2));
	rat->SetBinContent(j,v);
	rat->SetBinError(j,e);
      }
      c2->cd();
      //      rat->Draw("same");
      cout << rat->GetName() << endl;
      rat->Fit("pol0","","same");
    }
  }
}
