Int_t color = 1;
TH2D *DrawLaserTokens(Int_t tokens = 0, 
		      Int_t sec = 2, Int_t row = 1, 
		      Int_t p1 = 30, Int_t p2 = 50, 
		      Int_t t1 = 350, Int_t t2 = 380) {
  
  TString hName(Form("AvLaser_%02i",sec));
  if (tokens >= 0) hName += Form("_%03i",tokens); 
  TH3F *AvLaser = (TH3F *) gDirectory->Get(hName);
  if (! AvLaser) return 0;
  AvLaser->GetXaxis()->SetRange(row,row);
  TH2D *h2 = (TH2D *) AvLaser->Project3D("yz");
  h2->GetXaxis()->SetRange(t1,t2);
  h2->GetYaxis()->SetRange(p1,p2);
  TString same("same");
  TCanvas *c1 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c1");
  if (!c1) c1 = new TCanvas("c1","c1");
  else     c1->Clear();
  c1->cd();
  c1->SetLogz(1);
  h2->Draw("colz");
  TCanvas *c2 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c2");
  if (!c2) {c2 = new TCanvas("c2","c2"); color = 1; same = ""; c2->Divide(2,1);}
  else {same = "same";}
  c2->cd(1);
  h2->SetMarkerColor(color); color++;
  h2->ProjectionX()->Draw(same);
  c2->cd(2);
  h2->ProjectionY()->Draw(same);
  return h2;
}
