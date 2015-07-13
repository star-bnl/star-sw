/* Pressure 1021.02 +/- 0.642 
   Pressure 1021.74 +/- 0.138 for beginTime > "2004-02-12 14:00:00" and beginTime < "2004-02-12 15:10:00" 
   Pressure Garfield = 1012.73
*/
TGraphErrors *graphs[2] = {0,0};
void DriftE() {// 2004 Drift velocity versus Electric Field
  struct DriftE_t {
    Int_t Run;
    Double_t E;
    Double_t vWest, dvWest, vEast, dvEast;
  };
  DriftE_t data[] = {
    {12017, 133.37,   5.5398  , 1.67199e-05,  5.54024 , 2.14391e-05},
    {12033, 133.37,   5.53935 , 1.09141e-05,  5.53917 , 2.33470e-05},
    {12034, 133.37,   5.53927 , 1.30626e-05,  5.5393  , 3.16969e-05},
    {12035, 147.68,   5.52092 , 1.35202e-05,  5.52117 , 2.19841e-05},
    {12036, 147.68,   5.52093 , 1.06676e-05,  5.52083 , 1.74593e-05},
    {12037, 119.05,   5.50783 , 1.05165e-05,  5.50805 , 1.61257e-05},
    {12038, 119.05,   5.50769 , 1.50374e-05,  5.50787 , 2.02982e-05},
    {12142, 133.37,   5.53821 , 1.84130e-05,  5.53904 , 4.53891e-05}
  };
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  else       c1->Clear();
  gStyle->SetOptStat(0);
  TH1F *frame = c1->DrawFrame(100,5.2,200,5.56);
  frame->SetTitle("(Run IV) drift velocity versus electric field @ pressure = 1021.74 +/- 0.138 mbar");
  frame->SetXTitle("E/P(V/cm/atm)");
  frame->SetYTitle("Velocity(cm/#mus)");
  frame->SetStats(0);
  frame->GetYaxis()->SetLabelSize(0.02);
  Int_t N = sizeof(data)/sizeof(DriftE_t);
  graphs[0] = new TGraphErrors();
  graphs[1] = new TGraphErrors();
  graphs[0]->SetName("West");
  graphs[1]->SetName("East"); graphs[1]->SetMarkerColor(2);
  Double_t p = 1012.73/1021.74;
  for (Int_t i = 0; i < N; i++) {
    graphs[0]->SetPoint(i,data[i].E*p,data[i].vWest); graphs[0]->SetPointError(i,0,data[i].dvWest);
    graphs[1]->SetPoint(i,data[i].E*p,data[i].vEast); graphs[1]->SetPointError(i,0,data[i].dvEast);
  }
  //  graphs[0]->Fit("pol2"); ((TF1 *)graphs[0]->GetListOfFunctions()->FindObject("pol2"))->SetLineColor(1);
  graphs[0]->Draw("p");  
#if 0
  TF1 *pol2 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol2");
  if (pol2) {
    graphs[1]->Fit(pol2);  
    ((TF1 *)graphs[1]->GetListOfFunctions()->FindObject("pol2"))->SetLineColor(2);
  }
#endif
  frame->Draw();
  TLegend *l = new TLegend(0.2,0.2,0.6,0.4);
  graphs[0]->Draw("p"); l->AddEntry(graphs[0],"West");
  graphs[1]->Draw("p"); l->AddEntry(graphs[1],"East");
  l->Draw();
  TH1F *vElectron = (TH1F *) gDirectory->Get("vElectron");
  if (vElectron) {
    vElectron->SetLineColor(4);
    vElectron->SetMarkerColor(4);
    vElectron->SetLineWidth(3);
    vElectron->Draw("samel");
    l->AddEntry(vElectron,"Magboltz @ 1012.7 mbar"); 
  }
}
