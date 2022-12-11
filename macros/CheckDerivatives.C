/*
  root.exe lBichsel.C CheckDerivatives.C
 */
TF1 *prob1 = 0;
TF1 *prob2 = 0;
TF1 *probd = 0;
TH1 *h1 = 0;
TH1 *h2 = 0;
void CheckDerivatives(Double_t Np = 32) {
  prob1 = StdEdxModel::instance()->FProb();
  prob1->SetName("prob1");
  prob1->SetParameter(0, Np);
  prob1->Draw();
  prob2 = StdEdxModel::instance()->FProb();
  prob1->SetName("prob2");
  prob1->SetParameter(0, Np+1);
  prob1->SetLineColor(2);
  prob1->Draw("same");
  h1 = new TH1D(*((TH1D*)prob1->GetHistogram()));
  h1->SetName("prob1h");
  h2 = new TH1D(*((TH1D*)prob2->GetHistogram()));
  h2->SetName("prob2h");
  //  h1->Draw("same");
  //  h2->Draw("same");
  c2 = new TCanvas("c2","c2");
  probd = StdEdxModel::instance()->FProb();
  probd->SetParameter(0,Np+0.5);
  probd->SetLineColor(4);
  probd->Draw("");
  h1->Add(h2,-1);
  h1->Draw("same");
}
