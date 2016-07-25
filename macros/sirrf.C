Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t ppion  = pove*0.13956995;
  Double_t poverm = ppion/par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  return TMath::Log(charge2*BetheBloch::Sirrf(poverm,60,k==3)/BetheBloch::Sirrf(pove,60,0));
}

void sirrf() {
#if 1
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
  }
#endif
  TF1 *sipi = new TF1("sipi",sifunc,-.4,4.0,2);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  TF1 *sie = new TF1("sie",sifunc,-.4,4.0,2);
  sie->SetParameter(0,0.51099907e-3);
  sie->SetParameter(1,3);
  sie->SetLineColor(2);
//   TF1 *sie1 = new TF1("sie1",sifunc,-.4,4.0,2);
//   sie1->SetParameter(0,0.51099907e-3);
//   sie1->SetParameter(1,0);
//   sie1->SetLineColor(2);
  TF1 *sip = new TF1("si",sifunc,-.4,4.0,2);
  sip->SetParameter(0,0.93827231);
  sip->SetParameter(1,0);
  sip->SetLineColor(3);
  TF1 *siK = new TF1("siK",sifunc,-.4,4.0,2);
  siK->SetParameter(0,0.493677);
  siK->SetParameter(1,0);
  siK->SetLineColor(4);
  TF1 *sid = new TF1("sid",sifunc,-.4,4.0,2);
  sid->SetParameter(0,0.1876E+01);
  sid->SetParameter(1,0);
  sid->SetLineColor(6);
  TF1 *sit = new TF1("sit",sifunc,-.4,4.0,2);
  sit->SetParameter(0,0.2809E+01);
  sit->SetParameter(1,0);
  sit->SetLineColor(7);

  TF1 *sihe3 = new TF1("sihe3",sifunc,-.4,4.0,2);
  sihe3->SetParameter(0,0.2809E+01);
  sihe3->SetParameter(1,1);
  sihe3->SetLineColor(1);

  sipi->Draw("same");
  sie->Draw("same");
  //  sie1->Draw("same");
  siK->Draw("same");
  sip->Draw("same");
  sid->Draw("same");
  sit->Draw("same");
  sihe3->Draw("same");
}
