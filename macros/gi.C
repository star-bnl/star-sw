
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return BetheBloch::Sirrf(poverm,60.,k);
}

Double_t gifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  Double_t tcut = 1.e-3;
  if (par[2] > 0 && par[2] < 1.) tcut = par[2];
  Double_t value = BetheBloch::Girrf(poverm,tcut,k);
  //  printf("x : %f p: %f  val : %f \n",x[0],poverm,value);
  return value;
}
void gi() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
  }
  TCanvas *c1 = new TCanvas("c1");
  c1->SetLogx();
  c1->SetLogy();
  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  TLegend *leg = new TLegend(0.4,0.6,0.89,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
  hr->SetXTitle("#beta #gamma");
  hr->SetYTitle("dE/dx (keV/cm)");
  TF1 *sipi = new TF1("sipi",sifunc,1.e-2,1.e3,2);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  sipi->SetLineColor(1);
  sipi->Draw("same");
  leg->AddEntry(sipi,"Sirrf","l");
  TF1 *gi10keV = new TF1("gi10keV",gifunc,1.e-2,1.e3,3);
  gi10keV->SetParameter(0,0.13956995);
  gi10keV->SetParameter(1,0);
  gi10keV->SetParameter(2,1e-5);
  gi10keV->SetLineColor(2);
  gi10keV->Draw("same");
  leg->AddEntry(gi10keV,"Girrf with 10 keV cut","l");
  TF1 *gi100keV = new TF1("gi100keV",gifunc,1.e-2,1.e3,3);
  gi100keV->SetParameter(0,0.13956995);
  gi100keV->SetParameter(1,0);
  gi100keV->SetParameter(2,1.e-4);
  gi100keV->SetLineColor(6);
  gi100keV->Draw("same");
  leg->AddEntry(gi100keV,"Girrf with 100 keV cut","l");
  TF1 *gi1MeV = new TF1("gi1MeV",gifunc,1.e-2,1.e3,3);
  gi1MeV->SetParameter(0,0.13956995);
  gi1MeV->SetParameter(1,0);
  gi1MeV->SetParameter(2,1e-3);
  gi1MeV->SetLineColor(3);
  gi1MeV->Draw("same");
  leg->AddEntry(gi1MeV,"Girrf with 1 MeV cut","l");
  TF1 *gi100MeV = new TF1("gi100MeV",gifunc,1.e-2,1.e3,3);
  gi100MeV->SetParameter(0,0.13956995);
  gi100MeV->SetParameter(1,0);
  gi100MeV->SetParameter(2,.1);
  gi100MeV->SetLineColor(4);
  gi100MeV->Draw("same");
  leg->AddEntry(gi100MeV,"Girrf with 100 MeV cut","l");
  leg->SetHeader("dE/dx parameterizations");
  leg->Draw();
}
