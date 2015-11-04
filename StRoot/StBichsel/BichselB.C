class Bichsel;
//Bichsel *m_Bichsel = Bichsel::instance();
Bichsel *m_Bichsel = Bichsel::Instance();
TF1 *sipi = 0, *sie = 0, *sip = 0, *siK = 0, *sid = 0, *sit = 0, *sihe3 = 0;
//________________________________________________________________________________
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  //  Double_t ppion  = pove*0.13956995;
  Double_t poverm = ppion;///par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  return charge2*BetheBloch::Sirrf(poverm,60,k==3);///BetheBloch::Sirrf(pove,60,0));
}
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t ppion  = pove;//*0.13956995;
  Double_t poverm = ppion/par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  Double_t val1 = m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),1.);
  //  Double_t val2 = m_Bichsel->GetMostProbableZ(TMath::Log10(pove),1.);
  Double_t val = TMath::Exp(val1);// - val2;
  //  cout << "pove\t" << pove << "\t" << val1 << "\tpoverm\t" << val2 << "\t" << val << endl;
  return val;
}
//________________________________________________________________________________

void BichselB() {
#if 1
  if (!m_Bichsel || gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
#endif
  TLegend *leg = new TLegend(0.72,0.7,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  sipi = new TF1("bipi",bichselZ,-1.,4.0,2);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  sie = new TF1("bie",bichselZ,-1.,4.0,2);
  sie->SetParameter(0,0.51099907e-3);
  sie->SetParameter(1,3);
  sie->SetLineColor(2);
  leg->AddEntry(sie,"e-#pi","L");
//   TF1 *sie1 = new TF1("bie1",bichselZ,0.01,1.20,2);
//   sie1->SetParameter(0,0.51099907e-3);
//   sie1->SetParameter(1,0);
//   sie1->SetLineColor(2);
  sip = new TF1("bi",bichselZ,0.01,1.20,2);
  sip->SetParameter(0,0.93827231);
  sip->SetParameter(1,0);
  sip->SetLineColor(3);
  leg->AddEntry(sip,"p-#pi","L");
  sim = new TF1("bim",bichselZ,0.01,1.20,2);
  sim->SetParameter(0,0.1056584);
  sim->SetParameter(1,0);
  sim->SetLineColor(8);
  leg->AddEntry(sim,"#mu-#pi","L");
  siK = new TF1("biK",bichselZ,0.01,1.20,2);
  siK->SetParameter(0,0.493677);
  siK->SetParameter(1,0);
  siK->SetLineColor(4);
  leg->AddEntry(siK,"K-#pi","L");
  sid = new TF1("bid",bichselZ,0.01,1.20,2);
  sid->SetParameter(0,0.1876E+01);
  sid->SetParameter(1,0);
  sid->SetLineColor(6);
  leg->AddEntry(sid,"d-#pi","L");
//   TF1 *sit = new TF1("bit",bichselZ,0.01,1.20,2);
//   sit->SetParameter(0,0.2809E+01);
//   sit->SetParameter(1,0);
//   sit->SetLineColor(7);
//  leg->AddEntry(sit,"t-#pi","L");

//   sihe3 = new TF1("bihe3",bichselZ,0.01,1.20,2);
//   sihe3->SetParameter(0,0.2809E+01);
//   sihe3->SetParameter(1,1);
//   sihe3->SetLineColor(1);
//   leg->AddEntry(sihe3,"He3-#pi","L");
  sipi->Draw("same");
  sim->Draw("same");
  sie->Draw("same");
  //  sie1->Draw("same");
  siK->Draw("same");
  sip->Draw("same");
  sid->Draw("same");
  //  sit->Draw("same");
//   sihe3->Draw("same");
  leg->Draw();
}
