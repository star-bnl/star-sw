class Bichsel;
//Bichsel *m_Bichsel = Bichsel::instance();
Bichsel *m_Bichsel = Bichsel::Instance();
TF1 *sipi = 0, *sie = 0, *sip = 0, *siK = 0, *sid = 0, *sit = 0, *sihe3 = 0;
//________________________________________________________________________________
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t poverm = x[0]/par[0];
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
  Double_t poverm = x[0]/par[0];
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
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t poverm = x[0]/par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  Double_t val = m_Bichsel->GetI70(TMath::Log10(poverm),1.);
  return val;
}
//________________________________________________________________________________

void BichselP() {// dE/dx versus momentum 
#if 1
  if (!m_Bichsel || gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
#endif
  TLegend *leg = new TLegend(0.7,0.4,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  Char_t *names[8]   = {      "pi",          "e",       "p",     "mu",     "K",       "d",       "t",     "He3"};
  Char_t *Names[8]   = {     "#pi",          "e",       "p",    "#mu",     "K",       "d",       "t",     "He3"};
  Double_t masses[8] = {0.13956995,0.51099907e-3,0.93827231,0.1056584,0.493677,0.1876E+01,0.2809E+01,0.2809E+01};
  for (int l = 0; l < 5; l++) {
    TF1 *fbi = new TF1(Form("bi%s",names[l]),bichselZ,0.05,100.0,2);
    fbi->SetParameter(0,masses[l]);
    fbi->SetParameter(1,0);
    fbi->SetLineColor(l+1);
    fbi->SetLineStyle(1);
    leg->AddEntry(fbi,Names[l],"L");
    fbi->Draw("same");
    TF1 *fb70 = new TF1(Form("b70%s",names[l]),bichsel70,0.05,100.0,2);
    fb70->SetParameter(0,masses[l]);
    fb70->SetParameter(1,0);
    fb70->SetLineColor(l+1);
    fb70->SetLineStyle(2);
    leg->AddEntry(fb70,Form("%s I70",Names[l]),"L");
    fb70->Draw("same");
    TF1 *fsir = new TF1(Form("sir%s",names[l]),sifunc,0.05,100.0,2);
    fsir->SetParameter(0,masses[l]);
    fsir->SetParameter(1,0);
    fsir->SetLineColor(l+1);
    fsir->SetLineStyle(3);
    leg->AddEntry(fsir,Form("%s Sirrf",Names[l]),"L");
    fsir->Draw("same");
  }
  leg->Draw();
}
