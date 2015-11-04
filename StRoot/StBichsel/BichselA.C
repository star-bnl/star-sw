class Bichsel;
//Bichsel *m_Bichsel = Bichsel::instance();
Bichsel *m_Bichsel = 0;
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t poverm = TMath::Power(10.,x[0])/par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  Double_t val = m_Bichsel->GetI70M(TMath::Log10(poverm),1.)*TMath::Exp(par[2]);
  //  cout << "pove\t" << pove << "\t" << val1 << "\tpoverm\t" << val2 << "\t" << val << endl;
  return TMath::Log10(val);
}
//________________________________________________________________________________

void BichselA(const Char_t *tag="", Int_t hyp = 7) {
#if 1
  if (!m_Bichsel) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
#endif
  TLegend *leg = new TLegend(0.7,0.4,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  Char_t *names[8]   = {     "K",          "e",       "p",      "pi",     "mu",       "d",       "t",     "He3"};
  Char_t *Names[8]   = {     "K",          "e",       "p",     "#pi",    "#mu",       "d",       "t",     "He3"};
  Double_t masses[8] = {0.493677,0.51099907e-3,0.93827231,0.13956995,0.1056584,0.1876E+01,0.2809E+01,0.2809E+01};
  for (int l = 0; l < 5; l++) {
    if (hyp >= 0 && hyp != l) continue;
    for (int k = -1; k <= 1; k++) {
      TString Name(Form("%sb70%s_%i",tag,names[l],k+2)); cout << Name.Data() << endl;
      TF1 *fb70 = new TF1(Name,bichsel70,-1,4,3);
      fb70->SetParameter(0,masses[l]);
      fb70->SetParameter(1,0);
      if (l == 7) fb70->SetParameter(1,1);
      fb70->SetLineColor(l+1);
      fb70->SetLineStyle(1);
      TString Name(Names[l]);
      fb70->SetParameter(2,k*0.08);
      if (k == -1) Name += " -8%";
      if (k ==  1) Name += " +8%";
      leg->AddEntry(fb70,Form("%s %s I70",Name.Data(),tag),"L");
      fb70->Draw("same");
    }
  }
  leg->Draw();
}
