class Bichsel;
Bichsel *m_Bich = 0;
Bichsel *m_P10 = 0;
Bichsel *m_Bichsel = 0;
//________________________________________________________________________________
Double_t bichsel(Double_t *x,Double_t *par) {
  Double_t val = 0;
  Int_t l = par[0];
  Int_t m = par[1];
  if (l == 0) m_Bichsel = m_Bich;
  else        m_Bichsel = m_P10;
  if (m_Bichsel) { 
    if (m == 0) val =            m_Bichsel->GetMostProbableZ(x[0],1.);
    else        val = TMath::Log(m_Bichsel->GetI70(x[0],1.));
  }
  return val;
}
//________________________________________________________________________________
void P10Ar(){// plot dE/dx versus log10(p) for P10 and Ar
  if (!m_Bich || gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bich = Bichsel::Instance();
    m_P10  = Bichsel::Instance("p10");
  }
  TLegend *leg = new TLegend(0.7,0.4,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  Char_t *names[2]   = {      "Ar",          "P10"};
  Char_t *Names[2]   = {       "z",          "I70"};
  TF1 *f = 0;
  TString Opt("");
  for (int l = 0; l < 2; l++) {
    for (int m = 0; m < 2; m++) {
      TF1 *f = new TF1(Form("f%s%s",Names[m],names[l]),bichsel,-1.,4.0,2);
      f->SetLineColor(l+1);
      f->SetLineStyle(m+1);
      f->SetParameters(l,m);
      leg->AddEntry(f,Form("%s %s",Names[m],names[l]),"L");
      f->Draw(Opt.Data());
      Opt = "same";
    }
  }
  leg->Draw();
}

