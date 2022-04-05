class Bichsel;
//Bichsel *m_Bichsel = Bichsel::instance();
Bichsel *Ar = 0; //Bichsel::Instance();
Bichsel *P10 = 0;
Bichsel *m_Bichsel = 0;
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t ppion  = pove;//*0.13956995;
  Double_t poverm = ppion/par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  m_Bichsel = Ar;
  if (par[3] > 0) m_Bichsel = P10;
  Double_t val1 = m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),1.);
  //  Double_t val2 = m_Bichsel->GetMostProbableZ(TMath::Log10(pove),1.);
  Double_t val = TMath::Log10(TMath::Exp(val1));// - val2;
  //  cout << "pove\t" << pove << "\t" << val1 << "\tpoverm\t" << val2 << "\t" << val << endl;
  return val;
}
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t ppion  = pove;//*0.13956995;
  Double_t poverm = ppion/par[0];
  Int_t    k      = par[1];
  Double_t charge2     = 1;
  if (k == 1) {
    charge2 = 4;
    poverm *= 2;
  }
  m_Bichsel = Ar;
  if (par[3] > 0) m_Bichsel = P10;
  if (! m_Bichsel ) {cout << "m_Bichsel is not defined" << endl;}
  Double_t val1 = m_Bichsel->GetI70(TMath::Log10(poverm),1.)*TMath::Exp(par[2]);
  //  Double_t val2 = m_Bichsel->GetMostProbableZ(TMath::Log10(pove),1.);
  Double_t val = TMath::Log10(val1);// - val2;
  // Double_t val = val1; //TMath::Log(val1);// - val2;
  //  cout << "pove\t" << pove << "\t" << val1 << "\tpoverm\t" << val2 << "\t" << val << endl;
  return val;
}
//________________________________________________________________________________

void BichselD(TString tag="P10") {
#if 1
  if (!P10 || !Ar || gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    P10 = Bichsel::Instance("P10");
    Ar  = Bichsel::Instance("Bich");
  }
#endif
  TLegend *leg = new TLegend(0.7,0.4,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  Char_t *names[8]   = {     "K",          "e",       "p",      "pi",     "mu",       "d",       "t",     "He3"};
  Char_t *Names[8]   = {     "K",          "e",       "p",     "#pi",    "#mu",       "d",       "t",     "He3"};
  Double_t masses[8] = {0.493677,0.51099907e-3,0.93827231,0.13956995,0.1056584,0.1876E+01,0.2809E+01,0.2809E+01};
  for (int l = 0; l < 6; l++) {
    //     TF1 *fbi = new TF1(Form("bi%s",names[l]),bichselZ,-1.,4.0,2);
    //     fbi->SetParameter(0,masses[l]);
    //     fbi->SetParameter(1,0);
    //     fbi->SetLineColor(l+1);
    //     fbi->SetLineStyle(1);
    //     leg->AddEntry(fbi,Names[l],"L");
    //     fbi->Draw("same");
    for (int k = -1; k <= 1; k +=1) {
      TF1 *fb70 = new TF1(Form("%sb70%s",tag.Data(),names[l]),bichsel70,-1.,4.0,4);
      fb70->SetParameter(0,masses[l]);
      fb70->SetParameter(1,0);
      fb70->SetLineColor(l);
      fb70->SetLineStyle(1);
      TString Name(Names[l]);
      fb70->SetParameter(2,0);
      fb70->SetParameter(3,0);
      if (tag.Contains("p10",TString::kIgnoreCase)) fb70->SetParameter(3,1);
      cout << fb70->GetName() << endl;
      fb70->Print();
      if (k == -1) Name += " -8%";
      if (k ==  1) Name += " +8%";
      leg->AddEntry(fb70,Form("%s %s I70",Name.Data(),tag),"L");
      fb70->Draw("same");
    }
  }
  leg->Draw();
}
