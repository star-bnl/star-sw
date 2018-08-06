class Bichsel;
Bichsel *m_Bichsel = 0;
const Int_t NMasses = 5;
const Double_t Masses[NMasses] = {0.93827231,
				  0.493677,
				  0.13956995,
				  0.51099907e-3,
				  1.87561339};
const Char_t *Names[NMasses] = {"p", "K","pi","e","d"};
const Int_t NF = 6;
const Char_t *FNames[6] = {"Bz","Girrf","Sirrf","B70","B60","RB70"};
const Int_t Nlog2dx = 7;
const Double_t log2dx[Nlog2dx]; // = {0,1,1.6,2,2.58,2.8,3};
//________________________________________________________________________________
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return BetheBloch::Girrf(poverm,1.e-3,k);
}
//________________________________________________________________________________
Double_t gfunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return BetheBloch::Sirrf(poverm,par[2],k);
}
// Double_t bbfunc(Double_t *x,Double_t *par) {
//   Double_t pove   = x[0];
//   Double_t poverm = pove/par[0];
//   BetheBloch BB;
//   Double_t value = 1.e6*BB(poverm);
//   //  printf("x : %f p: %f  val : %f \n",x[0],poverm,value);
//   return value;
// }
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return TMath::Exp(m_Bichsel->GetMostProbableZM(TMath::Log10(poverm),par[3]));
}
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return m_Bichsel->GetI70M(TMath::Log10(poverm),par[3]);
}
Double_t Rbichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return m_Bichsel->GetI70M(TMath::Log10(poverm),par[3])/m_Bichsel->GetI70M(TMath::Log10(poverm));
}
Double_t Rbichselz(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return TMath::Exp(m_Bichsel->GetMostProbableZM(TMath::Log10(poverm),par[3]) -
		    m_Bichsel->GetMostProbableZM(TMath::Log10(poverm)));
}
Double_t bichsel60(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return m_Bichsel->GetI60(TMath::Log10(poverm),par[3]);
}
void bichdX(TString Opt = "I70M") {
  if (gClassTable->GetID("StBichsel") < 0) {
//     gSystem->Load("libStar");
//     gSystem->Load("St_base");
//     gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
  }
  gStyle->SetOptDate(0);
  if (!m_Bichsel) m_Bichsel = Bichsel::Instance();
  TCanvas *c1 = new TCanvas("c1");
  c1->SetLogx();
  //  c1->SetLogy();
  //  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  //  TH1F *hr = c1->DrawFrame(1.e-2,0,1.e3,10);
  TH1F *hr = 0;
  Int_t f =  0;
  TString Fnames;
  if (Opt.Contains("70",TString::kIgnoreCase)) {
    Fnames = "I_{70}";
    hr = c1->DrawFrame(1.e-1,0.7,1.e4,1.5);
    //  hr->SetXTitle("Momentum (GeV/c)");
    f = 5;
  } else if (Opt.Contains("z",TString::kIgnoreCase)) {
    Fnames = "I_{fit}";
    hr = c1->DrawFrame(1.e-1,0.7,1.e4,1.5);
    f = 0;
  }
  hr->SetTitle(Form("Difference Bichsel %s  predictions for different dx",Fnames.Data()));
  hr->SetXTitle("#beta#gamma                             ");
  //  hr->SetYTitle("dE/dx (keV/cm)");  
  hr->SetYTitle(Form("%s(dx)/%s(2 cm)",Fnames.Data(),Fnames.Data()));
  //                     Mass Type Length  log2(dx)
  Double_t params[4] = {  1.0,  0.,   60., 1.};  
  TLegend *leg = new TLegend(0.62,0.7,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  Int_t h = 0;
  //  for (Int_t f = 0; f< NF; f++) {
  TF1 *func = 0;
  for (Int_t f = 5; f <= 5; f++) {
    Int_t icol = 2;
    for (Int_t dx = 0; dx < Nlog2dx; dx++) { 
      params[3] = TMath::Log2(dx+1.);;
      Char_t *FunName = Form("%s%s%i",FNames[f],Names[h],dx);
      if (Opt.Contains("70",TString::kIgnoreCase)) func = new TF1(FunName,Rbichsel70,1.e-1,1.e4,4);
      if (Opt.Contains("z",TString::kIgnoreCase))  func = new TF1(FunName,Rbichselz,1.e-1,1.e4,4);
      if (! func) continue;
      func->SetLineColor(icol);
      //      func->SetLineStyle(f+1);
      icol++;
      if (icol == 5) icol++;
      func->SetParameters(params); 
      func->Draw("same");
      leg->AddEntry(func,Form("%s dX = %4.1f (cm)",Fnames.Data(),TMath::Power(2.,params[3])),"L");
    }
  }
  leg->Draw();
}
