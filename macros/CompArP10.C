class Bichsel;
Bichsel *m_BichselP10 = 0;
Bichsel *m_BichselAr  = 0;
const Int_t NMasses = 5;
const Double_t Masses[NMasses] = {0.93827231,
				  0.493677,
				  0.13956995,
				  0.51099907e-3,
				  1.87561339};
const Char_t *Names[NMasses] = {"p", "K","pi","e","d"};
const Int_t NF = 3;
const Char_t *FNames[3] = {"Bz","B70","B60"};
const Char_t *TNames[3] = {"Most Probable value","30% truncated mean","40% truncated mean"};
const Int_t Nlog2dx = 5; // 4;
const Double_t log2dx[5] = {-1,0,1,2,3};
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t poverm = x[0];
  return 
    m_BichselP10->GetMostProbableZ(TMath::Log10(poverm),par[0]) -
    m_BichselAr->GetMostProbableZ(TMath::Log10(poverm),par[0]);
}
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t poverm = x[0];
  return 
    TMath::Log(m_BichselP10->GetI70(TMath::Log10(4),par[0])/
	       m_BichselAr->GetI70(TMath::Log10(4),par[0]));
}
//________________________________________________________________________________
Double_t bichsel60(Double_t *x,Double_t *par) {
  Double_t poverm = x[0];
  return
    TMath::Log(m_BichselP10->GetI60(TMath::Log10(4),par[0])/
	       m_BichselAr->GetI60(TMath::Log10(4),par[0]));
}
//________________________________________________________________________________
void CompArP10() { // compare Ar and P10 calculations
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
  }
  const Double_t xmin = 1.e-2;
  const Double_t xmax = 1.e4;
  if (!m_BichselAr ) m_BichselAr  = Bichsel::Instance("bich");
  if (!m_BichselP10) m_BichselP10 = Bichsel::Instance("p10"); 
  for (Int_t f = 0; f < NF; f++) { // Functions
    TCanvas *c1 = new TCanvas(FNames[f],TNames[f]);
    c1->SetLogx();
    TH1F *hr = c1->DrawFrame(xmin,-0.2,xmax,0.2);
    hr->SetTitle("Comparison P10 wrp Ar calculations");
    hr->SetXTitle("#beta#gamma");
    hr->SetYTitle("log((dE/dx)_{P10}/(dE/dx)_{Ar})");  
    hr->Draw();
    TLegend *leg = new TLegend(0.4,0.7,0.9,0.9,"");
    for (Int_t dx = 0; dx < Nlog2dx; dx++) { 
      Char_t *FunName = Form("%s%i",FNames[f],(int)log2dx[dx]);
      TF1 *func = 0;
      if (TString(FNames[f]) == "Bz") {
	func = new TF1(FunName,bichselZ,xmin,xmax,5);
	func->SetLineColor(4);
      }
      else {if (TString(FNames[f]) == "B70") {
	func = new TF1(FunName,bichsel70,xmin,xmax,5);
	func->SetLineColor(6);
      }
      else {if (TString(FNames[f]) == "B60") {
	func = new TF1(FunName,bichsel60,xmin,xmax,5);
	func->SetLineColor(7);
      }}}
      if (! func) continue;
      func->SetLineColor(dx+1);
      func->SetParameter(0,log2dx[dx]);
      func->Draw("same");
      TString name(FNames[f]);
      name += "/"; 
      name += TNames[f];
      name += Form(" dX = %5.2f cm", pow(2.,log2dx[dx]));
      leg->AddEntry(func,name.Data(),"L");
    }
    leg->Draw();
  }
}
