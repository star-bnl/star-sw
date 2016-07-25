#include "Riostream.h"
#include <string>
#include "TFile.h"
#include "TH2.h"
#include "TString.h"
#include "TCanvas.h"
#include "TROOT.h"
#include "TDirectory.h"
#include "TStyle.h"
#include "TF1.h"
#include "TMath.h"
using namespace std;
const Char_t *files[3][2] = {
  "estar_test_b.map","0T 4m long",
  "estar_test_a.map","5T 4m long",
  "estar_test_c.map","5T 2m long"
};
static TH2F *gBr = 0, *gBz = 0;
void eStar_mag_map() {
  gStyle->SetOptStat(0);
  TFile *fOut = new TFile("eStarFieldT.root","update");
  Float_t R, Z, Br, Bz;
  char line[121];
  for (Int_t f = 0; f < 3; f++) {
    FILE *fp = fopen(files[f][0],"r");
    if (! fp) {
      cout << "Can't open" << files[f][0] << endl;
      continue;
    }
    TString name(files[f][0]);
    name.ReplaceAll("_test_","");
    name.ReplaceAll(".map","");
    TString Name("Br"); Name += name;
    TString Title("Br "); Title += files[f][0]; Title += ":"; Title += files[f][1];
    TH2F *br = new TH2F(Name,Title,501,-1.0,1001.0,251,-1.0,501.0);
    br->SetXTitle("Z (cm)");
    br->SetYTitle("R (cm)");
    Name = "Bz"; Name += name;
    Title = "Bz "; Title += files[f][0];
    TH2F *bz = new TH2F(Name,Title,501,-1.0,1001.0,251,-1.0,501.0);
    bz->SetXTitle("Z (cm)");
    bz->SetYTitle("R (cm)");
    while (fgets(&line[0],120,fp)) {
      TString Line(line);
      cout << Line.Data();
      if (Line.BeginsWith("User")) continue;
      if (Line.BeginsWith("Opera")) continue;
      if (Line.BeginsWith("@")) continue;
      if (Line.BeginsWith("Node")) continue;
      if (Line.BeginsWith("Command")) continue;
      if (Line.BeginsWith("Maximum")) continue;
      if (Line.BeginsWith("Unit")) continue;
      if (Line.BeginsWith("Info")) continue;
      if (Line.Contains("Page")) continue;
      if (Line.BeginsWith("Unit")) continue;
      if (Line.BeginsWith("Opend")) continue;
      if (Line.BeginsWith("No")) continue;
      if (Line.BeginsWith("Solution")) continue;
      if (Line.Contains("vector")) continue;
      if (Line.Contains("coordinate")) continue;
      
      Int_t n = sscanf(&line[0],"%f %f %f %f",&R, &Z, &Bz, &Br);
      if (n != 4) {
	cout << "Failed to read " << line;
	continue;
      }
      //      cout << "R " << R << "\tZ " << Z << "\tBr " << Br << "\tBz " << Bz << endl;
      Int_t bin = br->FindBin(Z,R);
      br->SetBinContent(bin,-Br/10000.);
      bz->SetBinContent(bin,-Bz/10000.);
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
Double_t blFunc(Double_t *x, Double_t *p = 0) {
  Double_t eta = x[0];
  Double_t Theta = 2*TMath::ATan(TMath::Exp(-eta));
  //  Double_t Theta = TMath::DegToRad()*x[0]; // rad
  Double_t cosT = TMath::Cos(Theta);
  Double_t sinT = TMath::Sin(Theta);
  //  Double_t eta = - TMath::Log(TMath::Tan(Theta/2.));
  Double_t step = 1; // cm
  /* 
     eta = - log(tan(Theta/2));
     tan(Theta/2) = exp(-eta);
     tan(Theta) = 2*Tan(Theta/2)/(1 - Tan^2(Teta/2)) = 2 * exp(-eta)/(1 - exp(-2*eta)) = 2/(exp(eta) - exp(-eta)) = 1/sinh(eta);
     cos(Theta) = 1./sqrt(1 + tan^2(Thera)) = 1./sqrt(1 + 1/sinh^2(eta)) = sinh(eta)/cosh(eta) = tanh(eta);
     sin(Theta) = sqrt(1 - cos^2(Theta)) = sqrt(1 - tanh^2(eta)) = 1/cosh(eta);
   */
  Double_t R = 0, Z = 0;
  Double_t sum = 0;
  Double_t sum2 = 0;
  while (R <= 200 && Z <= 200) {
    R += step*sinT;
    Z += step*cosT;
    Double_t br = gBr->Interpolate(Z,R);
    Double_t bz = gBz->Interpolate(Z,R);
    sum += TMath::Abs(bz*sinT - br*cosT)*step/100;
    sum2 += sum*step/100;
    //    sum += bz*sinT - br*cosT;
  }
  return sum2;
}
//________________________________________________________________________________
TF1 *BL(const Char_t *name = "", TH2F *br = 0, TH2F *bz = 0) {
  gBr = br; gBz = bz;
  if (! gBr || ! gBz) return 0;
  //  TF1 *f =  new TF1(Form("BL%s",name),blFunc,0,90,0);
  TF1 *f =  new TF1(Form("BL%s",name),blFunc,0,5,0);
  f->SetNpx(400);
  return f;
 }
//________________________________________________________________________________
void Plot(Int_t i1 = 0, Int_t i2 = 3) {
  const Char_t *RZ[2] = {"z","r"};
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  gStyle->SetOptStat(0);
  gStyle->SetTitleSize(1);
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1");
  c1->Divide(2,i2-i1);
  TH2F *brz0[2] = {0, 0};
  for (Int_t irz = 0; irz < 2; irz++) {
    for (Int_t idz = i1; idz < i2; idz++) {
      Int_t k = 1 + irz + 2*(idz-i1);
      c1->cd(k);
      TString name(Form("B%s%s",RZ[irz],files[idz][0]));
      name.ReplaceAll("_test_","");
      name.ReplaceAll(".map","");
      TH2F *hist = (TH2F *) gDirectory->Get(name);
      if (! hist) {
	cout << name.Data() << " has not been found" << endl;
	continue;
      }
      Double_t zmax = 500;
      //	Int_t i1 = hist->GetXaxis()->FindBin(-zmax);
      Int_t i2 = hist->GetXaxis()->FindBin( zmax);
      hist->GetXaxis()->SetRange(1,i2);
      Int_t j2 = hist->GetYaxis()->FindBin(500.);
      hist->GetYaxis()->SetRange(1,j2);
      brz0[irz] = hist;
      hist->Draw("colz");
    }
  }
}
/* ha->SetXTitle("#Theta (deg)"
ha->SetYTitle("|B #times L| (T m)")

TH1 *ha  = BL("a",Brestara,Bzestara)->GetHistogram(); ha->SetName("ha"); ha->Draw()
TH1 *hb  = BL("b",Brestarb,Bzestarb)->GetHistogram(); hb->SetName("hb"); hb->SetLineColor(2); hb->Draw("same")
TH1 *hc  = BL("c",Brestarc,Bzestarc)->GetHistogram(); hc->SetName("hc"); hc->SetLineColor(3); hc->Draw("same")
ha->SetXTitle("#eta ")
ha->SetYTitle("|B #times L^{2}| (T m^{2})")
TLegend *l = new TLegend(0.5,0.5,0.8,0.8)
l->AddEntry(hb,"No pole tips")
l->Draw()
l->AddEntry(ha,"Thin solenoid 5T, length 4 m")
l->AddEntry(hc,"Thin solenoid 5T, length 2 m")
 */
