/* 
   root.exe file_with_histograms.root Flux.C+

   1-st position z = 0 R = 732/2 + 25'' = 429.5) south and 6' up from beam line, vertical,                         03/01/13 - 04/03/13
   2-nd position z = 1' before face of FMS (706.3 - 30.48 = 675.82) and 6' = 1.8 m south from beam line,vertical,   04/03/13 - 04/17/13
   3-rd position z = 4' before wall, y = -8'' below beam, x = 53'' south from beam                                 04/17/13 - 05/03/13  9:50 a.m. EST (no signal, HV trip)
   4-th position z = 0 Y = 6' + 15"/2; x = 2.5' from MDT outer most                                                 05/08/13 - 05/22/13 10:00 a.m EST
   5-th position z = +21"; y = floor + 16"; x = on external leg of MDT module #3 from x = 0                         05/23/13 - 06/05/13 22:00   
   6-th position z = +21"; y = floor + 16"; x = on external leg of MDT module #3 from x = 0                         06/06/13 - 06/11/13  8:00     
   root.exe He3.C+ => dump DB to NTuple

2013 Vernier Scan:   
14082048  03/23/13   1-st, South
14098     04/08/13   2-nd, West
14113089  04/23/13   3-rd, East

07/08/2014 add reset in tmed
 */
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TNtuple.h"
#include "Rtypes.h"
#include "TMath.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "Math/GSLMinimizer1D.h"
#include "TROOT.h"
#include "TF1.h"
#include "TProfile.h"
#include "TGraphErrors.h"
#include "TPaveText.h"
#include "TText.h"
#endif
const Char_t *Tags[7] = {"All","South","West","East","North","Bottom","FarAway"};
Double_t      xyzL[7][3] = {
  {   0,   0,   0}, // All
  { 428, 183,   0}, // South
  { 183,   0, 676}, // West
  { 135, -20,-686}, // East
  {-442, 202,   0}, // North
  {  15,-390,  53}, // Bottom
  {-970,-390,-750} // FarAway
};
//________________________________________________________________________________
Double_t fm[6] = {1.18,  9.15, 12.14, 2.34,  0.66, 0.63};  // C0 kHz
Double_t em[6] = {1e-3,  4e-3,  2e-3, 1e-3,  3e-4, 3e-4};  // error
Double_t fe[6] = {0.12,  0.91,  1.21, 0.23,  0.07, 0.06};  // error
Double_t fp[6] = {26.9, 100.8, 112.4, 36.8, 20.8, 7.2};    // Predicted flux Hz/cm^2
Double_t fx[6] = {428, 183, 135, -442,   15, -970};
Double_t fy[6] = {183,   0, -20,  202, -390, -390};
Double_t fr[6] =  {69.5,  70.8,  71.0,  71.2,  71.4,  77.2}; // efficiency
Double_t fre[6] = { 3.6,   2.0,   2.0,   3.6,   4.5,   7.7}; // its error
/* ratio detected with respect to all with Ekin < 250 meV (%)
   69.5 +/- 3.6 
   70.8 +/- 2.0
   71.0 +/- 2.0
   71.2 +/- 3.6
   71.4 +/- 4.5
   77.2 +/- 7.7
*/
//________________________________________________________________________________
Double_t Sigma(Double_t sigma = 5, Double_t A = 14.61, Double_t rho = 1.2e-3) {
  /* sigma: cross section [barn]
     A    : atomic number [g/mol];
     rho  : density [g/cm^3] 
     Sigma: macro cross section [1/cm]
  */
  Double_t Navo = 6.022e23; // [g/mol]
  return (A/(Navo*rho))/(1e-24*sigma);
}
//________________________________________________________________________________
TGraphErrors *GraphFromHistogram(TProfile *hist) {
  if (! hist) return 0;
  Int_t npoints = hist->GetNbinsX();
  Double_t *x = new Double_t[npoints];
  Double_t *y = new Double_t[npoints];
  Double_t *e = new Double_t[npoints];
  Int_t np = 0;
  for (Int_t i = 1; i <=  npoints; i++) {
    if (hist->GetBinEntries(i) < 20) continue;
    e[np] = hist->GetBinError(i+1);
    if (e[np] <= 0) continue;
    y[np] = hist->GetBinContent(i+1);
    if (y[np] < 5*e[np]) continue;
    x[np] = hist->GetBinCenter(i+1);
    np++;
  }
  TGraphErrors *grr = new TGraphErrors(np, x, y, 0, e);
  grr->SetMarkerStyle(hist->GetMarkerStyle());
  grr->SetMarkerColor(hist->GetMarkerColor());
  delete [] x;
  delete [] y;
  delete [] e;
  return grr;
}
//________________________________________________________________________________
TString FluxInCounter(TH1F *hist = 0, Double_t rate = 1e6) {
  // Calculate flux => Hz/cm^2 
  TString t;
  if (! hist) return t;
  TH1D *vz = (TH1D *) gDirectory->Get("Vz");
  Double_t nev = vz->GetEntries();
  cout << "Normalized to " << nev << " events" << endl; 
  Double_t CL = 34.45;
  Double_t CR = (5.08-0.4)/2;
  Double_t V = TMath::Pi()*CR*CR*CL;
  Double_t scale = rate/V/nev;
  //  Double_t flux = scale*hist->GetSumOfWeights();
  //  t = Form("%7.2f [Hz/cm^{2}]",flux);
  Double_t error;
  Double_t flux = scale*hist->IntegralAndError(1,hist->GetNbinsX(),error);
  t += Form(" %7.2f +/- %7.2f",flux,scale*error);
  cout << hist->GetName() << "\t" << t.Data() << endl;
  
  return t;
}
//________________________________________________________________________________
void FluxesInCounter(const Char_t *cname = "c1") {
  gStyle->SetOptStat(0);
  //  unSet date/time for plot
  gStyle->SetOptDate(0);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject(cname);
  if (! c1) c1 = new TCanvas(cname,cname);
  c1->Clear();
  c1->Divide(2,3);
  
  for (Int_t i = 1; i <= 6; i++) {
    TH1F *tot = (TH1F *) gDirectory->Get(Form("Ekin10snHeS%i",i));
    if (! tot) continue;
    Int_t itn = tot->FindBin(TMath::Log10(1e-9*0.250));
    Double_t xc = 1e9*TMath::Power(10.,tot->GetBinLowEdge(itn+1));
    Double_t errT, errR, ErrT, ErrR;
    Double_t totT = tot->IntegralAndError(1,itn,errT);
    ErrT = errT/totT;
    TH1F *reg = (TH1F *) gDirectory->Get(Form("Ekin10overVnHeS%i",i));
    if (! reg) continue;
    Double_t totR = reg->IntegralAndError(1,itn,errR);
    ErrR = errR/totR;
    TLegend *l = new TLegend(0.6,0.7,0.9,0.9);
    reg->SetLineColor(2); reg->SetMarkerColor(2);
    cout << tot->GetName() << "\t e_Kin < " << (Int_t) (1000*xc) << " meV = " << totT << " +/- " << errT << endl;
    cout << reg->GetName() << "\t e_Kin < " << (Int_t) (1000*xc) << " meV = " << totR << " +/- " << errT << endl;
    cout << "Ratio " << 100*totR/totT << " +/- " << 100*TMath::Sqrt(ErrT*ErrT + ErrR*ErrR) << endl;
    tot->Rebin(4); tot->SetStats(0);
    reg->Rebin(4); reg->SetStats(0);
    c1->cd(i);
    TString title("total:       "); title += FluxInCounter(tot);
    tot->SetTitle(Tags[i]);
    if (i == 6) tot->SetXTitle("log_{10}(E_{kin}[GeV])");
    tot->Draw(); l->AddEntry(tot,title);
    title = "detected:"; title += FluxInCounter(reg);
    reg->Draw("same"); l->AddEntry(reg,title);
    l->Draw();
  }   
}
//________________________________________________________________________________
TH2F *Flux(TH2F *hist, Double_t rate = 1e6) {
  // Calculate flux => Hz/cm^2 
  if (! hist) return 0;
  TH1D *vz = (TH1D *) gDirectory->Get("Vz");
  Double_t nev = 0;
  if (! vz ) nev = 4900;
  else       nev = vz->GetEntries();
  cout << "Normalized to " << nev << " events" << endl; 
  TAxis *Z = hist->GetXaxis();
  Int_t nZ = Z->GetNbins();
  Double_t Zmin = Z->GetXmin();
  Double_t Zmax = Z->GetXmax();
  Double_t dZ   = (Zmax - Zmin)/nZ;
  TAxis *R = hist->GetYaxis();
  Int_t nR = R->GetNbins();
  Double_t Rmin = R->GetXmin();
  Double_t Rmax = R->GetXmax();
  Double_t dR   = (Rmax - Rmin)/nR;
  TH2F *h2 = new TH2F(*hist);
  h2->SetName(Form("Norm%s",hist->GetName()));
  TString Title(hist->GetTitle());
  Title.ReplaceAll("flux","flux (Hz/cm^{2})");
  Title.ReplaceAll("* step","");
  h2->SetTitle(Title);
  h2->SetXTitle("Z (cm)");
  h2->SetYTitle("R (cm)");
  h2->SetStats(0);
  for (Int_t j = 1; j <= nR; j++){
    Double_t r1 = Rmin + dR*(j-1);
    Double_t r2 = r1   + dR;
    Double_t V  = TMath::Pi()*(r2*r2 - r1*r1)*dZ;
    Double_t scale = rate/V/nev;//0.6; // Jamie : BBC rate corresponds to ~60% of inelastic cross section
    for (Int_t i = 1; i <= nZ; i++){
      h2->SetBinContent(i,j,scale*h2->GetBinContent(i,j));
    }
  }
  return h2;
}
//________________________________________________________________________________
TH3F *Flux(TH3F *hist, Double_t rate = 1e6) {
  // Calculate flux => Hz/cm^2 
  if (! hist) return 0;
  TH1D *vz = (TH1D *) gDirectory->Get("Vz");
  Double_t nev = 0;
  if (! vz ) nev = 4900;
  else       nev = vz->GetEntries();
  cout << "Normalized to " << nev << " events" << endl; 
  TAxis *Z = hist->GetXaxis();
  Int_t nZ = Z->GetNbins();
  Double_t Zmin = Z->GetXmin();
  Double_t Zmax = Z->GetXmax();
  Double_t dZ   = (Zmax - Zmin)/nZ;
  TAxis *R = hist->GetYaxis();
  Int_t nR = R->GetNbins();
  Double_t Rmin = R->GetXmin();
  Double_t Rmax = R->GetXmax();
  Double_t dR   = (Rmax - Rmin)/nR;
  TAxis *Phi = hist->GetZaxis();
  Int_t nPhi = Phi->GetNbins();
  TH3F *h3 = new TH3F(*hist);
  h3->SetName(Form("Norm%s",hist->GetName()));
  TString Title(hist->GetTitle());
  Title.ReplaceAll("flux","flux (Hz/cm^{2})");
  Title.ReplaceAll("* step","");
  h3->SetTitle(Title);
  h3->SetXTitle("Z (cm)");
  h3->SetYTitle("R (cm)");
  //  h3->SetZTitle("#phi (degree)");
  h3->SetStats(0);
  for (Int_t j = 1; j <= nR; j++){
    Double_t r1 = Rmin + dR*(j-1);
    Double_t r2 = r1   + dR;
    Double_t V  = TMath::Pi()*(r2*r2 - r1*r1)*dZ/nPhi;
    Double_t scale = rate/V/nev;//0.6; // Jamie : BBC rate corresponds to ~60% of inelastic cross section
    for (Int_t i = 1; i <= nZ; i++){
      for (Int_t k = 1; k <= nPhi; k++){
	h3->SetBinContent(i,j,k,scale*h3->GetBinContent(i,j,k));
      }
    }
  }
  return h3;
}
//________________________________________________________________________________
TH2F *Draw(TH3F* h3,const Char_t *option="yx", Double_t zmin = 1, Double_t zmax = -1) {
  // gStyle->SetPalette(8,0)
  if (! h3) return 0;
  TString Title(h3->GetTitle());
  TString Option(option);
  Double_t scale = 1;
  TAxis *z = 0;
  if      (! Option.Contains("z",TString::kIgnoreCase)) {z = h3->GetZaxis();}
  else if (! Option.Contains("x",TString::kIgnoreCase)) {z = h3->GetXaxis();}
  else if (! Option.Contains("y",TString::kIgnoreCase)) {z = h3->GetYaxis();}
  if (! z) return 0;
  Title += "  "; Title += z->GetTitle();
  if (zmin < zmax) {
    Int_t i1 = z->FindBin(zmin);
    Int_t i2 = z->FindBin(zmax);
    z->SetRange(i1,i2);
    scale = 1./(i2 - i1 + 1.0); cout << "scale = " << scale << endl;
    Title += Form(" [%5.0f,%5.0f]",z->GetBinLowEdge(i1),z->GetBinUpEdge(i2));
  } else {
    scale = 1./z->GetNbins(); cout << "scale = " << scale << endl;
    //    Title += " whole range";
  }
  cout << Title.Data() << endl;
  TH2F *h2 = (TH2F *)  h3->Project3D(option);
  if (scale != 1) h2->Scale(scale);
  h2->SetStats(0);
  h2->SetTitle(Title);
  if (Option == "yx") {h2->SetXTitle("Z (cm)"); h2->SetYTitle("R (cm)");h2->GetYaxis()->SetTitleOffset(1.2);}
  return h2;
}
//________________________________________________________________________________
TH1D *Draw(TH2F* h2, Double_t zmin = 1, Double_t zmax = -1) {
  if (! h2) return 0;
  TString Title(h2->GetTitle());
  Double_t scale = 1;
  TAxis *z = h2->GetXaxis();
  if (! z) return 0;
  Title += "  "; Title += z->GetTitle();
  if (zmin < zmax) {
    Int_t i1 = z->FindBin(zmin);
    Int_t i2 = z->FindBin(zmax);
    z->SetRange(i1,i2);
    scale = 1./(i2 - i1 + 1.0); cout << "scale = " << scale << endl;
    Title += Form(" [%5.0f,%5.0f]",z->GetBinLowEdge(i1),z->GetBinUpEdge(i2));
  } else {
    scale = 1./z->GetNbins(); cout << "scale = " << scale << endl;
    //    Title += " whole range";
  }
  TH1D *h1 = (TH1D *)  h2->ProjectionY();
  if (zmax > zmin) h1->SetName(Form("%s_z%i",h1->GetName(),TMath::Nint(zmax)));
  if (scale != 1) h1->Scale(scale);
  h1->SetStats(0);
  h1->SetTitle(Title);
  return h1;
}
//________________________________________________________________________________
TH2F *DrawFlux(TH2F* hist) {
  TH2F *h2 = Flux(hist);
  h2->Draw("colz");
  return h2;
}
//________________________________________________________________________________
TH2F *DrawFlux(TH3F* hist) {
  TH3F *h3 = Flux(hist);
  TH2F *h2 = Draw(h3,"yx");
  h2->Draw("colz");
  return h2;
}
//________________________________________________________________________________
void DrawnSliseZ(Double_t zmin = 1, Double_t zmax = -1,  Double_t rate = 1e6) {
  const Char_t *nNames[3] = {"fluxn","flux100keVn","flux250meVn"};
  const Char_t *nTitles[3] = {"All","E_{kin} > 100 keV","E_{kin} < 250 meV"};
  gStyle->SetOptStat(0);
  //  unSet date/time for plot
  gStyle->SetOptDate(0);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
  if (! c1) c1 = new TCanvas("c1","c1");
  c1->Clear();
  //  c1->SetLogx(1);
  c1->SetLogy(1);
  TH1 *frame = c1->DrawFrame(1,1e-1,1500,1e5);
  frame->SetTitle(Form("Fluxes normalized to %5.2f (MHz) of primary interaction rate at z = %5.1f",rate/1e6, 0.5*(zmin+zmax)));
  frame->SetXTitle("R (cm)");
  frame->SetYTitle("flux (Hz/cm^{2})");
  
  TLegend *leg = new TLegend(0.5,0.6,0.7,0.8);
  leg->Draw();
  for (Int_t i = 0; i <3; i++) {
    TH3F *h3o = (TH3F *) gDirectory->Get(nNames[i]);
    if (! h3o) continue;
    TH3F *h3 = (TH3F *) gDirectory->Get(Form("Norm%s",nNames[i]));
    if (! h3) h3 = Flux(h3o,rate);
    TH2F *h2 = (TH2F *) gDirectory->Get(Form("%s_yx",nNames[i]));
    if (! h2) h2 = Draw(h3);
    TH1D *h1 = (TH1D *) gDirectory->Get(Form("%s_yx_py",nNames[i]));
    if (! h1) h1 = (TH1D *) gDirectory->Get(Form("%s_yx_py_z%i",nNames[i],TMath::Nint(zmax)));
    if (! h1) h1 = Draw(h2,zmin,zmax);
    if (i == 0) h1->SetMinimum(1);
    h1->SetLineStyle(i+1);
    //    if (i == 1) h1->SetLineStyle(5);
    //    if (i == 2) h1->SetLineStyle(9);
    if (i < 2) {
      h1->SetLineColor(i+1);
      h1->SetLineWidth(4);
      h1->SetMarkerColor(i+1);
    } else {
      h1->SetLineColor(i+2);
      h1->SetLineWidth(4);
      h1->SetMarkerColor(i+2);
    }
    h1->Draw("same"); 
    leg->AddEntry(h1,nTitles[i]);
  }
  if (zmin < zmax) {
    Double_t x[7];
    Double_t y[7];
    Double_t ey[7];
    Int_t N = 0;
    TString t;
    for (Int_t l = 1; l < 7; l++) {
      if (xyzL[l][2] >= zmin && xyzL[l][2] <= zmax) {
	x[N] = TMath::Sqrt(xyzL[l][0]*xyzL[l][0] + xyzL[l][1]*xyzL[l][1]);
	Double_t fRc = fm[l-1]*10; // kHz => Hz, count => flux Hz/cm**2
	Double_t eRc = fe[l-1];
	Double_t fRcS = fRc/(fr[l-1]/100.);
	Double_t eRcS = fRcS*TMath::Sqrt(TMath::Power(eRc/fRc,2) + TMath::Power(fre[l-1]/fr[l-1],2));
	y[N] = fRcS;
	ey[N] = eRcS;
	if (! N) t = Tags[l];
	else {t += " and "; t += Tags[l];}
	cout << "Add point " << Tags[l] << " at R " << x[N] << " with " << y[N] << " +/- " << ey[N] << endl;
	N++;
      }
    }
    if (! N) return;
    TGraphErrors *grr = new TGraphErrors(N, x, y, 0, ey);
    grr->SetMarkerColor(4);
    grr->SetLineColor(4);
    grr->Draw("p");
    leg->AddEntry(grr,t);
    }
}
//________________________________________________________________________________
Double_t Rate() {// neutron flux Hz/cm^2
  /* Ekin = 25 meV; Boltzman k = 8.617385e-5; eV/K; */
  //  Double_t ShapingTime = 5e-6; // secs
  Double_t flux = 1;          cout << "flux = " << flux <<  " (Hz/cm^2)" << endl;
#if 1
  Double_t T0 = 273.15; // STP
  Double_t P0 = 0.986;  //
  Double_t T  = T0 + 25;           cout << "Tempereture (T) = " << T << " (K)" << endl;
#else
  /* Helium-3 boils at 3.19 kelvin Hthe density when it is at its boiling point: 59 gram / liter  at a pressure of one atmosphere. */
  Double_t T0 = 3.19;
  Double_t P0 =  1.00;
  Double_t T = 273.15 + 25;           cout << "Tempereture (T) = " << T << " (K)" << endl;
  //  Double_t dens0 = 59e-3;
  Double_t dens0 = 125e-3; // g/cm^3 @ 21.1 C; http://www.lindeus.com/internet.lg.lg.usa/en/images/he3-helium-3138_88295.pdf
#endif
  Double_t k = 8.617385e-5;// Boltzman eV/K 
  Double_t Ekin0 = 25.3e-12; //[GeV] 
  Double_t Ekin  = 3./2.*k*T*1e-9 ; 
  cout << "Thermal neutron kinetic energy (kT) = " << Ekin*1e12 << " (meV)" << endl;
  Double_t mass = 0.93956563;//*GeV;
  Double_t v0 = TMath::Sqrt(2*Ekin0/mass); cout << "Thermal neutron verlocity v0 = " << v0 << "(C)" << endl;
  // http://www.nndc.bnl.gov/exfor/servlet/X4sMakeX4 http://www.nndc.bnl.gov/exfor/x4data/E4R53712_e4.gif?ID=121826803
  Double_t xsection0 = 5400;   
  Double_t xsection = xsection0*TMath::Sqrt(Ekin0/Ekin);
  cout << "Thermal neutron cross section (n + He3 -> p + H3) = " << xsection0 << "*v0/v  = " << xsection << " (barn)" << endl; 
  // cm^2 at v0; => xsection = xsection0*(v0/v);
  // Centronic 31He3/304/25e;
  //  cout << " Centronic 31He3/304/25e counter " << endl;
  cout << "RS_P4-1614-204 GE Power System Reuter-Stokes" << endl;
  //  Double_t V = 31*TMath::Pi()*2.5*2.5/4; // 10; // cm^3 volume of counter
  //          sensitive length           2" Dia and 13.56" length
  Double_t Length = 13.56*2.54;
  Double_t Radius = (2*2.54 - 1.5)/2;
  Double_t V = Length*TMath::Pi()*Radius*Radius;   
  cout << "Volume = " << V << " (cm^3)" << endl;
  //  Double_t P = 304./76.; cout << "Pressure = " << P << " (ATM)" << endl;// 10; // pressure ATM
  Double_t P = 3.; cout << "Pressure = " << P << " (ATM)" << endl;// 10; // pressure ATM
  Double_t l = V*flux; cout << "Total neutron path length in counter = V * flux = " << l << " (cm * Hz)" << endl; 
  Double_t N = l*xsection; cout << " N = l * xsection = " << N << " (cm^3 * Hz)" << endl;
  // Mixture : 1  
  Double_t AHe3 = 3;
#if 1
  Double_t dens = AHe3/24.465294e3/((P0/P)/(T0/T)); // 0.18e-3*P; // g/cm^3 for He3  @ 273 K
#else
  Double_t dens = dens0*(P/P0)*(T0/T);
#endif
  cout << "density = " << dens << endl;
  Double_t Avo = 1e-24*TMath::Na();// 1/mol
  Double_t cmIbarn = AHe3/(Avo*dens); // [g/mol] / ([1/mol]  * [g/(cm*barn)]) =>  cm*barn;
  cout << "cm * barn  = " << cmIbarn << " (cm * barn)" << endl; 
  Double_t Lint = cmIbarn / xsection ; cout << "Interaction Length = " << Lint << endl;
  Double_t rate = l/Lint; // Hz
  Double_t ratepern = rate/flux;
  cout << "counts = " << ratepern << " per 1 n (Hz/cm^2)" << endl;
  cout << "rate @ " << flux << " = " << rate << " (Hz)" << endl;
return rate;
}
//________________________________________________________________________________
Double_t fDeadTime = 1./1.52931e+05; // 6.5 usec
Double_t rate = 0;
ROOT::Math::GSLMinimizer1D *fminBrent = 0;
//________________________________________________________________________________
Double_t BBCcor(Double_t rBBC) {// nonextending or nonparalyzable: n = r/(1 - r/fDead) 
  static Double_t fDead = 9.383e6;// == RHIC clock 
  if (rBBC >= fDead) return -1;
  return rBBC/(1. - rBBC/fDead);
}
//________________________________________________________________________________
Double_t func(Double_t n) {
  Double_t dev = rate - n*TMath::Exp(-n*fDeadTime);
  return dev*dev;
}
//________________________________________________________________________________
Double_t MDTcor(Double_t rMDT) {// extending or paralyzable : r = n * exp(-tau*n);
  Double_t nMDT;
  if (! fminBrent) fminBrent = new ROOT::Math::GSLMinimizer1D();
  rate = rMDT;
  fminBrent->SetFunction(&func,rMDT,0.9*rMDT,100*rMDT);
  if (! fminBrent->Minimize(10,0.1,0.1)) {
    nMDT = -1;
  } else {
    nMDT = fminBrent->XMinimum();
  }
  return nMDT;
}
//________________________________________________________________________________
Double_t CorCoinRate(Double_t N_EW, Double_t N_E, Double_t N_W) {
  Double_t N_BC = 9.383e6*111./120;//* no. of bunch crossings
  Double_t p_EW = N_EW/N_BC; if (p_EW > 1) p_EW = 1;
  Double_t p_E  = N_E /N_BC; if (p_E  > 1) p_E  = 1;
  Double_t p_W  = N_W /N_BC; if (p_W  > 1) p_W  = 1;
  if (p_EW <= 0) return -1;
  if (p_W < p_EW || p_E < p_EW) return -1;
  if (p_E*p_W > p_EW) return -1;
  if (p_E + p_W > 1 + p_EW) return -1;
  Double_t R = 1 - (p_E*p_W - p_EW)/( p_E + p_W -1 - p_EW);
  Double_t r = 0;
  if (R > 0) {
    r = - TMath::Log(R);
  } else {
    return -1;
  }
  return N_BC*r;
}
//________________________________________________________________________________
Double_t ZdcCoinRate(Double_t N_EW, Double_t N_E, Double_t N_W) {
  /*  cross =  2.81 mb; #ZDC from van der Meer scan, Apr. 8, from Jamie's getSampelFrac.pl 
      in.elastic cross sectiopn pp @ 510 GeV = 50 mb
   */
  return 50./2.81*CorCoinRate(N_EW, N_E, N_W);
}
//________________________________________________________________________________
Double_t BbcCoinRate(Double_t N_EW, Double_t N_E, Double_t N_W) {
  /*  cross =  2.81 mb; #ZDC from van der Meer scan, Apr. 8, from Jamie's getSampelFrac.pl 
      in.elastic cross sectiopn pp @ 510 GeV = 50 mb
   */
  return 1./0.6*CorCoinRate(N_EW, N_E, N_W);
}
//________________________________________________________________________________
Double_t ffunc(Double_t *x, Double_t */* p=0 */) {
  return MDTcor(x[0]);
}
//________________________________________________________________________________
void DrawNeutronZdc() {
  TNtuple *THe32013 = (TNtuple *) gDirectory->Get("THe32013");
  if (! THe32013) return;
  gStyle->SetOptStat(0);
  //  unSet date/time for plot
  gStyle->SetOptDate(0);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
  if (! c1) c1 = new TCanvas();
  c1->Clear();
  TH1 *frame = c1->DrawFrame(0,0,12,60);
  //  frame->SetTitle("Thermal Neutron flux versus event rate (estimated from ZDC)");
  frame->SetTitle("");
  frame->SetXTitle("Event rate (MHz)");
  frame->SetYTitle("He^{3} counter rate (kHz)");
  const Char_t *Dates[7] = {"",                                      // All
			    " && date > 20130329&&date <= 20130403", // South
			    " && date > 20130404&&date <  20130417", // West
			    " && date > 20130417&&date <  20130503", // East
			    " && date > 20130508&&date <  20130522", // North
			    " && date > 20130522&&date  < 20130605", // Bottom
			    " && date > 20130605"                    // FarAway
  };
  THe32013->SetMarkerStyle(1);
  TLegend *l = new TLegend(0.1,0.5,0.4,0.9);  l->Draw();
  Int_t mstyle[6] = {20,21,24,25,22,23};
  Int_t mcolor[6] = { 1, 2, 4, 6, 1, 2};
  for (Int_t i = 1; i < 7; i++) {
    Int_t col = mcolor[i-1];
    //    if (col >= 5) col++;
    THe32013->SetMarkerColor(col);
    THe32013->Draw(Form("1e-3*MTD:1e-6*ZdcCoinRate(ZDCEandWnokiller,ZDCEnokiller,ZDCWnokiller)>>MZ%s_1(120,1e-5,12,1000,0,1)",Tags[i]),
		   Form("ZDCEandWnokiller>5000&&ZdcCoinRate(ZDCEandWnokiller,ZDCEnokiller,ZDCWnokiller)>0&&ZdcCoinRate(ZDCEandWnokiller,ZDCEnokiller,ZDCWnokiller)/ZDCEandWnokiller>10&&MTD<6e4&&MTD>1%s",Dates[i]),"sameprof");
    
    //    TH2D *MZ = (TH2D *) gDirectory->Get(Form("MZ%s",Tags[i]));
    TProfile *MZ = (TProfile *) gDirectory->Get(Form("MZ%s_1",Tags[i]));
    if (! MZ) continue;
    MZ->SetLineColor(col); 
    if (! i) {l->AddEntry(MZ,Tags[i]); continue;}
    //    TProfile *MZ_1 = MZ->ProfileX();
    TProfile *MZ_1 = MZ;
    //    MZ_1->SetMarkerStyle(20+i-1);
    MZ_1->SetMarkerStyle(mstyle[i-1]);
    MZ_1->SetMarkerSize(1.5);
    if (! MZ_1) continue;
    MZ_1->SetMarkerColor(col);
    //    MZ_1->SetMarkerStyle(20);
    Double_t xmin =  0.0;
    Double_t xmax = 12.0;
    if (i == 3) xmax = 10.0;
//     if (i == 1) xmax = 6;}
//     if (i == 2) {xmin = 2.5;xmax = 4;}
//     if (i == 3) {xmin = 2.2;xmax = 3;}
//     if (i == 4) {xmin = 3.0;xmax = 5;}
//     if (i == 5) {xmin = 4.6;xmax = 7;}
    TGraphErrors *gr = GraphFromHistogram(MZ_1);
    TF1 *f = new TF1(Form("f%s",Tags[i]),"[0]*x+[1]*x*x",xmin,xmax);
    f->SetLineStyle(i);
    //    MZ_1->SetLineStyle(i);
    f->SetLineColor(col);
    gr->Fit(f,"r+rob=0.75","",xmin,xmax);
    f->SetMarkerStyle(mstyle[i-1]);
    f->SetMarkerColor(col);
    f->Draw("same");
    MZ_1->GetListOfFunctions()->Add(f);
    //    MZ_1->Draw("same");
    //    f->Draw("same");
    //    l->AddEntry(MZ_1,Form("%s: %5.1f #pm %3.1f therm. neutron / 1 MHz min.bias.",Tags[i],f->GetParameter(0),f->GetParError(0)));
    //    l->AddEntry(MZ_1,Form("%s: %5.1f [Hz/cm^{  2} / 1 MHz min.bias.]",Tags[i],f->GetParameter(0)));
    //    l->AddEntry(MZ_1,Form("%-7s:%5.2f",Tags[i],f->GetParameter(0)));
    l->AddEntry(f,Form("%-7s:%5.2f",Tags[i],f->GetParameter(0)));
  }
  c1->Clear();
  frame = c1->DrawFrame(0,0,12,60);
  //  frame->SetTitle("Measured Thermal Neutron flux versus event rate (estimated from ZDC)");
  frame->SetXTitle("R (MHz)");
  frame->SetYTitle("C (kHz)");
  l->Draw();
  for (Int_t i = 0; i < 7; i++) {
//     TH2D *MZ = (TH2D *) gDirectory->Get(Form("MZ%s",Tags[i]));
//     if (! MZ) continue;
//     MZ->SetStats(0);
//     MZ->Draw("same");
    if (! i) {continue;}
    //    TProfile *MZ_1 = (TProfile *) gDirectory->Get(Form("%s_pfx",MZ->GetName()));
    TProfile *MZ_1 = (TProfile *) gDirectory->Get(Form("MZ%s_1",Tags[i]));
    if (! MZ_1) continue;
    MZ_1->SetStats(0);
    MZ_1->Rebin(4);
    MZ_1->Draw("same");
  }
}
//________________________________________________________________________________
Float_t Dtime(Int_t utime) {
  if (utime > 1366766045) return utime - 1366766045;
  if (utime > 1365462700) return utime - 1365462700;
  return utime - 1364076302;
}
//________________________________________________________________________________
TH1D *ConvExpBin(TH1D *o = 0) {
  if (! o) return 0;
  Int_t nx = o->GetNbinsX();
  Double_t *x = new Double_t[nx+1];
  x[0] = 1e-9*TMath::Power(10.,o->GetXaxis()->GetBinLowEdge(1));
  for (Int_t i = 1; i <= nx; i++) {
    x[i] = 1e-9*TMath::Power(10.,o->GetXaxis()->GetBinUpEdge(i));
  }
  TH1D *n = new TH1D(Form("%sE",o->GetName()),o->GetTitle(),nx,x);
  for (Int_t i = 1; i <= nx; i++) {
    n->SetBinContent(i,o->GetBinContent(i)/(x[i]-x[i-1]));
  }
  delete [] x;
  return n;
}
//________________________________________________________________________________
void TofGCut() {
  const Char_t *Files[5] = {"fluxZ.1ms.root","fluxY.10s.root","fluxU.1ks.root", "fluxU.1ks.root", 0};
  const Char_t *Names[5] = {"All, TofMax = 1e-3 s", "All, TofMax = 10 s", "All, TofMax = 1e3 s","Thermal, TofMax = 1e3 s"};
  gStyle->SetOptStat(0);
  //  unSet date/time for plot
  gStyle->SetOptDate(0);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
  if (! c1) c1 = new TCanvas();
  c1->Clear();
  c1->SetLogx(1);
  c1->SetLogy(1);
  TH1 *frame = c1->DrawFrame(1e-10,1e-5,1e4,1e13);
  frame->SetTitle("Neutron time of flight for different TofMax cut");
  frame->SetXTitle("time (s)");
  frame->SetYTitle("dN/dt (neutron/s/event)");
  TLegend *l = new TLegend(0.6,0.6,0.9,0.85);
  l->Draw();
  for (Int_t i = 1; Files[i]; i++) {
    TFile *f = TFile::Open(Files[i]);
    if (! f) continue;
    TH2F *tofg = (TH2F*) f->Get("tofg");
    if (! tofg) continue;
    Int_t p = 13;
    if (i == 3) p = 51;
    TH1D *n = tofg->ProjectionX(Form("n%3s",Names[i]),p,p);
    if (! n) continue;
    TH1D *vz = (TH1D *) f->Get("Vz");
    if (! vz) continue;
    n->Scale(1./vz->GetEntries());
    TH1D *nC = ConvExpBin(n);
    if (! nC) continue;
    //    nC->SetMarkerColor(i+1);
    //    nC->SetLineColor(i+1);
    nC->Draw("same");
    l->AddEntry(nC,Names[i]);
  }
}
//________________________________________________________________________________
void nToFc(Int_t p = 13, TString opt = "hist") {
  gStyle->SetOptStat(0);
  //  unSet date/time for plot
  gStyle->SetOptDate(0);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
  if (! c1) c1 = new TCanvas();
  if (! opt.Contains("same")) {
    c1->Clear();
    c1->SetLogx(1);
    c1->SetLogy(1);
  }
  TFile *f = gFile;
  TH2F *tofg = (TH2F*) f->Get("tofg");
  if (! tofg) return;
  TString s;
  if (p == 51) s = "Therm";
  TH1D *n = tofg->ProjectionX(Form("nAll%s",s.Data()),p,p);
  if (! n) return;
  TH1D *vz = (TH1D *) f->Get("Vz");
  if (! vz) return;
  n->Scale(1./vz->GetEntries());
  TH1D *nC = ConvExpBin(n);
  if (! nC) return;
  if (p == 51) {nC->SetLineColor(2); nC->SetLineStyle(2);}
  nC->SetStats(0);
  //  nC->SetTitle("Neutron's time of flight");
  nC->SetTitle("");
  nC->SetXTitle("time (s)");
  nC->SetYTitle("dN/dt (n/s/event)");
  nC->SetLineWidth(4.);
  //  nC->SetLineStyle(2);
  nC->Draw(opt.Data());
  TF1 *expo = (TF1 *) gROOT->GetListOfFunctions()->FindObject("expo");
  if (expo) {
    expo->SetLineColor(1);
    expo->SetLineStyle(1);
    nC->Fit(expo,"er","same",1e-2,2e-1);
  } else {
    nC->Fit("expo","er","same",1e-2,2e-1);
  }
  //  expo->SetLineColor(4);
  expo = (TF1 *) gROOT->GetListOfFunctions()->FindObject("expo");
  expo->SetLineColor(4);
  expo->SetLineStyle(2);
  nC->Fit(expo,"er+","same",1e1,1e4);
}
//________________________________________________________________________________
void nToF() {
  nToFc(13,"hist");
  nToFc(51,"histsame");
}
//________________________________________________________________________________
void Results() {
  TH1D *meas = new TH1D("meas","Measured thermal neutron flux in STAR hall @ 1 MHz pp 510 GeV minimum biased event rate", 6, 0.5, 6.5);
  TH1D *pred = new TH1D("pred","Predicted thermal neutron flux", 6, 0.5, 6.5);
  for (Int_t i = 0; i < 6; i++) {
    meas->SetBinContent(i+1,fm[i]);
    meas->SetBinError(i+1,fe[i]);
    meas->GetXaxis()->SetBinLabel(i+1,Tags[i+1]);
    pred->SetBinContent(i+1,fp[i]);
  }
  gStyle->SetOptDate(0);
  meas->SetYTitle("flux (Hz/cm^{2} )");
  meas->SetXTitle("Position");
  meas->SetStats(0);
  meas->SetMarkerStyle(20);
  pred->SetLineColor(2);
  pred->SetMarkerColor(2);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
  if (! c1) c1 = new TCanvas();
  c1->Clear();
  meas->Draw();
  pred->Draw("same");
  TLegend *l = new TLegend(0.6,0.7,0.9,0.8);
  l->AddEntry(meas,"Measured flux");
  l->AddEntry(pred,"Predicted flux");
  l->Draw();
}
//________________________________________________________________________________
void Ratio() {
  TH1D *meas = new TH1D("Ratio","Ratio of measured thermal neutron flux to estimated one", 6, 0.5, 6.5);
  for (Int_t i = 0; i < 6; i++) {
    meas->SetBinContent(i+1,fm[i]/fp[i]);
    meas->SetBinError(i+1,fe[i]/fp[i]);
    meas->GetXaxis()->SetBinLabel(i+1,Tags[i+1]);
  }
  gStyle->SetOptDate(0);
  meas->SetYTitle("Ratio");
  meas->SetXTitle("Position");
  meas->SetStats(0);
  meas->SetMarkerStyle(20);
  TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
  if (! c1) c1 = new TCanvas();
  c1->Clear();
  meas->Draw();
}
//________________________________________________________________________________
TGraphErrors *RatioR() {
  Double_t r[6], re[6], rr[6], rx[6];
  for (Int_t i = 0; i < 6; i++) {
    r[i] = fm[i]/fp[i];
    re[i] = 0.1;
    rx[i] = 1.0;
    rr[i] = TMath::Sqrt(fx[i]*fx[i] + fy[i]*fy[i]);
  }
  TGraphErrors *gr = new TGraphErrors(6,rr, r, rx, re);
  gr->Draw("axp");
  return gr;
}
//________________________________________________________________________________
void DrawEkin() {
  gStyle->SetOptStat(0);
  //  unSet date/time for plot
  gStyle->SetOptDate(0);
  TH1D *vz = (TH1D *) gDirectory->Get("Vz");
  Double_t nev;
  if (! vz ) nev = 4900;
  else       nev = vz->GetEntries();
  cout << "Normalized to " << nev << " events" << endl; 
  TH1F *Ekin10sn = (TH1F *) gDirectory->Get("Ekin10sn");
  if (! Ekin10sn) return;
  TH1F *Ekin10overVn = (TH1F *) gDirectory->Get("Ekin10overVn");
  if (! Ekin10overVn) return;
  Ekin10sn->Scale(1./nev);
  Ekin10overVn->Scale(1./nev);
  Ekin10sn->SetLineWidth(4);
  Ekin10sn->SetTitle("");
  Ekin10sn->SetXTitle("log_{10}(E_{kin}[GeV])");
  Ekin10sn->Draw("hist");
  Ekin10overVn->SetLineWidth(4);
  Ekin10overVn->SetLineStyle(2);
  Ekin10overVn->SetLineColor(2);
  Ekin10overVn->Draw("histsame");
}
//________________________________________________________________________________
void fluxTable(TH3F *h3o) {
  if (! h3o) return;
  TH3F *h3 = Flux(h3o);
#if 0
  Int_t l = -1;
  for (Int_t i = 1; i < 7; i++) {
    if (location == Tags[i]) {l = i; break;}
  }
  if (l < 0) return;
#endif
  cout  << "\\begin{center}" << endl << "\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}" << endl << "\\hline" << endl 
       << "{\\footnotesize  Location}&{Z(cm)}&{r(cm)}&{$\\phi (^{\\circ})$}" << endl 
       << "&{\\footnotesize \\textbf{$C_{0}$} (kHz)}&{\\footnotesize $\\epsilon (\\%)$}&{\\footnotesize  RC $(Hz/cm^2)$  }&{\\footnotesize   MC$(Hz/cm^2)$  }&{\\footnotesize MC/RC } \\\\" << endl 
       << "\\hline"        << endl;
  for (Int_t l = 1; l < 7; l++) {
    Double_t z   = xyzL[l][2];
    Double_t rho = TMath::Sqrt(xyzL[l][0]*xyzL[l][0] + xyzL[l][1]*xyzL[l][1]);
    Double_t phi = TMath::RadToDeg()*TMath::ATan2(xyzL[l][1],xyzL[l][0]);
    Int_t i = h3->GetXaxis()->FindBin(z);
    Int_t j = h3->GetYaxis()->FindBin(rho);
    Int_t k = h3->GetZaxis()->FindBin(phi);
    Double_t fMc = h3->GetBinContent(i,j,k);
    Double_t eMc = h3->GetBinError(i,j,k);
//    cout << Form("%7s",Tags[l]) << " f(" << i << "," << j << "," << k << ") = " << fMc << " +/- " << eMc << endl; 
    Double_t S = 100;             // Sensitivity [counts/(1 Hz/cm^2)]
    Double_t fRc = fm[l-1]*1e3/S; // kHz => Hz, count => flux Hz/cm**2
    Double_t eRc = 0.1*fRc;       // Seinsitivity S = 100 +/- 10 counts/(1 Hz/cm^2);
    Double_t fRcS = fRc/(fr[l-1]/100.); // % => efficiency
    Double_t eRcS = fRcS*TMath::Sqrt(TMath::Power(eRc/fRc,2) + TMath::Power(fre[l-1]/fr[l-1],2));
    Double_t r   = fMc/fRcS;
    Double_t er  = r*TMath::Sqrt(TMath::Power(eMc/fMc,2) + TMath::Power(eRcS/fRcS, 2));
//     cout << Form("{\\footnotesize %7s}&{\\footnotesize  %6.1f }&{\\footnotesize %6.1f }&{\\footnotesize  %6.1f }"
// 		 "&{\\footnotesize %5.1f $\\pm$ %4.1f }&{\\footnotesize %5.1f $\\pm$ %4.1f }"
// 		 "&{\\footnotesize %5.1f $\\pm$ %4.1f }&{\\footnotesize  %5.1f $\\pm$ %4.1f }&{\\footnotesize %5.2f $\\pm$ %4.2f} \\\\",
// 		 Tags[l],z,rho,phi,fm[l-1],em[l-1], fr[l-1], fre[l-1],fRcS, eRcS, fMc, eMc, r, er) << endl;
    cout << Form("{\\footnotesize %7s}&{\\footnotesize  %6.1f }&{\\footnotesize %6.1f }&{\\footnotesize  %6.1f }"
		 "&{\\footnotesize %5.1f}&{\\footnotesize %5.1f $\\pm$ %4.1f }"
		 "&{\\footnotesize %5.1f $\\pm$ %4.1f }&{\\footnotesize  %5.1f $\\pm$ %4.1f }&{\\footnotesize %5.2f $\\pm$ %4.2f} \\\\",
		 Tags[l],z,rho,phi,fm[l-1], fr[l-1], fre[l-1],fRcS, eRcS, fMc, eMc, r, er) << endl;
  }
  cout <<"\\hline" << endl << "\\end{tabular}" << endl << "\\end{center}" << endl << "\\label{table}" << endl << "\\end{table}" << endl;
}
//________________________________________________________________________________
TH2F *Flux(Int_t part = -1, Int_t type = -1, Double_t RateN = 1e6, Bool_t plot = kTRUE) {
  TH2F *h2D = 0;
  if (! gFile) return h2D;
  // Define color map
  gStyle->SetPalette(55);
  enum {Nregions = 7, Nparts = 5, NH1T = 3, NH1TE = NH1T + 1, NH2T = 9};
  struct Name_t {
    const Char_t *Name;
    const Char_t *Title;
  };
  struct NameX_t {
    Name_t name;
    Int_t nX;
    Double_t xMin, xMax;
    Int_t nY;
    Double_t yMin, yMax;
  };
  Name_t Particles[Nparts+1] = {
    {"", "#pi/K/p and others    "}, // 0
    {"g","#gamma"},             // 1
    {"e","e^{#pm}"},            // 2
    {"m","#mu^{#pm}"},          // 3
    {"n","neutron"},            // 4
    {"A","charged particles and #gamma"} //5 
  };
  NameX_t Types1[NH1TE] = {
    {{"Ekin10"    ,"Log_{10}(GEKIN) for %s for %s"                 }, 340, -14., 3.0, 0, 0, 0},  //5 -> 0 300
    {{"Ekin10s"   ,"Log_{10}(GEKIN) for %s at step for %s weighted with L"   }, 340, -14., 3.0, 0, 0, 0},  //6 -> 1 320
    {{"Ekin10V"   ,"Log_{10}(GEKIN) for %s at production Vx for %s"}, 340, -14., 3.0, 0, 0, 0},   //7 -> 2 400
    {{"Ekin10overV","Log_{10}(GEKIN) for %s at step weight with L/v for %s"}, 340, -14., 3.0, 0, 0, 0} 
  };
  Double_t xstep =  5;
  Double_t ystep =  2;
  Double_t xmax  = 2000, xmin = - xmax;
  Double_t ymax  = 1500, ymin =      0;
  Int_t nx = (xmax - xmin)/xstep;
  Int_t ny = (ymax - ymin)/ystep;
  NameX_t Types2[NH2T] = {
    {{"flux"      ,"flux from %s * step "                    }, nx, xmin, xmax, ny, ymin, ymax},  //0 100
    {{"flux100keV","flux from %s * step E_{kin} > 100 keV "  }, nx, xmin, xmax, ny, ymin, ymax},  //1 800
    {{"flux250meV","flux from %s * step E_{kin} < 250 meV "  }, nx, xmin, xmax, ny, ymin, ymax},  //2 500
    {{"entries"   ,"entries from %s "                        }, nx, xmin, xmax, ny, ymin, ymax},  //3 900
    {{"VxProd"    ,"Vertex Production of %s "                }, nx, xmin, xmax, ny, ymin, ymax},  //4 200
    {{"dose"      ,"dose  from %s "                          }, nx, xmin, xmax, ny, ymin, ymax},  //8 ->5  600
    {{"star"      ,"star density "                           }, nx, xmin, xmax, ny, ymin, ymax},  //9 ->6 700
    {{"RD"        ,"Residual Dose "                          }, nx, xmin, xmax, ny, ymin, ymax},  //0 ->7 701
    {{"DepEnergy" ,"Deposited energy at step (keV) "         }, nx, xmin, xmax, ny, ymin, ymax}
  };
  Int_t p1 = 0, p2 = Nparts;
  if (part >= 0) {p1 = p2 = part;}
  Int_t t1 = 0, t2 = NH2T - 1;
  if (type >= 0) {t1 = t2 = type;}
  for (Int_t p = p1;  p <= p2; p++) {
    for (Int_t t = t1; t <= t2; t++) {
      if (t != 5 && p == Nparts) continue;
      //      if (t == 3 || t == 5 || t == 6 || t == 7) continue;
      if (t == 3 || t == 6 || t == 7) continue;
      TString Name;
      if (p == Nparts) {
	TH1 *hsum = 0;
	TString NameA(Types2[t].name.Name); NameA += Particles[p].Name;
	TString TitleA(Particles[p].Title);
	for (Int_t pp = 0; pp < 4; pp++) {
	  Name = Types2[t].name.Name; Name += Particles[pp].Name;
	  TH1 *h = (TH1 *) gDirectory->Get(Name);
	  if (! h) {cout << "Histogram\t" << Name.Data() << " has not been found" << endl; continue;}
	  if (! hsum) {
	    hsum = (TH1 *) h->Clone(NameA);
	    hsum->SetTitle(TitleA);
	  } else {
	    hsum->Add(h);
	  }
	}
      }
      Name = Types2[t].name.Name; Name += Particles[p].Name; 
      Double_t rate = RateN; // 1 MHz
      if (t == 5) {
	if (RateN == 1e6) 
	  rate = 1e-3*rate*1e7/(6.2415e10); // 1 MHz * 1e7 seconds; 1 krad = 1e-3*6.2415e10 MeV/kg = 6.2415e10 keV/g;
	else 
	  rate = 1e-3*rate/(6.2415e10); // per run ; 1 krad = 1e-3*6.2415e10 MeV/kg = 6.2415e10 keV/g;
      }
      TH1 *h = (TH1 *) gDirectory->Get(Name);
      if (! h) {cout << "Histogram\t" << Name.Data() << " has not been found" << endl; continue;}
      Int_t dim = h->GetDimension();
      if (dim != 2 && dim != 3) return h2D;
      TH2F *h2 = 0;
      if (dim == 3) {
	TH3F *h3 = (TH3F *) h;
	h2 = (TH2F *)  h3->Project3D("yx");
      } else {
	h2 = (TH2F *) h;
      }
      if (h2->GetEntries() < 100) continue;
      h2D = Flux(h2,rate);
      if (! h2D) continue;
      TString Title(h2D->GetTitle());
      Title.ReplaceAll(" yx projection","");
      Title.ReplaceAll("#pi/K/p and others","charged hadrons");
      if (t == 5) {
	if (p == Nparts) {
	  if (RateN == 1e6) 
	    Title.ReplaceAll("charged particles and #gamma",Form("Dose[krad/10^{6+7} events = 1 MHz * 1 year] from %s", Particles[p].Title));
	  else {
	    Title.ReplaceAll("charged particles and #gamma",Form("Dose[krad/run] from %s", Particles[p].Title));
	  }
	} else {
	  if (RateN == 1e6) 
	    Title.ReplaceAll("dose","Dose[krad/10^{6+7} events = 1 MHz * 1 year]");
	  else {
	    Title.ReplaceAll("dose","Dose[krad/run]");
	  }
	}
      } else {
	if (RateN == 1e6) {
	  Title.ReplaceAll("(keV)","(keV cm^{    -3} / 1 MHz)");
	  Title.ReplaceAll("and #phi","");
	  Title.ReplaceAll("(Hz/cm^{2})","(Hz/cm^{   2} / 1MHz)");
	  Title.ReplaceAll("Vertex Production of","Vertex Production (1 cm^{  -3} / 1 MHz) of  ");
	  Title.ReplaceAll("from","from  ");
	} else {
	  Title.ReplaceAll("flux (Hz/cm^{2})","Fluence (cm^{-2})");
	  Title.ReplaceAll("1MHz","run");
	  Title.ReplaceAll("(keV)","(keV cm^{    -3} / run)");
	  Title.ReplaceAll("and #phi","");
	  Title.ReplaceAll("(Hz/cm^{2})","(cm^{-2} / run)");
	  Title.ReplaceAll("Vertex Production of","Vertex Production (1 cm^{  -3} / run) of  ");
	  Title.ReplaceAll("from","from  ");
	}
      }
      if (Title.Contains("Dose[krad/run] from Dose[krad/run]")) {
	cout << "BOT ONO" << endl;
      }
      h2D->SetTitle(Title);
      if (plot) {
	TCanvas *c1 = new TCanvas(Name,Name,2046,130,1068,596);
	//      TCanvas *c1 = new TCanvas(Name,Name);
	c1->SetLogz(1);
	Int_t ix1 = h2D->GetXaxis()->FindBin(-1000.);
	Int_t ix2 = h2D->GetXaxis()->FindBin( 1000.);
	h2D->GetXaxis()->SetRange(ix1,ix2);
	Int_t iy2 = h2D->GetYaxis()->FindBin(500.);
	h2D->GetYaxis()->SetRange(1,iy2);
	//      h2D->SetMinimum(1.);
	h2D->SetContour(100);
	h2D->SetBit(TH1::kNoTitle);
	h2D->Draw("colz");
	TPaveText *pt = new TPaveText(0.1670082,0.9037037,0.8668033,0.9888889,"blNDC");
	pt->SetName("title");
	pt->SetBorderSize(1);
	pt->SetFillColor(10);
	TText *text = pt->AddText(Title.Data());
	pt->SetTextSize(0.04);
	pt->Draw();
	c1->Update();
	TString pngName(c1->GetName());
	pngName += ".png";
	c1->SaveAs(pngName);
	pngName.ReplaceAll(".png",".root");
	c1->SaveAs(pngName);
	delete c1;
      }
    }
  }
  return h2D;
}
//________________________________________________________________________________
void pp200r2015(Int_t part = -1, Int_t type = -1) {
  // 2015 pp 200
  Double_t sigma_inel = 45; // mb @ 200 GeV
  Double_t L_integrated = 180; //  pb^1 ; RHIC STAR + PHENIX = 382; // pb^-1;
  Double_t Norm = sigma_inel * 1e-3 * L_integrated /(1e-12);
  cout << "run 2015 pp 200 = " << Norm << " events" << endl;
  Flux(part,type, Norm);
}
//________________________________________________________________________________
void pAu200r2015(Int_t part = -1, Int_t type = -1) {
  // 2015 pAu 200
  Double_t sigma_inel = 1.5;// b Jamie 06/24/15;       1.5*35; // mb @ 200 GeV
  Double_t L_integrated = 620e-3; //  nb^1 => pb^1 ; 
  Double_t Norm = sigma_inel * L_integrated /(1e-12);
  cout << "run 2015 pAu 200 = " << Norm << " events" << endl;
  Flux(part,type, Norm);
}
//________________________________________________________________________________
void pAl200r2015(Int_t part = -1, Int_t type = -1) {
  // 2015 pAl 200
  Double_t sigma_inel = 0.35;// b Jamie 06/24/15;       1.5*35; // mb @ 200 GeV
  Double_t L_integrated = 1.95; //  pb^1 ; 
  Double_t Norm = sigma_inel * L_integrated /(1e-12);
  cout << "run 2015 pAu 200 = " << Norm << " events" << endl;
  Flux(part,type, Norm);
}
//________________________________________________________________________________
Double_t geneSC(Double_t *x, Double_t *p) {
  Double_t r = x[0];
  return p[0]*(3191/(r*r) +122.5/r - 0.395);
}
//________________________________________________________________________________
TF1 *GeneSC() {// TF1 * f = new TF1("f",[&](double *x,double *){ return g->Eval(x[0]); },0,10,0); ROOT 6
  TF1 *f = new TF1("GeneSC",geneSC, 50, 200, 1);
  f->SetParameter(0,1.);
  return f;
}
//________________________________________________________________________________
void SpaceCharge() {
  TF1 *Gene = GeneSC();
  TCanvas *csc = new TCanvas("Space Charge","Space Charge");
  csc->Divide(3,4);
  TH2F *hT = 0; 
  TH2F *hC = Flux(0,8,1e6,kFALSE); hT = new TH2F(*hC); hT->SetName("Total"); hT->SetTitle("Total");
  TH2F *hG = Flux(1,8,1e6,kFALSE); hT->Add(hG);
  TH2F *hE = Flux(2,8,1e6,kFALSE); hT->Add(hE);
  TH2F *hs[4] = {hC, hG, hE, hT};
  for (Int_t i = 0; i < 4; i++) {
    if (! hs[i]) continue;
    Int_t ix1 = hs[i]->GetXaxis()->FindBin(-200.);
    Int_t ix2 = hs[i]->GetXaxis()->FindBin( 200.);
    hs[i]->GetXaxis()->SetRange(ix1,ix2);
    Int_t jx1 = hs[i]->GetYaxis()->FindBin( 55);
    Int_t jx2 = hs[i]->GetYaxis()->FindBin(195);
    hs[i]->GetYaxis()->SetRange(jx1,jx2);
    csc->cd(3*i + 1)->SetLogz(); hs[i]->Draw("colz"); 
    csc->cd(3*i + 2); 
    TH1 *projY = hs[i]->ProjectionY(); projY->Fit(Gene); 
    csc->cd(3*i + 3); 
    TH1 *projX = hs[i]->ProjectionX(); projX->Fit("pol1"); 
  }
}
/*
 Centronic 31He3/304/25e counter 
===================================
flux = 10 (Hz/cm^2)
Tempereture (T) = 300 (K)
Thermal neutron kinetic energy (kT) = 38.7782 (meV)
Thermal neutron verlocity v0 = 9.08543e-06(C)
Thermal neutron cross section (n + He3 -> p + H3) = 5400 v0/v (barn)
 Centronic 31He3/304/25e counter 
Pressure = 4 (ATM)
Volume = 152.171 (cm^3)
Total neutron path length in counter = V  flux 1521.71 (cm * Hz)
 N = l * xsection0 = 8.21723e-18 (cm^3 * Hz)
A = 3  density = A/22.4(l) = 0.000133929 (g/cm^3)
No. of atoms in 1 cm^3 = Avogadro/22.4(l) = 1.44531e+20 (1/cm^3)
counts =118.765 per 1 n (Hz/cm^2)
rate @ 10 = 1187.65 (Hz)
(Double_t)1.18764743380125947e+03
==================================
RS_P4-1614-204 GE Power System reuter-Stokes (Instrumentation Division)
========================================================================

flux = 10 (Hz/cm^2)
Tempereture (T) = 300 (K)
Thermal neutron kinetic energy (kT) = 38.7782 (meV)
Thermal neutron verlocity v0 = 9.08543e-06(C)
Thermal neutron cross section (n + He3 -> p + H3) = 5400 v0/v (barn)
RS_P4-1614-204 GE Power System reuter-Stokes
Pressure = 3 (ATM)
Volume = 698.243 (cm^3)
Total neutron path length in counter = V  flux 6982.43 (cm * Hz)
 N = l * xsection0 = 3.77051e-17 (cm^3 * Hz)
A = 3  density = A/22.4(l) = 0.000133929 (g/cm^3)
No. of atoms in 1 cm^3 = Avogadro/22.4(l) = 1.08399e+20 (1/cm^3)
counts =408.718 per 1 n (Hz/cm^2)
rate @ 10 = 4087.18 (Hz)
================================================================================




  Zdc/Bbc ~ constant after 20130329; replace 20130301 =>20130329 
  THe32013->Draw("MTD:BBCEandW>>MdtBbcI(1000,0,7e6,1000,0,1.2e4)","date > 20130329&&date < 20130403","colz");
  THe32013->Draw("MTD:BBCEandW>>MdtBbcII(1000,0,7e6,1000,0,6e4)" ,"date > 20130403&&date < 20130417","colz");
  THe32013->Draw("MTD:BBCEandW>>MdtBbcIII(1000,0,7e6,1000,0,6e4)","date > 20130417&&date < 20130503","colz");
  THe32013->Draw("MTD:BBCEandW>>MdtBbcIV(1000,0,7e6,1000,0,6e4)" ,"date > 20130508&&date < 20130522","colz");
  THe32013->Draw("MTD:BBCEandW>>MdtBbcV(1000,0,7e6,1000,0,6e4)"  ,"date > 20130522","colz");

  THe32013->Draw("MTD:ZDCEandW>>MdtZdcI(1000,0,6e5,1000,0,1.2e4)","date > 20130329&&date < 20130403","colz");
  THe32013->Draw("MTD:ZDCEandW>>MdtZdcII(1000,0,6e5,1000,0,6e4)" ,"date > 20130403&&date < 20130417","colz");
  THe32013->Draw("MTD:ZDCEandW>>MdtZdcIII(1000,0,6e5,1000,0,6e4)","date > 20130417&&date < 20130503","colz");
  THe32013->Draw("MTD:ZDCEandW>>MdtZdcIV(1000,0,6e5,1000,0,6e4)" ,"date > 20130508&&date < 20130522","colz");

  THe32013->Draw("MTD/ZDCEandW:utime-788936400>>MZ(1000,5.73e8,5.81e8,100,0,0.2)","ZDCEandW>0","colz") some problems with ZDC for date < 20130309
  THe32013->Draw("BBCEandW/ZDCEandW:utime-788936400>>BZ(1000,5.73e8,5.81e8,100,0,100)","ZDCEandW>0","colz") 
  THe32013->Draw("ZDCEandW/BBCEandW:utime-788936400>>ZB(1000,5.73e8,5.81e8,100,0,0.2)","BBCEandW>0","colz")

    THe32013->Draw("MTD:BbcCoinRate(BBCEandW,BBCE,BBCW) >> MBbc","date > 20130329&&date < 20130403","colz");
    THe32013->Draw("MTD:ZdcCoinRate(ZDCEandW,ZDCE,ZDCW) >> MZdc","date > 20130329&&date < 20130403","colz");
 THe32013->Draw("ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)/ZDCEandW:ZDCEandW","ZDCEandW>0&&ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>0&&date > 20130329","");
 THe32013->Draw("BbcCoinRate(BBCEandW,BBCE,BBCW)/BBCEandW:BBCEandW","BBCEandW>0&&BbcCoinRate(BBCEandW,BBCE,BBCW)>0&&date > 20130329","");

THe32013->Draw("BBCEandW/BBCE:BBCEandW","BBCE>0&&date > 20130329","");
THe32013->Draw("ZDCEandW/ZDCE:ZDCEandW","ZDCE>0&&date > 20130329","");

 THe32013->Draw("ZDCEandW/ZDCE:utime-788936400","ZDCE>0","");

.L Flux.C+
gStyle->SetOptStat(0);
TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
if (! c1) c1 = new TCanvas();
c1->Reset().;
TH1 *frame = c1->DrawFrame(0,0,12,6e4);
frame->SetTitle("He3 counter rate versus estimated from ZDC event rate");
frame->SetXTitle("Event rate (MHz)");

TLegend *l = new TLegend(0.7,0.6,0.9,0.8);
THe32013->SetMarkerStyle(1);
THe32013->SetMarkerColor(1);
THe32013->Draw("MTD:1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>MZAll(1000,1e-5,12,1000,0,6e4)","ZDCEandW>0","same");
MZAll->SetLineColor(1); l->AddEntry(MZAll,"All"); l->Draw();
THe32013->SetMarkerColor(2);
THe32013->Draw("MTD:1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ1(1000,1e-5,12,1000,0,6e4)","ZDCEandW>0 && date > 20130329&&date <= 20130403","same");
MZ1->SetLineColor(2); l->AddEntry(MZ1,"South, 20130329 < date <= 20130403");
THe32013->SetMarkerColor(3);
THe32013->Draw("MTD:1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ2(1000,1e-5,12,1000,0,6e4)","ZDCEandW>0 && date > 20130404&&date < 20130417","same");
MZ2->SetLineColor(3); l->AddEntry(MZ2,"West, 20130404 < date < 20130417");
THe32013->SetMarkerColor(4);
THe32013->Draw("MTD:1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ3(1000,1e-5,12,1000,0,6e4)","ZDCEandW>0 && date > 20130417&&date < 20130503","same");
MZ3->SetLineColor(4); l->AddEntry(MZ3,"East, 20130417 < date < 20130503");
THe32013->SetMarkerColor(6);
THe32013->Draw("MTD:1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ4(1000,1e-5,12,1000,0,6e4)","ZDCEandW>0 && date > 20130508&&date < 20130522","same");
MZ4->SetLineColor(6); l->AddEntry(MZ4,"North, 20130508 < date < 20130522");
THe32013->SetMarkerColor(7);
THe32013->Draw("MTD:1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ5(1000,1e-5,12,1000,0,6e4)","ZDCEandW>0 && date > 20130522","same");
MZ5->SetLineColor(7); l->AddEntry(MZ5,"Bottom. 20130522 < date < now");

.L Flux.C+
gStyle->SetOptStat(0);
TCanvas *c1 = (TCanvas *) gROOT->FindObject("c1");
if (! c1) c1 = new TCanvas();
c1->Reset().;
TH1 *frame = c1->DrawFrame(0,0,12,6e4);
frame->SetTitle("Thermal Neutron flux (Hz/cm^{2}) versus estimated from ZDC event rate");
frame->SetXTitle("Event rate (MHz)");

TLegend *l = new TLegend(0.7,0.6,0.9,0.8);
THe32013->SetMarkerStyle(1);
THe32013->SetMarkerColor(1);
THe32013->Draw("1e-2*MTDcor(MTD):1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>MZAll(1000,1e-5,12,1000,0,1e3)","ZDCEandW>0","same");
MZAll->SetLineColor(1); l->AddEntry(MZAll,"All"); l->Draw();
THe32013->SetMarkerColor(2);
THe32013->Draw("1e-2*MTDcor(MTD):1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ1(1000,1e-5,12,1000,0,1e3)","ZDCEandW>0 && date > 20130329&&date <= 20130403","same");
MZ1->SetLineColor(2); l->AddEntry(MZ1,"South, 20130329 < date <= 20130403");
THe32013->SetMarkerColor(3);
THe32013->Draw("1e-2*MTDcor(MTD):1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ2(1000,1e-5,12,1000,0,1e3)","ZDCEandW>0 && date > 20130404&&date < 20130417","same");
MZ2->SetLineColor(3); l->AddEntry(MZ2,"West, 20130404 < date < 20130417");
THe32013->SetMarkerColor(4);
THe32013->Draw("1e-2*MTDcor(MTD):1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ3(1000,1e-5,12,1000,0,1e3)","ZDCEandW>0 && date > 20130417&&date < 20130503","same");
MZ3->SetLineColor(4); l->AddEntry(MZ3,"East, 20130417 < date < 20130503");
THe32013->SetMarkerColor(6);
THe32013->Draw("1e-2*MTDcor(MTD):1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ4(1000,1e-5,12,1000,0,1e3)","ZDCEandW>0 && date > 20130508&&date < 20130522","same");
MZ4->SetLineColor(6); l->AddEntry(MZ4,"North, 20130508 < date < 20130522");
THe32013->SetMarkerColor(7);
THe32013->Draw("1e-2*MTDcor(MTD):1e-6*ZdcCoinRate(ZDCEandW,ZDCE,ZDCW)>>  MZ5(1000,1e-5,12,1000,0,1e3)","ZDCEandW>0 && date > 20130522","same");
MZ5->SetLineColor(7); l->AddEntry(MZ5,"Bottom. 20130522 < date < now");

MZ1->ProfileX()->Draw()
pol1->FixParameter(0,0)
MZ1_pfx->Fit("pol1","er","",2,8)
   2  p1           1.23109e+03   1.17802e-01   1.17802e-01  -7.74471e-05
MZ2->ProfileX()->Draw()
pol1->FixParameter(0,0)
MZ2_pfx->Fit("pol1","er","",2,4)
   2  p1           8.50935e+03   1.24992e+00   1.24992e+00   5.89824e-06
MZ3->ProfileX()->Draw()
pol1->FixParameter(0,0)
MZ3_pfx->Fit("pol1","er","",2,8)
   2  p1           1.06998e+04   1.00390e+00   1.00390e+00   2.46104e-06
MZ4->ProfileX()->Draw()
pol1->FixParameter(0,0)
MZ4_pfx->Fit("pol1","er","",2,8)
   2  p1           2.41052e+03   2.66883e-01   2.66883e-01  -3.44164e-04
MZ5->ProfileX()->Draw()
pol1->FixParameter(0,0)
MZ5_pfx->Fit("pol1","er","",2,8)
   2  p1           6.88668e+02   5.46599e-02   5.46599e-02  -1.66374e-03

time
-----
TFile *_file0 = TFile::Open("14082Out.root")
TFile *_file1 = TFile::Open("14098Out.root")
TFile *_file2 = TFile::Open("14113089.root")
.L Flux.C+
gStyle->SetOptStat(0);
_file0->cd();
FitP->SetMarkerColor(1);
FitP->Draw("MTD/ZDCEandW:Dtime(utime)>>R1(400,0,1200)","","prof")
  R1->SetMaximum(0.4);
_file1->cd();
FitP->SetMarkerColor(2);
FitP->Draw("MTD/ZDCEandW:Dtime(utime)>>R2(400,0,1200)","","profsame")

_file2->cd();
FitP->SetMarkerColor(3);
FitP->Draw("MTD/ZDCEandW:Dtime(utime)>>R3(400,0,1200)","","profsame")



THe32013->Draw("MTD:utime-788936400>>T(440460,573181200,581990400,100,0,6e4)","date > 20130301&& MTD>0","")
c1->Clear()
T->SetStats(0)
T->GetXaxis()->SetTimeDisplay(1)
T->SetTitle("^{3}He conter rate versus time")
T->Draw()
TimePeriods()
================================================================================
06/20/15 
  2015
--------
Jamie, 06/19/15

Ok, so this is tagged on "physics on", fully singles corrected ZDC coincidence rate
pp:
http://www.star.bnl.gov/protected/common/common2015/trigger2015/sampleEfficiencypp200GeV/integrated_delivered_byfill_time.png
using cross section 0.264e9 ;(pb) #ZDC from van der Meer scan => integrated luminosity = 180 pb^-1

pAu:
http://www.star.bnl.gov/protected/common/common2015/trigger2015/sampleEfficiencypAu200GeV/integrated_delivered_byfill_time.png
using cross section 35e6 nb   => integrated luminosity = 620 nb^-1
 sigma_inel = 
pAl:
http://www.star.bnl.gov/protected/common/common2015/trigger2015/sampleEfficiencypAl200GeV/integrated_delivered_byfill_time.png
using cross section 11.4e6 nb; => integrated luminosity = 1.95 pb^-1
-----------
pp 200: Zdc cross section : 0.264e9 pb = 0.264e6 nb = 0.264e3 ub = 0.264 mb => Jamie plot Integrated luminosity = 180 pb^-1;
STAR production trigger only = 6.41 Bevents, All = 6.54 Bevents;
Jamie  total no. of event = 180 pb^-1 *  0.264e6 = 47.52 B
http://www.rhichome.bnl.gov/RHIC/Runs/
Run-15 CY2014/15, FY2015 (22.4 cryo-weeks planned)	polarized p + p	100.2	10.9 weeks 382 pb-1
Total no. of events = 50 mb * 382 pb-1 = 19.1e12
________________________________________________________________________________
Jamie, 06/24/15

BBC cross section is something like 50x the trigger version of the ZDC coincidence,
which has cross section 11.4/1.68/0.96 = 7 mb, i.e.
http://www.star.bnl.gov/protected/common/common2015/trigger2015/plots_pAl200gev/plots.html#BBCMB
i.e. 0.35 b

Cross-check:
For the pAu, BBC cross section is something like 80x the trigger version of the ZDC coincidence,
which has cross section 35/1.68 = 21 mb, i.e. 1.6 b 

Checking scaling, should go roughly like A^(2/3), so 
1.6 b/(197^{2/3})* (27^{2/3}) = 0.43 b, bit high.
Alternatively, as an estimate 40 mb * 27^{2./3.} = 0.36 b, pretty close.

The 1.6b for p+Au might be an overestimate due to backgrounds, because the BBC/ZDC shows a lot of structure.
I think Glauber gives something closer to 1.5 b.

BBC should be sensitive to most of the inelastic cross section.  So something like 0.35 b should be close
for the inelastic cross section for p+Al.


*/

