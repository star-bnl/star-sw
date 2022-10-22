#include "TROOT.h"
#include "Riostream.h"
#include "TLegend.h"
#include "TString.h"
#include "TF1.h"
#include "TMath.h"
#include "TFile.h"
#include "TH1.h"
#include "TCanvas.h"
#include "Names.h"
//________________________________________________________________________________
Double_t tmax(Double_t *x, Double_t *p) {
  static Double_t Tcut = 1e-4; // 100 keV maximum cluster size (~80 keV)
  static const Double_t m_e = .51099907e-3;
  Int_t h = (Int_t) p[0];
  Double_t M = TpcRSPart[h].mass;
  Int_t pdg  = TpcRSPart[h].pdg; 
  Int_t charge = TpcRSPart[h].charge; 
  Double_t bgL10 = x[0];
  Double_t bg = TMath::Power(10.,bgL10);
  Double_t bg2 = bg*bg;
  Double_t gamma = TMath::Sqrt(bg2 + 1);
  Double_t Tmax; 
  if (TMath::Abs(pdg) == 11) {
    if (charge > 0) Tmax =     m_e*(gamma - 1);
    else            Tmax = 0.5*m_e*(gamma - 1);
  } else {
    Double_t r = m_e/M;
    Tmax = 2*m_e*bg2/(1 + 2*gamma*r + r*r); 
  }
  return 1e6*TMath::Min(Tcut, Tmax);
}
//________________________________________________________________________________
void Tmax() {
  TLegend *l = new TLegend(0.15,0.6,0.4,0.9);
  TString same;
  //  for (Int_t h = 0; h < NTpcRSParts; h++) {
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  TH1F *frame = c1->DrawFrame(-2.,0.,1.,110.);
  frame->SetXTitle("log_{10}(#beta #gamma)");
  frame->SetYTitle("t_{max} (keV)");
  frame->Draw();
  for (Int_t h = 0; h < 9; h++) {
    TString name(TpcRSPart[h].name);
    name.ReplaceAll("+","P");
    name.ReplaceAll("-","N");
    cout << "create " << TpcRSPart[h].name << endl;
    TF1 *f = new TF1(name,tmax,-2.,1.,1);
    f->SetParName(0,"TpcRSIndex");
    f->SetParameter(0,h );
    f->SetLineColor(h+1);
    f->Draw("same"); // same = "same";
    l->AddEntry(f,TpcRSPart[h].pdgname);
  }
  l->Draw();
}
//________________________________________________________________________________
Double_t Ec(Double_t *x, Double_t *p) {  // StTpcRSMaker::
  if (x[0] < p[0]/2 || x[0] > 3.064*p[0]) return 0;
  if (x[0] < p[0]) return 1;
  return TMath::Power(p[0]/x[0],4);
}
//________________________________________________________________________________
TF1 *fEc(Double_t w) { // StTpcRSMaker::StTpcRSMaker::
  TF1 *f = new TF1("Ec",Ec,0,3.064*w,1);
  f->SetParameter(0,w);
  return f;
}
//--------------------------------------------------------------------------------
void CheckNdE() {
  TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
  //  TH1D *dNdE = (TH1D* ) f->Get("dNdE");
  TH1D *dNdEL10 = (TH1D*) f->Get("dNdEL10");
  //  TH1D *dNdE1 = new TH1D("dNdE1","dNdE from dNdE",100,0.0, 1e3);
  TH1D *dNdE2 = new TH1D("dNdE2","dNdE from dNdEL10",100,0.0, 1e3);
  static Double_t cLog10 = TMath::Log(10.);
  for (Int_t i = 0; i < 10000; i++) {
    //    Double_t e1 = dNdE->GetRandom();
    //    dNdE1->Fill(e1);
    Double_t e2 = TMath::Exp(cLog10*dNdEL10->GetRandom());
    dNdE2->Fill(e2);
  }
}
//________________________________________________________________________________
Double_t dNdEFunc(Double_t *x, Double_t *p) {
  static TH1D *dNdE = 0;
  static Double_t dEmaxL10 = 5.40502303800167372e+00;
  static Double_t dNmax    = -1;
  static Double_t slope    = -2.40581e+00;
  static Double_t xmin     =  0;
  static Double_t Norm     =  2.95275302029816906e+01; // F->Integral(0,1e9) 
  if (! dNdE) {
    TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    dNdE = (TH1D* ) f->Get("dNdE");
    Int_t nx = dNdE->GetXaxis()->GetNbins();
    dEmaxL10 = TMath::Log10(dNdE->GetBinCenter(nx));
    dNmax    = dNdE->GetBinContent(nx);
    xmin     = dNdE->GetBinLowEdge(1);
  }
  if (x[0] < xmin) return 0;
  Double_t xL10 = TMath::Log10(x[0]);
  if (xL10 < dEmaxL10) return dNdE->Interpolate(x[0])/Norm;
  else                 return dNmax*TMath::Power(10., slope*(xL10 - dEmaxL10))/Norm; 
}
//--------------------------------------------------------------------------------
TF1 *dNdxF() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("dNdxF",dNdEFunc,8.69499969482421875e+00, 2.54110750000000000e+05, 0);
    f->SetNpx(951);
  }
  return f;
}
//________________________________________________________________________________
Double_t NpEffFunc(Double_t *x, Double_t *p) {
  static TF1 *F = dNdxF();
  static Double_t W = 26.2; // eV
  Double_t tMax = 1e3*tmax(x, p);
  return F->Integral(W/2, tMax);
}
//________________________________________________________________________________
TF1 *dNdxEff(Int_t h = 0) {
  TString name(TpcRSPart[h].name);
  name.ReplaceAll("+","P");
  name.ReplaceAll("-","N");
  TF1 *f = new TF1(Form("Eff%s",name.Data()),NpEffFunc,-2.,0.,1);
  f->SetParName(0,"TpcRSIndex");
  f->SetParameter(0,h );
  f->SetLineColor(h+1);
  return f;  
}
//________________________________________________________________________________
TH1D *protonEff() {
//========= Macro generated from object: Func/
//========= by ROOT version5.34/39
   
   TH1D *Func__1 = new TH1D("protonEff","",100,-2,-1.73472e-17);
   Func__1->SetBinContent(1,0.829974);
   Func__1->SetBinContent(2,0.833488);
   Func__1->SetBinContent(3,0.836679);
   Func__1->SetBinContent(4,0.839568);
   Func__1->SetBinContent(5,0.842227);
   Func__1->SetBinContent(6,0.844577);
   Func__1->SetBinContent(7,0.846659);
   Func__1->SetBinContent(8,0.848506);
   Func__1->SetBinContent(9,0.850145);
   Func__1->SetBinContent(10,0.851604);
   Func__1->SetBinContent(11,0.857514);
   Func__1->SetBinContent(12,0.863548);
   Func__1->SetBinContent(13,0.869116);
   Func__1->SetBinContent(14,0.874097);
   Func__1->SetBinContent(15,0.878448);
   Func__1->SetBinContent(16,0.88217);
   Func__1->SetBinContent(17,0.885305);
   Func__1->SetBinContent(18,0.887923);
   Func__1->SetBinContent(19,0.890128);
   Func__1->SetBinContent(20,0.891974);
   Func__1->SetBinContent(21,0.893513);
   Func__1->SetBinContent(22,0.894796);
   Func__1->SetBinContent(23,0.895866);
   Func__1->SetBinContent(24,0.896761);
   Func__1->SetBinContent(25,0.897513);
   Func__1->SetBinContent(26,0.898147);
   Func__1->SetBinContent(27,0.898683);
   Func__1->SetBinContent(28,0.899139);
   Func__1->SetBinContent(29,0.899531);
   Func__1->SetBinContent(30,0.899868);
   Func__1->SetBinContent(31,0.90016);
   Func__1->SetBinContent(32,0.900415);
   Func__1->SetBinContent(33,0.900639);
   Func__1->SetBinContent(34,0.900836);
   Func__1->SetBinContent(35,0.90101);
   Func__1->SetBinContent(36,0.901165);
   Func__1->SetBinContent(37,0.901304);
   Func__1->SetBinContent(38,0.901431);
   Func__1->SetBinContent(39,0.90164);
   Func__1->SetBinContent(40,0.901817);
   Func__1->SetBinContent(41,0.901969);
   Func__1->SetBinContent(42,0.902099);
   Func__1->SetBinContent(43,0.902212);
   Func__1->SetBinContent(44,0.902309);
   Func__1->SetBinContent(45,0.902392);
   Func__1->SetBinContent(46,0.902469);
   Func__1->SetBinContent(47,0.902536);
   Func__1->SetBinContent(48,0.902595);
   Func__1->SetBinContent(49,0.902646);
   Func__1->SetBinContent(50,0.902692);
   Func__1->SetBinContent(51,0.902733);
   Func__1->SetBinContent(52,0.902769);
   Func__1->SetBinContent(53,0.902801);
   Func__1->SetBinContent(54,0.90283);
   Func__1->SetBinContent(55,0.902857);
   Func__1->SetBinContent(56,0.902881);
   Func__1->SetBinContent(57,0.902902);
   Func__1->SetBinContent(58,0.902922);
   Func__1->SetBinContent(59,0.90294);
   Func__1->SetBinContent(60,0.902956);
   Func__1->SetBinContent(61,0.90297);
   Func__1->SetBinContent(62,0.902984);
   Func__1->SetBinContent(63,0.902996);
   Func__1->SetBinContent(64,0.903007);
   Func__1->SetBinContent(65,0.903017);
   Func__1->SetBinContent(66,0.903026);
   Func__1->SetBinContent(67,0.903034);
   Func__1->SetBinContent(68,0.903042);
   Func__1->SetBinContent(69,0.903049);
   Func__1->SetBinContent(70,0.903055);
   Func__1->SetBinContent(71,0.903061);
   Func__1->SetBinContent(72,0.903066);
   Func__1->SetBinContent(73,0.903071);
   Func__1->SetBinContent(74,0.903075);
   Func__1->SetBinContent(75,0.903079);
   Func__1->SetBinContent(76,0.90308);
   Func__1->SetBinContent(77,0.90308);
   Func__1->SetBinContent(78,0.90308);
   Func__1->SetBinContent(79,0.90308);
   Func__1->SetBinContent(80,0.90308);
   Func__1->SetBinContent(81,0.90308);
   Func__1->SetBinContent(82,0.90308);
   Func__1->SetBinContent(83,0.90308);
   Func__1->SetBinContent(84,0.90308);
   Func__1->SetBinContent(85,0.90308);
   Func__1->SetBinContent(86,0.90308);
   Func__1->SetBinContent(87,0.90308);
   Func__1->SetBinContent(88,0.90308);
   Func__1->SetBinContent(89,0.90308);
   Func__1->SetBinContent(90,0.90308);
   Func__1->SetBinContent(91,0.90308);
   Func__1->SetBinContent(92,0.90308);
   Func__1->SetBinContent(93,0.90308);
   Func__1->SetBinContent(94,0.90308);
   Func__1->SetBinContent(95,0.90308);
   Func__1->SetBinContent(96,0.90308);
   Func__1->SetBinContent(97,0.90308);
   Func__1->SetBinContent(98,0.90308);
   Func__1->SetBinContent(99,0.90308);
   Func__1->SetBinContent(100,0.90308);
   Func__1->SetEntries(700);
   Func__1->SetDirectory(0);
   Func__1->SetStats(0);
   Func__1->SetFillColor(19);
   Func__1->SetFillStyle(0);
   Func__1->SetLineColor(9);
   Func__1->SetLineWidth(3);
   Func__1->SetMarkerStyle(20);
   Func__1->GetXaxis()->SetTitleOffset(1.2);
   //   Func__1->Draw("");
   return Func__1;
}
//________________________________________________________________________________
TH1D *electronEff() {
//========= Macro generated from object: Func/
//========= by ROOT version5.34/39
   
   TH1D *Func__2 = new TH1D("electronEff","",100,-2,0);
   Func__2->SetBinContent(1,0.00439274);
   Func__2->SetBinContent(2,0.0611353);
   Func__2->SetBinContent(3,0.133572);
   Func__2->SetBinContent(4,0.218659);
   Func__2->SetBinContent(5,0.307193);
   Func__2->SetBinContent(6,0.396369);
   Func__2->SetBinContent(7,0.48356);
   Func__2->SetBinContent(8,0.566106);
   Func__2->SetBinContent(9,0.63976);
   Func__2->SetBinContent(10,0.697719);
   Func__2->SetBinContent(11,0.738488);
   Func__2->SetBinContent(12,0.763405);
   Func__2->SetBinContent(13,0.777091);
   Func__2->SetBinContent(14,0.784883);
   Func__2->SetBinContent(15,0.79048);
   Func__2->SetBinContent(16,0.795443);
   Func__2->SetBinContent(17,0.800338);
   Func__2->SetBinContent(18,0.805233);
   Func__2->SetBinContent(19,0.810123);
   Func__2->SetBinContent(20,0.814921);
   Func__2->SetBinContent(21,0.819516);
   Func__2->SetBinContent(22,0.823838);
   Func__2->SetBinContent(23,0.827843);
   Func__2->SetBinContent(24,0.831534);
   Func__2->SetBinContent(25,0.834908);
   Func__2->SetBinContent(26,0.837952);
   Func__2->SetBinContent(27,0.840756);
   Func__2->SetBinContent(28,0.843276);
   Func__2->SetBinContent(29,0.845505);
   Func__2->SetBinContent(30,0.847481);
   Func__2->SetBinContent(31,0.849235);
   Func__2->SetBinContent(32,0.850793);
   Func__2->SetBinContent(33,0.853524);
   Func__2->SetBinContent(34,0.860158);
   Func__2->SetBinContent(35,0.866006);
   Func__2->SetBinContent(36,0.871319);
   Func__2->SetBinContent(37,0.876032);
   Func__2->SetBinContent(38,0.880109);
   Func__2->SetBinContent(39,0.883571);
   Func__2->SetBinContent(40,0.886473);
   Func__2->SetBinContent(41,0.888903);
   Func__2->SetBinContent(42,0.890948);
   Func__2->SetBinContent(43,0.892656);
   Func__2->SetBinContent(44,0.89408);
   Func__2->SetBinContent(45,0.895267);
   Func__2->SetBinContent(46,0.896258);
   Func__2->SetBinContent(47,0.897089);
   Func__2->SetBinContent(48,0.897788);
   Func__2->SetBinContent(49,0.898378);
   Func__2->SetBinContent(50,0.898878);
   Func__2->SetBinContent(51,0.899306);
   Func__2->SetBinContent(52,0.899673);
   Func__2->SetBinContent(53,0.89999);
   Func__2->SetBinContent(54,0.900266);
   Func__2->SetBinContent(55,0.900507);
   Func__2->SetBinContent(56,0.900719);
   Func__2->SetBinContent(57,0.900906);
   Func__2->SetBinContent(58,0.901072);
   Func__2->SetBinContent(59,0.901219);
   Func__2->SetBinContent(60,0.901351);
   Func__2->SetBinContent(61,0.901511);
   Func__2->SetBinContent(62,0.901706);
   Func__2->SetBinContent(63,0.901873);
   Func__2->SetBinContent(64,0.902016);
   Func__2->SetBinContent(65,0.902139);
   Func__2->SetBinContent(66,0.902245);
   Func__2->SetBinContent(67,0.902336);
   Func__2->SetBinContent(68,0.902416);
   Func__2->SetBinContent(69,0.902489);
   Func__2->SetBinContent(70,0.902553);
   Func__2->SetBinContent(71,0.902609);
   Func__2->SetBinContent(72,0.902658);
   Func__2->SetBinContent(73,0.902702);
   Func__2->SetBinContent(74,0.90274);
   Func__2->SetBinContent(75,0.902775);
   Func__2->SetBinContent(76,0.902806);
   Func__2->SetBinContent(77,0.902834);
   Func__2->SetBinContent(78,0.90286);
   Func__2->SetBinContent(79,0.902882);
   Func__2->SetBinContent(80,0.902903);
   Func__2->SetBinContent(81,0.902922);
   Func__2->SetBinContent(82,0.902939);
   Func__2->SetBinContent(83,0.902955);
   Func__2->SetBinContent(84,0.902969);
   Func__2->SetBinContent(85,0.902982);
   Func__2->SetBinContent(86,0.902993);
   Func__2->SetBinContent(87,0.903004);
   Func__2->SetBinContent(88,0.903014);
   Func__2->SetBinContent(89,0.903022);
   Func__2->SetBinContent(90,0.90303);
   Func__2->SetBinContent(91,0.903038);
   Func__2->SetBinContent(92,0.903044);
   Func__2->SetBinContent(93,0.903051);
   Func__2->SetBinContent(94,0.903056);
   Func__2->SetBinContent(95,0.903061);
   Func__2->SetBinContent(96,0.903066);
   Func__2->SetBinContent(97,0.90307);
   Func__2->SetBinContent(98,0.903074);
   Func__2->SetBinContent(99,0.903077);
   Func__2->SetBinContent(100,0.90308);
   Func__2->SetMinimum(0);
   Func__2->SetEntries(1400);
   Func__2->SetDirectory(0);
   Func__2->SetStats(0);
   Func__2->SetFillColor(19);
   Func__2->SetFillStyle(0);
   Func__2->SetLineColor(5);
   Func__2->SetLineWidth(3);
   Func__2->SetMarkerStyle(20);
   Func__2->GetXaxis()->SetTitleOffset(1.2);
   //   Func__2->Draw("");
   return Func__2;
}
//________________________________________________________________________________
TH1D *positronEff() {
//========= Macro generated from object: Func/
//========= by ROOT version5.34/39
   
   TH1D *Func__4 = new TH1D("positronEff","",100,-2,-1.73472e-17);
   Func__4->SetBinContent(1,0.607048);
   Func__4->SetBinContent(2,0.672647);
   Func__4->SetBinContent(3,0.721308);
   Func__4->SetBinContent(4,0.753371);
   Func__4->SetBinContent(5,0.771645);
   Func__4->SetBinContent(6,0.781624);
   Func__4->SetBinContent(7,0.787978);
   Func__4->SetBinContent(8,0.793113);
   Func__4->SetBinContent(9,0.79802);
   Func__4->SetBinContent(10,0.802916);
   Func__4->SetBinContent(11,0.80781);
   Func__4->SetBinContent(12,0.81267);
   Func__4->SetBinContent(13,0.817372);
   Func__4->SetBinContent(14,0.821831);
   Func__4->SetBinContent(15,0.82599);
   Func__4->SetBinContent(16,0.829824);
   Func__4->SetBinContent(17,0.833351);
   Func__4->SetBinContent(18,0.836554);
   Func__4->SetBinContent(19,0.839451);
   Func__4->SetBinContent(20,0.842124);
   Func__4->SetBinContent(21,0.844485);
   Func__4->SetBinContent(22,0.846577);
   Func__4->SetBinContent(23,0.848433);
   Func__4->SetBinContent(24,0.85008);
   Func__4->SetBinContent(25,0.851544);
   Func__4->SetBinContent(26,0.857228);
   Func__4->SetBinContent(27,0.863295);
   Func__4->SetBinContent(28,0.868888);
   Func__4->SetBinContent(29,0.873893);
   Func__4->SetBinContent(30,0.878271);
   Func__4->SetBinContent(31,0.882018);
   Func__4->SetBinContent(32,0.885176);
   Func__4->SetBinContent(33,0.887815);
   Func__4->SetBinContent(34,0.890035);
   Func__4->SetBinContent(35,0.891896);
   Func__4->SetBinContent(36,0.893447);
   Func__4->SetBinContent(37,0.89474);
   Func__4->SetBinContent(38,0.895818);
   Func__4->SetBinContent(39,0.896721);
   Func__4->SetBinContent(40,0.897479);
   Func__4->SetBinContent(41,0.898117);
   Func__4->SetBinContent(42,0.898657);
   Func__4->SetBinContent(43,0.899117);
   Func__4->SetBinContent(44,0.899511);
   Func__4->SetBinContent(45,0.89985);
   Func__4->SetBinContent(46,0.900145);
   Func__4->SetBinContent(47,0.900401);
   Func__4->SetBinContent(48,0.900626);
   Func__4->SetBinContent(49,0.900824);
   Func__4->SetBinContent(50,0.901);
   Func__4->SetBinContent(51,0.901155);
   Func__4->SetBinContent(52,0.901294);
   Func__4->SetBinContent(53,0.901419);
   Func__4->SetBinContent(54,0.901625);
   Func__4->SetBinContent(55,0.901804);
   Func__4->SetBinContent(56,0.901957);
   Func__4->SetBinContent(57,0.902089);
   Func__4->SetBinContent(58,0.902202);
   Func__4->SetBinContent(59,0.9023);
   Func__4->SetBinContent(60,0.902384);
   Func__4->SetBinContent(61,0.902461);
   Func__4->SetBinContent(62,0.902529);
   Func__4->SetBinContent(63,0.902588);
   Func__4->SetBinContent(64,0.90264);
   Func__4->SetBinContent(65,0.902686);
   Func__4->SetBinContent(66,0.902727);
   Func__4->SetBinContent(67,0.902763);
   Func__4->SetBinContent(68,0.902796);
   Func__4->SetBinContent(69,0.902825);
   Func__4->SetBinContent(70,0.902852);
   Func__4->SetBinContent(71,0.902876);
   Func__4->SetBinContent(72,0.902897);
   Func__4->SetBinContent(73,0.902917);
   Func__4->SetBinContent(74,0.902935);
   Func__4->SetBinContent(75,0.902951);
   Func__4->SetBinContent(76,0.902966);
   Func__4->SetBinContent(77,0.902979);
   Func__4->SetBinContent(78,0.902992);
   Func__4->SetBinContent(79,0.903003);
   Func__4->SetBinContent(80,0.903013);
   Func__4->SetBinContent(81,0.903022);
   Func__4->SetBinContent(82,0.90303);
   Func__4->SetBinContent(83,0.903038);
   Func__4->SetBinContent(84,0.903045);
   Func__4->SetBinContent(85,0.903051);
   Func__4->SetBinContent(86,0.903057);
   Func__4->SetBinContent(87,0.903062);
   Func__4->SetBinContent(88,0.903067);
   Func__4->SetBinContent(89,0.903071);
   Func__4->SetBinContent(90,0.903075);
   Func__4->SetBinContent(91,0.903079);
   Func__4->SetBinContent(92,0.90308);
   Func__4->SetBinContent(93,0.90308);
   Func__4->SetBinContent(94,0.90308);
   Func__4->SetBinContent(95,0.90308);
   Func__4->SetBinContent(96,0.90308);
   Func__4->SetBinContent(97,0.90308);
   Func__4->SetBinContent(98,0.90308);
   Func__4->SetBinContent(99,0.90308);
   Func__4->SetBinContent(100,0.90308);
   Func__4->SetEntries(1200);
   Func__4->SetDirectory(0);
   Func__4->SetStats(0);
   Func__4->SetFillColor(19);
   Func__4->SetFillStyle(0);
   Func__4->SetLineColor(6);
   Func__4->SetLineWidth(3);
   Func__4->SetMarkerStyle(20);
   Func__4->GetXaxis()->SetTitleOffset(1.2);
   //   Func__4->Draw("");
   return Func__4;
}
/*
  
  gStyle->SetOptDate(0);
  TH1F *frame = c2->DrawFrame(-2,0,0,1)
  frame->SetXTitle("log_{10}(#beta #gamma)")
  frame->SetYTitle("Efficiency")
  frame->Draw()
  TH1D *proton = protonEff()
  proton->Draw("samel");
  TH1D *electron = electronEff()
  electron->Draw("samel");
  TH1D *positron = positronEff()
  positron->Draw("samel");
  TLegend *l = new TLegend(0.4,0.5,0.8,0.8);
  l->AddEntry(electron,"e^{-}");
  l->AddEntry(positron,"e^{+}");
  l->AddEntry(proton,"P");
  l->Draw();
 */
