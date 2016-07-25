/*
  root.exe MuAuAu200A710.root lDb.C 'MakeSuperSectorPositionB.C(20140101,731)'
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TString.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "StChain.h"
#include "tables/St_Survey_Table.h"
#include "TGeoMatrix.h"
#include "StTpcDb/StTpcDb.h"
#include "TStyle.h"
#endif
struct data_t {
  Double_t dx, ddx, dy, ddy, dz, ddz, alpha, dalpha, beta, dbeta, gamma, dgamma;
  Char_t *comment;
  void Print() {
    cout << Form("dx = %8.1f+/-%5.1f ", dx, ddx) 
	 << Form("dy = %8.1f+/-%5.1f ", dy, ddy) 
	 << Form("dz = %8.1f+/-%5.1f ", dz, ddy) 
	 << Form("alpha = %7.2f+/-%5.2f ", alpha, dalpha) 
	 << Form("beta = %7.2f+/-%5.2f ", beta, dbeta) 
	 << Form("gamma = %7.2f+/-%5.2f ", gamma, dgamma) 
      //	 << comment 
	 << endl;
  }
};
//________________________________________________________________________________
void MakeSuperSectorPositionB(Int_t date = 20140101, Int_t time = 720){
  const Char_t *plots[3] = {"dXS","dYS","dZS"};
  data_t Data[24]; memset(Data, 0, sizeof(Data));
  gStyle->SetOptStat(0);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1",Form("c%i",time));
  c1->Clear();
  c1->Divide(1,3);
  for (Int_t i = 0; i < 3; i++) {
    c1->cd(i+1)->SetLogz(1);
    TH3F *h3 = (TH3F *) gDirectory->Get(plots[i]);
    if (! h3) {cout << "Can't find " << plots[i] << endl; return;} 
    TH2D *h2 = (TH2D *) h3->Project3D("zx");
    h2->Draw("colz");
    h2->FitSlicesY();
    TH1D *h1 = (TH1D *) gDirectory->Get(Form("%s_1",h2->GetName()));
    if (! h1) {cout << "Can't find " << plots[i] << "_1" <<  endl; return;} 
    h1->Draw("same");
    for (Int_t j = 1; j <= 24; j++) {
      Double_t s = 1;
#if 0
      if (j <= 12) s = -1;
#endif
      if      (i == 0) {Data[j-1].dx    =   s*1e4*h1->GetBinContent(j); Data[j-1].ddx    = 1e4*h1->GetBinError(j);}
      else if (i == 1) {Data[j-1].dy    =   s*1e4*h1->GetBinContent(j); Data[j-1].ddy    = 1e4*h1->GetBinError(j);}
      else if (i == 2) {Data[j-1].dz    =   s*1e4*h1->GetBinContent(j); Data[j-1].ddz    = 1e4*h1->GetBinError(j);}
    }
  }
  for (Int_t j = 1; j <= 24; j++) {
    cout << Form("%2i ",j); Data[j-1].Print();
  }
#if 0
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,tpcDb,MagF,nodefault");
#endif
  StChain *chain = (StChain *) StChain::GetChain();
  StMaker *db = chain->Maker("db");
  if (! db) return;
  db->SetDebug(1);
  db->SetDateTime(date,time);
  chain->Init();
  chain->MakeEvent();
  St_Survey *TpcSuperSectorPositionB = (St_Survey *) chain->GetDataBase("Geometry/tpc/TpcSuperSectorPositionB");
  if (! TpcSuperSectorPositionB)  {cout << "TpcSuperSectorPositionB has not been found"  << endl; return;}
  TpcSuperSectorPositionB->Print(0,24);
  TGeoHMatrix GL;
  Survey_st *TpcSuperSectorOld         = TpcSuperSectorPositionB->GetTable();        
  Survey_st row[24];
  for (Int_t i = 0; i < 24; i++) {
    row[i] = TpcSuperSectorOld[i];
    GL.SetRotation(&TpcSuperSectorOld[i].r00);
    GL.SetTranslation(&TpcSuperSectorOld[i].t0); cout << "s: " << i+1 << " GL\t"; GL.Print();
    TGeoHMatrix dR, dT;
    dR.RotateX(180./TMath::Pi()*Data[i].alpha*1e-3);
    dR.RotateY(180./TMath::Pi()*Data[i].beta*1e-3); 
    dR.RotateZ(180./TMath::Pi()*Data[i].gamma*1e-3);
    Double_t xyz[3];
    Double_t dxyz[3], drot[3];
    xyz[0] = 1e-4*Data[i].dx;
    xyz[1] = 1e-4*Data[i].dy;
    xyz[2] = 1e-4*Data[i].dz;
    dxyz[0] = 1e-4*Data[i].ddx;
    dxyz[1] = 1e-4*Data[i].ddy;
    dxyz[2] = 1e-4*Data[i].ddz;
    drot[0] = Data[i].dalpha*1e-3;
    drot[1] = Data[i].dbeta*1e-3; 
    drot[2] = Data[i].dgamma*1e-3;
    dR.SetTranslation(xyz);
    memcpy(&row[i].sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row[i].sigmaTrX, dxyz, 3*sizeof(Double_t));
    cout << "Additional rotation for Inner Sector\t"; dR.Print();
    //    TGeoHMatrix GLnew = GL * dR;  cout << "GLnew\t"; GLnew.Print();
    //    TGeoHMatrix GLnew = StTpcDb::instance()->Flip().Inverse()*dR*StTpcDb::instance()->Flip()*GL; GLnew.Print();
#if 1
    TGeoHMatrix dRI = dR.Inverse();
    TGeoHMatrix GLnew = GL*dRI; GLnew.Print();
#else
    TGeoHMatrix GLnew = GL*dR; GLnew.Print();
#endif
    Double_t *R = GLnew.GetRotationMatrix();
    memcpy(&row[i].r00, R, 9*sizeof(Double_t));
    Double_t *tr = GLnew.GetTranslation();
    memcpy(&row[i].t0, tr, 3*sizeof(Double_t));
  }
  TString fOut =  Form("TpcSuperSectorPositionB.%8i.%06i.C", date, time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[24] = {" << endl; 
  out << "    //             -gamma     beta    gamma            -alpha    -beta    alpha                x0       y0       z0" << endl;
  for (Int_t i = 0; i < 24; i++) {
    out << "    {" << Form("%2i",i+1) << ","; 
    Double_t *r = &(row[i].r00);
    cout << " ";
    for (Int_t j =  0; j <  9; j++) out << Form("%8.5f,",r[j]);
    cout << " ";
    for (Int_t j =  9; j < 12; j++) out << Form("%8.4f,",r[j]); 
    cout << " ";
    for (Int_t j = 12; j < 18; j++) out << Form("%8.5f,",r[j]);
    out << "\"\"}";
    if (i != 23) out << ",";
    out << endl;
  }
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"TpcSuperSectorPositionB\",24);" << endl; 
  out << "  for (Int_t i = 0; i < 24; i++) tableSet->AddAt(&row[i].Id);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
} 

