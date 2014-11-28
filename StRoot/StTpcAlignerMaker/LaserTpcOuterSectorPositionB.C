/*
  root.exe 15*.Laser.root 'Chain.C("FitP")' lDb.C 'LaserTpcOuterSectorPositionB.C+(tchain,20140101,205)'
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
void LaserTpcOuterSectorPositionB(TChain *FitP = 0, Int_t date = 20140101, Int_t time = 138, Int_t sectorList = 0){
  if (! FitP) return;
  TProfile *profs[4];
  const Char_t *plots[4] = {"dY","dZ","dYdX","dZdX"};
  data_t Data[24]; memset(Data, 0, sizeof(Data));
  Int_t selectedSectors[24]; memset(selectedSectors, 0, sizeof(selectedSectors));
  if (sectorList == 0) {
    for (Int_t i = 0; i < 24; i++) selectedSectors[i] = 1;
  } else {
    Int_t res = sectorList;
    while (res) {
      Int_t sec = res%100; res /= 100;
      if (sec > 0) selectedSectors[sec-1] = 1;
      cout << "Update sector " << sec << endl;
    }
  }
  gStyle->SetOptStat(0);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1",Form("c%i",time));
  c1->Clear();
  c1->Divide(2,2);
  for (Int_t i = 0; i < 4; i++) {
    c1->cd(i+1);
    FitP->Draw(Form("%s:sec >> %s(24,0.5,24.5)",plots[i],plots[i]),"!trackID","prof");
    profs[i] = (TProfile *) gDirectory->Get(plots[i]);
    if (! profs[i]) {cout << "Can't find " << profs[i] << endl; return;}
    for (Int_t j = 1; j <= 24; j++) {
      if      (i == 0) {Data[j-1].dy    =   profs[i]->GetBinContent(j); Data[j-1].ddy    = profs[i]->GetBinError(j);}
      else if (i == 1) {Data[j-1].dz    =   profs[i]->GetBinContent(j); Data[j-1].ddz    = profs[i]->GetBinError(j);}
      else if (i == 2) {Data[j-1].gamma =   profs[i]->GetBinContent(j); Data[j-1].dgamma = profs[i]->GetBinError(j);}
#if 1
      else if (i == 3) {Data[j-1].beta  = - profs[i]->GetBinContent(j); Data[j-1].dbeta  = profs[i]->GetBinError(j);}
#endif      
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
  St_Survey *TpcOuterSectorPositionB = (St_Survey *) chain->GetDataBase("Geometry/tpc/TpcOuterSectorPositionB");
  if (! TpcOuterSectorPositionB)  {cout << "TpcOuterSectorPositionB has not been found"  << endl; return;}
  TpcOuterSectorPositionB->Print(0,24);
  TGeoHMatrix GL;
  Survey_st *TpcOuterSectorOld         = TpcOuterSectorPositionB->GetTable();        
  Survey_st row[24];
  TGeoTranslation T123(123,0,0);
  for (Int_t i = 0; i < 24; i++) {
    row[i] = TpcOuterSectorOld[i];
    GL.SetRotation(&TpcOuterSectorOld[i].r00);
    GL.SetTranslation(&TpcOuterSectorOld[i].t0); cout << "s: " << i+1 << " GL\t"; GL.Print();
    TGeoHMatrix dR, dT;
    if (selectedSectors[i]) {
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
      dR = T123*dR*T123.Inverse();
    }
    cout << "Additional rotation for Inner Sector\t"; dR.Print();
    //    TGeoHMatrix GLnew = GL * dR;  cout << "GLnew\t"; GLnew.Print();
    //    TGeoHMatrix GLnew = StTpcDb::instance()->Flip().Inverse()*dR*StTpcDb::instance()->Flip()*GL; GLnew.Print();
    TGeoHMatrix dRI = dR.Inverse();
    TGeoHMatrix GLnew = StTpcDb::instance()->Flip().Inverse()*dRI*StTpcDb::instance()->Flip()*GL; GLnew.Print();
    Double_t *R = GLnew.GetRotationMatrix();
    memcpy(&row[i].r00, R, 9*sizeof(Double_t));
    Double_t *tr = GLnew.GetTranslation();
    memcpy(&row[i].t0, tr, 3*sizeof(Double_t));
  }
  TString fOut =  Form("TpcOuterSectorPositionB.%8i.%06i.C", date, time);
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
  out << "  St_Survey *tableSet = new St_Survey(\"TpcOuterSectorPositionB\",24);" << endl; 
  out << "  for (Int_t i = 0; i < 24; i++) tableSet->AddAt(&row[i].Id);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
} 

