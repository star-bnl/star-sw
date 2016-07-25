/*  Tpc Super Sector Position
    root.exe lDb.C ResulsG.C+
    > MakeSuperSectorPositionB()
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
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TFile.h"
#include "TNamed.h"
#include "SuperSectorPar.h"
#include "THStack.h"
#include "StChain.h"
#include "tables/St_Survey_Table.h"
#include "TGeoMatrix.h"
#endif
TCanvas *c1 = 0;
THStack *hs[6];
TLegend *leg[6];
//________________________________________________________________________________
void MakeSuperSectorPositionB(){
  const Char_t *plots[3] = {"dXS","dYS","dZS"};
  for (Int_t j = 1; j <= 24; j++) {
    cout << Form("%2i ",j); Passes[NP-1].Data[j-1].Print();
  }
  Int_t date = Passes[NP-1].date;
  Int_t time = Passes[NP-1].time;
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
    TGeoHMatrix dR;
#if 0
    dR.RotateX(-TMath::RadToDeg()*Passes[NP-1].Data[i].alpha*1e-3*1.0);
    dR.RotateY(-TMath::RadToDeg()*Passes[NP-1].Data[i].beta*1e-3*1.0); 
#else
    dR.RotateX(TMath::RadToDeg()*Passes[NP-1].Data[i].alpha*1e-3*1.0);
    dR.RotateY(TMath::RadToDeg()*Passes[NP-1].Data[i].beta*1e-3*1.0); 
#if 0
    dR.RotateZ(TMath::RadToDeg()*Passes[NP-1].Data[i].gamma*1e-3*1.0);
#endif
#endif
    Double_t xyz[3];
    Double_t dxyz[3], drot[3];
    xyz[0] = 1e-4*Passes[NP-1].Data[i].x*1.0;
    xyz[1] = 1e-4*Passes[NP-1].Data[i].y*1.0;
    xyz[2] = 1e-4*Passes[NP-1].Data[i].z*1.0;
    dxyz[0] = 1e-4*Passes[NP-1].Data[i].Dx;
    dxyz[1] = 1e-4*Passes[NP-1].Data[i].Dy;
    dxyz[2] = 1e-4*Passes[NP-1].Data[i].Dz;
    drot[0] = Passes[NP-1].Data[i].Dalpha*1e-3;
    drot[1] = Passes[NP-1].Data[i].Dbeta*1e-3; 
    drot[2] = Passes[NP-1].Data[i].Dgamma*1e-3;
    dR.SetTranslation(xyz);
    memcpy(&row[i].sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row[i].sigmaTrX, dxyz, 3*sizeof(Double_t));
    cout << "Additional rotation for Inner Sector\t"; dR.Print();
    //    TGeoHMatrix GLnew = GL * dR;  cout << "GLnew\t"; GLnew.Print();
    //    TGeoHMatrix GLnew = StTpcDb::instance()->Flip().Inverse()*dR*StTpcDb::instance()->Flip()*GL; GLnew.Print();
    TGeoHMatrix GLnew = GL*dR; GLnew.Print();
    //    TGeoHMatrix GLnew = dR * GL; GLnew.Print();
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
    out << "\"" << Passes[NP-1].PassName << "\"}";
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

//________________________________________________________________________________
void ResultsG(const Char_t *opt="") {
  gStyle->SetMarkerStyle(20);
  gStyle->SetOptStat(0);
  cout << "NP \t" << NP << endl;
  Int_t NH = NP;
  if (NH == 2) NH++; // make average if we have only FF + RF
  TH1D ***dath = new TH1D**[NH]; 
  for (Int_t p = 0; p < NH; p++) {dath[p] = new TH1D*[6]; memset(dath[p],0, 6*sizeof(TH1D*));}
  const Char_t *names[6] = {" #Deltax"," #Deltay"," #Deltaz"," #Delta #alpha"," #Delta #beta"," #Delta #gamma"};
  const Char_t *nameK[6] = {"Dx","Dy","Dz","Da",     "Db",    "Dg"};
  TString Opt(opt);
  for (Int_t i = 0; i < 6; i++) {
    hs[i] = new THStack(nameK[i],names[i]);
#if 0
    if (i < 3) hs[i]->SetYTitle(Form("%s (#mum)",names[i]));
    else       hs[i]->SetYTitle(Form("%s (mrad)",names[i]));
    hs[i]->SetXTitle("sector");
#endif
    Double_t ymin =  1e10;
    Double_t ymax = -1e10;
    TString Name;
    TString Title;
    if (! i)     leg[i] = new TLegend(0.10,0.65,0.30,0.90);
    else         leg[i] = 0;
    TString same("e");
    Int_t color = 1;
    TH1::SetDefaultSumw2(kTRUE);
    for (Int_t k = 0; k < NP; k++) {
      if (k < NP) {
	if (i == 0 && k < NP) Passes[k].Print();
	Name = Form("%s%s",Passes[k].PassName,nameK[i]);
	if (Opt != "" && ! Name.Contains(Opt,TString::kIgnoreCase)) continue;
	Title = Form("Alignment fit for  %s %s",names[i],Passes[k].PassName);
      } else { // Average
	Name = Form("%s%s%s",Passes[0].PassName,Passes[1].PassName,nameK[i]);
	if (Opt != "" && ! Name.Contains(Opt,TString::kIgnoreCase)) continue;
	Title = Form("Alignment fit for %s sum %s %s",names[i],Passes[0].PassName,Passes[1].PassName);
	
      }
      //      cout << Name.Data() << "\t" << Title.Data() << "\ti\t" << i << "\tk\t" << k << endl;
      dath[k][i] = (TH1D *) gDirectory->Get(Name);
      if (dath[k][i]) delete dath[k][i];
      
      dath[k][i] = new TH1D(Name,Title, 24, 0.5, 24.5);
      //      cout << "Create: " << dath[k][i]->GetName() << "\t" << dath[k][i]->GetTitle() << endl;
      dath[k][i]->SetMarkerColor(color);
      dath[k][i]->SetLineColor(color);
      color++;
      dath[k][i]->SetXTitle("sector");
      if (i < 3) dath[k][i]->SetYTitle(Form("%s (#mum)",names[i]));
      else       dath[k][i]->SetYTitle(Form("%s (mrad)",names[i]));
      for (Int_t l = 0; l < 24; l++) {
	Int_t secs;
	Double_t val, err;
	if (k < NP) {
	  Double_t *X = &Passes[k].Data[l].x;
	  secs = Passes[k].Data[l].sector;
	  if (X[2*i+1] >= 0 /* && X[2*i+1] < 99 */) {
	    val = X[2*i];
	    err = X[2*i+1];
	  } else {continue;}
	} else {
	  Double_t *X0 = &Passes[0].Data[l].x;
	  Double_t *X1 = &Passes[1].Data[l].x;
	  secs = Passes[0].Data[l].sector;
	  if (X0[2*i+1] >= 0 /* && X0[2*i+1] < 99 */ &&
	      X1[2*i+1] >= 0 /* && X1[2*i+1] < 99 */) {
	    val = 0.5*(X0[2*i] + X1[2*i]);
	    dath[k][i]->SetBinContent(secs,val);
	    err = TMath::Sqrt(X0[2*i+1]*X0[2*i+1]+X1[2*i+1]*X1[2*i+1])/2;
	  } else {continue;}
	} 
	if (err < 0.001) err = 0.001;
	dath[k][i]->SetBinContent(secs,val);
	dath[k][i]->SetBinError(secs,err);
	if (ymin > val - err) ymin = val - err;
	if (ymax < val + err) ymax = val + err;
      }
      hs[i]->Add(dath[k][i]);
      if (leg[i]) {
	if (k < NP) leg[i]->AddEntry(dath[k][i],Passes[k].PassName);
	else        leg[i]->AddEntry(dath[k][i],"sum");
      }
    }
  }
  c1 = new TCanvas("IO","Tpc Outer to Inner alignment parameters",1200,800);
  c1->Divide(3,2);
  for (Int_t i = 0; i < 6; i++) {
    c1->cd(i+1);
    if (! hs[i]) continue;
    TString same("e");
    Double_t ymax = hs[i]->GetMaximum("nostack");
    Double_t ymin = hs[i]->GetMinimum("nostack");
    TList *list = hs[i]->GetHists();
    TIter next(list);
    TH1 *h = 0;
    while ((h = (TH1*) next())) {
      h->GetYaxis()->SetTitleOffset(1.4);
      if (same == "e") {
	if (ymax > 0)     h->SetMaximum(1.1*ymax);
	else              h->SetMaximum(0.9*ymax);
	if (ymin < 0)     h->SetMinimum(1.1*ymin);
	else              h->SetMinimum(0.9*ymin);
      }
      TString hName(h->GetName());
      if (hName.BeginsWith("db",TString::kIgnoreCase)) h->Draw("same");
      else                                             h->Draw(same);
      same = "same";
    }
    if (leg[i]) leg[i]->Draw();
  }
  c1->Update();
}
