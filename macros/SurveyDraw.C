/*
  root.exe lDb.C SurveyDraw.C+
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
#include "TF1.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TFile.h"
#include "THStack.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_Survey_Table.h"
#include "StChain.h"
#include "StEvtHddr.h"
#endif
St_Survey *table = 0;
St_db_Maker *db = 0;
static Int_t pass = 0;
//#define __DB__
#ifdef __DB__
struct db_t {
  Int_t sector;
  Double_t x, gamma;
  const Char_t *comment;
};
db_t Db[24] = {
  { 1,  48.00,   0.56, "2000-05-01 00:00:05"},
  { 2,-241.00,  -0.73, "2000-05-01 00:00:05"},
  { 3, -28.00,  -0.19, "2000-05-01 00:00:05"},
  { 4,  -6.00,  -0.17, "2000-05-01 00:00:05"},
  { 5, 151.00,   0.11, "2000-05-01 00:00:05"},
  { 6, 181.00,  -0.20, "2000-05-01 00:00:05"},
  { 7,   5.00,  -0.34, "2000-05-01 00:00:05"},
  { 8, 360.00,  -0.20, "2000-05-01 00:00:05"},
  { 9,-202.00,  -0.47, "2000-05-01 00:00:05"},
  {10, 108.00,   0.20, "2000-05-01 00:00:05"},
  {11, 108.00,  -0.17, "2000-05-01 00:00:05"},
  {12,  75.00,   0.17, "2000-05-01 00:00:05"},
  {13,  76.00,  -0.26, "2000-05-01 00:00:05"},
  {14, 364.00,  -0.10, "2000-05-01 00:00:05"},
  {15, 190.00,   0.06, "2000-05-01 00:00:05"},
  {16,  50.00,  -0.05, "2000-05-01 00:00:05"},
  {17,-143.00,  -0.08, "2000-05-01 00:00:05"},
  {18,-146.00,   0.15, "2000-05-01 00:00:05"},
  {19,-207.00,  -0.51, "2000-05-01 00:00:05"},
  {20,-213.00,  -0.37, "2000-05-01 00:00:05"},
  {21,-133.00,   0.41, "2000-05-01 00:00:05"},
  {22,  -5.00,   0.35, "2000-05-01 00:00:05"},
  {23,-316.00,  -0.13, "2000-05-01 00:00:05"},
  {24,  29.00,  -0.48, "2000-05-01 00:00:05"} 
};
#endif /* __DB__ */
TCanvas *c1 = 0;
struct data_t {
  Int_t sector;
  Double_t x, Dx, y, Dy, z, Dz, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;
  const Char_t *Comment;
  void Print() {
    cout << Form("%2i %8.2f %5.2f %8.2f %5.2f %8.2f %5.2f",sector, x, Dx, y, Dy, z, Dz)
	 << Form(" %8.2f %5.2f %8.2f %5.2f %8.2f %5.2f %s", alpha, Dalpha, beta, Dbeta, gamma, Dgamma, Comment) << endl;
  }
};
struct SurveyPass_t {
  Int_t date, time;
  const Char_t *PassName;
  data_t Data[24];
#if 0
  SurveyPass_t &operator=(Int_t d, Int_t t, const Char_t *Pass, data_t data[24]) {
    date = d; time = t; PassName = Pass;
    for (Int_t i = 0; i < 24; i++) Data[i] = data[i];
    return *this;
  }
#endif
  void Print() {
    cout << Form("%8i %6i %s",date,time,PassName) << endl;
    for (Int_t i = 0; i < 24; i++) {
      Data[i].Print();
    }
  }
};
THStack *hs[6];
TLegend *leg[6];
const Char_t *names[6] = {" #Deltax"," #Deltay"," #Deltaz"," #Delta #alpha"," #Delta #beta"," #Delta #gamma"};
const Char_t *nameK[6] = {"Dx","Dy","Dz","Da",     "Db",    "Dg"};
const Char_t *hnames[3] = {"Sum","FF","RF"};
//________________________________________________________________________________
Double_t shift(Double_t *x, Double_t *par) {
  return (x[0] < 12.5) ? par[0] : -par[0];
}
//________________________________________________________________________________
TF1 *Shift() {
  TF1 *f = new TF1("Shift",shift,0.5,24.5,1);
  f->SetParameter(0,0.0);
  return f;
}
//________________________________________________________________________________
//void SurveyDraw(const Char_t *tabNam      = "Geometry/tpc/TpcSuperSectorPositionB", Int_t date = 20140101, Int_t time = 1112) {
void SurveyDraw(const Char_t *tabNam  = "Geometry/tpc/TpcSuperSectorPositionB", Int_t date = 20140101, Int_t time = 1450) {
#ifdef __CINT__
  cout << "Please run in AClic mode => SurveyDraw.C+" << endl;
  return;
#endif
  StChain *chain = (StChain *) StChain::GetChain();
  if (! db) {
    db = (St_db_Maker *) chain->Maker("db");
    if (! db) return;
    db->SetDebug(1);
    chain->Init();
  }
  if (date == 0) {
    TDatime t;
    date = t.GetDate();
    cout << "Set Date " << date << " Time " << time << endl;
  }
  //  db->SetDateTime(date,time);
  StEvtHddr *header = chain->GetEvtHddr();
  header->SetRunNumber(1);
  header->SetDateTime(date,time);
  header->Print();
  chain->MakeEvent();
  TString TabNam(tabNam);
  if (TabNam.BeginsWith("StarDb/")) TabNam.ReplaceAll("StarDb/","");
  TString name(gSystem->BaseName(tabNam));
  table = (St_Survey *) db->GetDataBase(TabNam);
  if (! table) {
    cout << "Table:" << tabNam << " has not been found" << endl;
    return;
  }
  TString Out = name;
  Out += Form(".%8i.%06i.root",date,time);
  TFile *fOut = new TFile(Out,"recreate");
  TDatime t[2];
  db->GetValidity(table,t);
  Int_t dateDb = t[0].GetDate();
  Int_t timeDb = t[0].GetTime();
  cout << "==============================================" << endl;
  Int_t Nrows = table->GetNRows();
  cout << "Found table " << table->GetName() << " with NRows = " << Nrows << " in db" << endl;
  cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
       << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
  cout << "==============================================" << endl;
  table->Print(0,10);
  //________________________________________________________________________________
  gStyle->SetMarkerStyle(20);
  gStyle->SetOptStat(0);
  pass++;
  TF1 *fShift = Shift();
  Int_t NH = 1;
  TH1D **dbh = 0;
  Int_t color = 1;
  if (name == "TpcOuterSectorPositionB") {
#ifdef __DB__
    dbh = new TH1D*[6]; 
    memset(dbh, 0, 6*sizeof(TH1D*));
    for (Int_t i = 0; i < 6; i++) {
      if ( ! (i == 0) && !(i == 5)) continue;
      dbh[i] = (TH1D *) gDirectory->Get(Form("Db%s",nameK[i]));
      if (dbh[i]) delete dbh[i];
      dbh[i] = new TH1D(Form("Db%s",nameK[i]),Form("Alignment from Db for %s",names[i]), 24, 0.5, 24.5);
      dbh[i]->SetMarkerStyle(1);
      cout << "Create: " << dbh[i]->GetName() << "\t" << dbh[i]->GetTitle() << endl;
      Int_t m = 0;
      if (i == 5) m = 1;
      for (Int_t j = 0; j < 24; j++) {
	Double_t *XX = &Db[j].x;
	Double_t s = 1;
	if (Db[j].sector > 12) s = -1;
	dbh[i]->SetBinContent(Db[j].sector,s*XX[m]);
      }
      dbh[i]->SetLineColor(color);
      dbh[i]->SetMarkerColor(color);
    }
    color++;
#endif
  }
  if (Nrows == 48) NH += 2;
  TH1D ***dath = new TH1D**[NH]; 
  for (Int_t p = 0; p < NH; p++) {dath[p] = new TH1D*[6]; memset(dath[p],0, 6*sizeof(TH1D*));}
#if 0
  struct Survey {
    long   Id;
    double r00;  0 
    double r01;  1 -gamma 
    double r02;  2  beta  
    double r10;  3  gamma 
    double r11;  4 
    double r12;  5 -alpha  
    double r20;  6 -beta  
    double r21;  7  alpha 
    double r22;  8 
    double t0, t1, t2;  9 10 11 
    double sigmaRotX, sigmaRotY, sigmaRotZ;  12 13 14 
    double sigmaTrX, sigmaTrY, sigmaTrZ;     15 16 17 
    char   comment[32];
  };
#endif
  const Int_t   offset[2][6] = {
    //x   y   z alpha beta gamma
    { 9, 10, 11,   7,   2,    3}, // val
    {15, 16, 17,  12,  13,   14}  // err
  };
  for (Int_t i = 0; i < 6; i++) {
    hs[i] = new THStack(nameK[i],names[i]);
    Double_t scale = 1e4;
    if (i > 2) scale = 1e3;
    TString Name;
    TString Title;
    if (i == 0 && NH > 1)     leg[i] = new TLegend(0.10,0.65,0.30,0.90);
    else            leg[i] = 0;
    TString same("e");
    TH1::SetDefaultSumw2(kTRUE);
    for (Int_t k = 0; k < NH; k++) {
      Name = hnames[k]; Name += nameK[i]; Name += pass;
      Title = "Alignment fit for "; Title += names[i];
      dath[k][i] = (TH1D *) gDirectory->Get(Name);
      if (dath[k][i]) delete dath[k][i];
      dath[k][i] = new TH1D(Name,Title, 24, 0.5, 24.5);
      dath[k][i]->SetMarkerColor(color+k);
      dath[k][i]->SetLineColor(color+k);
      dath[k][i]->SetXTitle("sector");
      if (i < 3) dath[k][i]->SetYTitle(Form("%s (#mum)",names[i]));
      else       dath[k][i]->SetYTitle(Form("%s (mrad)",names[i]));
      for (Int_t l = 0; l < 24; l++) {
	Survey_st *data = table->GetTable() + l;
	Double_t *X = &data->r00;
	Int_t secs;
	Double_t val, err;
	if (k == 0) {
	  secs = data->Id;
	  if (X[offset[1][i]] >= 0 && X[offset[1][i]] < 99) {
	    val = X[offset[0][i]];
	    err = X[offset[1][i]];
	  } else {continue;}
	} else {
	  Double_t *X0 = X;
	  Double_t *X1 = &(data+24)->r00;
	  secs = (data+24)->Id;
	  if (X0[offset[1][i]] >= 0 && X0[offset[1][i]] < 99 &&
	      X1[offset[1][i]] >= 0 && X1[offset[1][i]] < 99) {
	    if (k == 1) val = (X0[offset[0][i]] + X1[offset[0][i]]);
	    else        val = (X0[offset[0][i]] - X1[offset[0][i]]);
	    err = TMath::Sqrt(X0[offset[1][i]]*X0[offset[1][i]]+X1[offset[1][i]]*X1[offset[1][i]])/2;
	  } else {continue;}
	} 
	if (i <=  2 && err < 0.00250) err = 0.00250;
	if (i >   2 && err < 0.00005) err = 0.00005;
	if (i ==  5 && err > 0.00200) err = 1e-7;
	//	if (err < 0.001) err = 0.001;
	dath[k][i]->SetBinContent(secs,scale*val);
	dath[k][i]->SetBinError(secs,scale*err);
      }
      dath[k][i]->Fit(fShift);
      hs[i]->Add(dath[k][i]);
      if (leg[i]) {
	//	cout << "dath[" << k << "][" << i << "] Name = " << dath[k][i]->GetName() << endl;
	//	leg[i]->AddEntry(dath[k][i],dath[k][i]->GetName());
	leg[i]->AddEntry(dath[k][i],hnames[k]);
      }
    }
#ifdef __DB__
    if (dbh[i]) {
      hs[i]->Add(dbh[i]);
      if (leg[i]) leg[i]->AddEntry(dbh[i],"DB 05/01/00");
    }
#endif
  }
  
  TString nCan("c1"), tCan("c1");
  if (name.Contains("TpcOuterSectorPosition")) {
    nCan = "IO"; nCan += Form(".%8i.%06i",dateDb,timeDb);
    tCan = name; tCan +=" Time";  tCan += Form(".%8i.%06i",dateDb,timeDb);
  } else if (name.Contains("TpcSuperSectorPosition")) {
    nCan = "Sup"; nCan += Form(".%8i.%06i",dateDb,timeDb);
    tCan = name; tCan +=" Time";  tCan += Form(".%8i.%06i",dateDb,timeDb);
  }
  c1 = new TCanvas(nCan,tCan,1200,800);
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
  fOut->Write();
}
//________________________________________________________________________________
void Draw() {
  c1 = new TCanvas("c1","c1",1200,800);
  c1->Divide(3,2);
  TSeqCollection *files = gROOT->GetListOfFiles();
  TIter next(files);
  TFile *f = 0;
  for (Int_t i = 0; i < 6; i++) {
    if (i == 0)     leg[i] = new TLegend(0.10,0.65,0.30,0.90);
    else            leg[i] = 0;
    next.Reset();
    Int_t color = 0;
    hs[i] = new THStack(nameK[i],names[i]);
    while ((f = (TFile *) next())) {
      f->cd();
      TString Name;
      TString Title(gSystem->BaseName(f->GetName()));
      Title.ReplaceAll("TpcSuperSectorPositionB.","");
      Title.ReplaceAll(".root","");
      Name = hnames[0]; Name += nameK[i]; Name += 1;
      TH1D *h = (TH1D *) gDirectory->Get(Name);
      if (! h) continue;
      color++;
      h->SetMarkerColor(color);
      h->SetLineColor(color);
      h->SetStats(0);
      TIter nextF(h->GetListOfFunctions());
      TF1 *fun = 0;
      while ((fun = (TF1 *) nextF())) {
	fun->SetLineColor(color);
      }
      hs[i]->Add(h);
      if (leg[i]) leg[i]->AddEntry(h,Title);
    }
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
