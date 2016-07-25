#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TLegend.h"
#include "TChain.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StEvtHddr.h"
#include "TFile.h"
#include "TMath.h"
#include "TGraph.h"
#endif
//#define __MakeNTuple__
#ifdef __MakeNTuple__
//#include "StBFChain.h"
//#include "StTpcDb/StTpcdEdxCorrection.h"
//#include "St_db_Maker/St_db_Maker.h"
#include "StDetectorDbMaker/St_tpcGainC.h"
//#include "TInterpreter.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
//R__EXTERN StBFChain *chain;
class St_tpcGas;
class tpcGas_st;
class StTpcdEdxCorrection;
class StBFChain;        
class StMessMgr;
class St_db_Maker;
StBFChain   *chain=0; 
St_db_Maker *dbMk = 0;
StTpcdEdxCorrection *TpcdEdxCorrection = 0;
struct Date_t {
  Int_t date;
  Int_t time;
  Int_t run;
};
struct Row_t {
  Float_t time, d, t, sector, row, gain,  AvPadGain, GasGain, GasGain45, GasGain13, run;
};
//________________________________________________________________________________
void TpcAging(){ 
/*
mysql> select entryTime,elementID,beginTime,flavor,deactive,npar from TpcLengthCorrectionB where elementID=1 and deactive = 0 and entryTime > "2005" order by beginTime;
+---------------------+-----------+---------------------+--------+----------+------+
| entryTime           | elementID | beginTime           | flavor | deactive | npar |
+---------------------+-----------+---------------------+--------+----------+------+
| 2007-11-05 10:50:39 |         1 | 2001-09-24 00:00:04 | ofl    |        0 |   10 |
| 2005-02-14 12:36:54 |         1 | 2003-11-20 00:00:01 | ofl    |        0 |   -2 |
| 2005-02-14 12:37:52 |         1 | 2004-01-04 00:00:01 | ofl    |        0 |   -2 |
| 2005-02-14 12:38:13 |         1 | 2004-02-05 00:00:01 | ofl    |        0 |   -2 |
| 2005-02-14 12:38:41 |         1 | 2004-02-17 00:00:01 | ofl    |        0 |   -2 |
| 2005-02-14 12:39:03 |         1 | 2004-03-24 00:00:01 | ofl    |        0 |   -2 |
| 2005-02-14 12:39:26 |         1 | 2004-04-04 00:00:01 | ofl    |        0 |   -2 |
| 2005-05-09 12:55:32 |         1 | 2005-01-11 22:00:00 | ofl    |        0 |    5 |
| 2005-05-13 16:38:42 |         1 | 2005-01-11 22:00:01 | ofl    |        0 |    5 |
| 2005-07-29 16:16:55 |         1 | 2005-04-03 01:00:00 | ofl    |        0 |   -9 |
| 2006-08-20 17:45:22 |         1 | 2006-03-08 11:58:00 | ofl    |        0 |   -9 |
| 2006-08-08 12:00:55 |         1 | 2006-04-06 05:00:00 | ofl    |        0 |  -10 |
| 2006-08-08 12:03:10 |         1 | 2006-05-10 15:06:01 | ofl    |        0 |  -10 |
| 2007-10-04 12:23:33 |         1 | 2007-03-21 00:00:41 | ofl    |        0 |  -10 |
| 2007-10-08 16:53:48 |         1 | 2007-03-21 00:00:42 | ofl    |        0 |  -10 |
| 2008-07-07 10:43:27 |         1 | 2007-11-01 00:00:16 | ofl    |        0 |  -10 |
| 2008-07-07 10:42:22 |         1 | 2008-01-28 00:00:16 | ofl    |        0 |  -10 |
+---------------------+-----------+---------------------+--------+----------+------+
*/
  Date_t dates[] = {
    {20010924,      4, 1},
//     {20031120,      1, 2},
//     {20040104,      1, 3},
//     {20040205,      1, 4},
//     {20040217,      1, 5},
//     {20040324,      1, 6},
//     {20040404,      1, 7},
    {20050111, 220001, 2},
    {20050403,  10001, 3},
    {20060308, 115801, 4},
    {20060406,  50000, 5},
    {20060510, 150601, 6},
    {20070321,     42, 7},
    {20071101,     16, 8},
    {20080128,     16, 9}, //                     Inner                     Outer
    {20090301,    101,10}, // pp500NV tpcAnodeHVC 1996-01-01 00:00:00 1170  -"-                 1390
    {20090428, 132101,11}, // pp200NV tpcAnodeHVC 2009-04-28 13:20:00 1170 
    {20090428, 174101,12}, // pp200IV                        17:37:00 1135
    {20090428, 183901,13}, // pp200IO                                       2009-04-28 18:38:40  1345   
    {20090630, 130000,14}, // pp2pp pp200
    {       0,      0, 0}
  };
  gROOT->LoadMacro("bfc.C");
  TString Chain("ry2001,mysql,StarMagField,StDbT,TpcDb,Nodefault");
  bfc(-1,Chain.Data(),0,0,0);
  dbMk = (St_db_Maker *) chain->Maker("db"); 
  dbMk->SetDebug(1);
  dbMk->SetDateTime(2001,1);
  chain->Init(); chain->Make();
  Int_t m_Mask = -1;
  //  SETBIT(m_Mask,StTpcdEdxCorrection::kTpcLast);
  Row_t row;
  TFile *f = new TFile("Aging20090715.root","RECREATE");
  TNtuple *FitP = new TNtuple("T","Aging","time:d:t:sector:row:gain:AvPadGain:GasGain:GasGain45:GasGain13:run");
  FitP->SetMarkerStyle(20);
  dEdxY2_t CdEdx;
  Double_t dEdxFixed = 2.54e-6;
  Double_t ADCfixedInner = dEdxFixed/St_tss_tssparC::instance()->gain_in();
  Double_t ADCfixedOuter = dEdxFixed/St_tss_tssparC::instance()->gain_out();
  Double_t ZdriftDistance = 100.;
  StTpcdEdxCorrection *TpcdEdxCorrection = 0;
  Int_t i = 0;
  St_tpcGainC *tpcGain = 0;
  while (dates[i].date) {
    cout << "i = " << i << " d " << dates[i].date << " t " << dates[i].time << endl;
    StEvtHddr   *hddr = chain->GetEvtHddr();
    hddr->SetRunNumber(i+1);
    hddr->SetDateTime(dates[i].date,dates[i].time);
    TDatime t(dates[i].date,dates[i].time);
    row.time = t.Convert();
    row.d    = dates[i].date;
    row.t    = dates[i].time;
    row.run  = i+1;
    dbMk->SetDateTime(dates[i].date,dates[i].time+1); 
    chain->MakeEvent();
    //    dbMk->Make();
    if (TpcdEdxCorrection) delete TpcdEdxCorrection;
    TpcdEdxCorrection = new StTpcdEdxCorrection(m_Mask, 1);
    //    TpcdEdxCorrection->ReSetCorrections();
    St_tpcGas           *tpcGas = TpcdEdxCorrection->tpcGas();
#if 1
    if (tpcGas) {
      Float_t conditions[16] = {
	1018.2 , // barometricPressure     
	1.99 , // inputTPCGasPressure    
	1.13 , // nitrogenPressure       
	0.73 , // gasPressureDiff        
	297.66 , // inputGasTemperature    
	297.87 , // outputGasTemperature   
	14.98 , // flowRateArgon1         
	0.44 , // flowRateArgon2         
	1.36 , // flowRateMethane        
	10.17 , // percentMethaneIn       
	27.39 , // ppmOxygenIn            
	11.52 , // flowRateExhaust        
	10.21 , // percentMethaneOut      
	7.86 , // ppmWaterOut            
	-0.27 , // ppmOxygenOut           
	539.55}; // flowRateRecirculation  
      memcpy (tpcGas->GetTable(), conditions, sizeof(tpcGas_st));
      //      tpcGas->Print(0,1);
    };
#endif
    if (tpcGain) delete tpcGain;
    tpcGain = St_tpcGainC::instance();
    for (Int_t sector = 1; sector <= 24; sector++) {
      row.sector = sector;
      Double_t R45 = 0;
      Double_t R13 = 0;
      for (Int_t r = 45; r >= 1; r--) {
#if 1
	if (sector ==  2 && r >= 14 && r <= 21) continue;
	if (sector ==  4 && r >=  1 && r <=  7) continue;
	if (sector ==  5 && r >= 30 && r <= 37) continue;
	if (sector ==  6 && r >= 22 && r <= 37) continue;
	if (sector == 11 && r >=  1 && r <=  7) continue;
	if (sector == 11 && r >= 38 && r <= 45) continue;
	if (sector == 12 && r >= 10 && r <= 13) continue;
	if (sector == 18 && r >=  1 && r <=  7) continue;
	if (sector == 18 && r >= 10 && r <= 13) continue;
	if (sector == 18 && r >= 38 && r <= 45) continue;
	if (sector == 19 && r >= 38 && r <= 45) continue;
	if (sector == 20 && r >= 14 && r <= 21) continue;
#endif
	if (! St_tpcAnodeHVC::instance()->livePadrow(sector,r)) continue;
	Int_t j = 0;
	Double_t dEdx = dEdxFixed;
	if (r <= 13) {
	  j = 0;
	  dEdx = dEdxFixed/St_tss_tssparC::instance()->gain_in()*St_tss_tssparC::instance()->gain_in(sector,r);
	} else {
	  j = 1;
	  dEdx = dEdxFixed/St_tss_tssparC::instance()->gain_out()*St_tss_tssparC::instance()->gain_out(sector,r);
	}
	dEdx /= avrGain[j];
	Int_t naccepted = 0;
	Double_t Gain = 0;
	row.AvPadGain = 0;
	for (Int_t pad = 1; pad <= 182; pad++) {
	  Gain   = tpcGain->Gain(sector,r,pad);
	  if (Gain > 0 && Gain < 8) {
	    row.AvPadGain += Gain;
	    naccepted++;
	  }
	}
	if (! naccepted) continue;
	row.AvPadGain /= naccepted;
	CdEdx.Reset();
	CdEdx.sector = sector; 
	CdEdx.row    = r;
	CdEdx.dEdx   = dEdx;
	CdEdx.ZdriftDistance = ZdriftDistance;
	CdEdx.xyzD[0] = 1.;
	CdEdx.xyzD[1] = CdEdx.xyzD[2] = 0;
	CdEdx.edge   = 50.;
	if (r <= 13) {
	  CdEdx.dx     = 1.2;
	} else {
	  CdEdx.dx     = 2.4;
	}
	CdEdx.dE = CdEdx.dEdx*CdEdx.dx;
	Int_t iok    = TpcdEdxCorrection->dEdxCorrection(CdEdx);
	if (iok) continue;
	//	row.gain     = CdEdx.C[StTpcdEdxCorrection::kTpcLast].dE*1.e6;
	row.gain     = dEdxFixed/CdEdx.dEdx;
	row.GasGain  = 1./(row.gain*row.AvPadGain);
	row.GasGain45 = -1;
	row.GasGain13 = -1;
	if (r == 45) R45 = row.GasGain;
	if (r == 13) R13 = row.GasGain;
	if (R45 > 0) row.GasGain45 = row.GasGain/R45;
	if (R13 > 0) row.GasGain13 = row.GasGain/R13;
	row.row = r;
	FitP->Fill(&row.time);
	if (sector == 1 && r == 1) {
	  cout << "time " << t.AsString() << " s/r " << sector << "/" << r << " gain " <<  row.gain 
	       << " PedGain " << row.AvPadGain  << " ===================================" << endl;
	}
      }
    }    
    i++;
  }
  f->Write();
}
#endif /* ! __MakeNTuple__ */
//________________________________________________________________________________
void Draw() {
  //Double_t avrGain[2] = {1.70183e+00, 1.93559e+00}; // Inner , Outer for run = 1
  Double_t avrGain[2] = {2.278, 2.651};
  gStyle->SetOptStat(1000000001);
  TNtuple *T = (TNtuple *) gDirectory->Get("T");
  if (! T ) return;
  TF1 *f = new TF1("f","[0]+[1]*x"); // %/year
  //  T->Draw("gain:time","gain>0&&time>1060e6&&(time<1.04e9||time>1.044e9)&&row==41","sameprof")
  //  TString Select("d>20050000&&gain>1&&time>1060e6&&row==");
  TString Select("gain>1&&row==");
  T->SetMarkerStyle(20);
  TCanvas *c1 = new TCanvas("c1","Tpc gas gain");
  TH1F *frame = c1->DrawFrame(2001,0.8,2010,1.2);
  frame->SetTitle("Tpc gas gain");
  frame->SetXTitle("Date (years)");
  frame->SetYTitle("Gain");
//   gStyle->SetTimeOffset(-788936400);
//   frame->GetXaxis()->SetTimeDisplay(1);
//   frame->GetXaxis()->SetTimeFormat("%m\/%y");
//   gPad->Modified();
  TLegend *leg = new TLegend(0.1,0.1,0.55,0.25,"");
  TCanvas *c2 = new TCanvas();
  Int_t marker = 20;
  Int_t color = 0;
  TH1D *R = new TH1D("r99","slope row dependence",45,0.5,45.5);
  for (Int_t r = 1; r <= 45; r++) {
    c2->cd();
    TString hname(Form("r%i",r));
    Int_t i = 0;
    if (r > 13) i = 1;
    if (r == 14) {marker = 21; color = 0;}
    TString plot(Form("%f/gain:run >> %s",avrGain[i],hname.Data())); 
    TString select = Select;
    select += Form("%i",r);
    color++;
    T->SetMarkerStyle(marker);
    T->SetMarkerColor(color);
    T->Draw(plot,select,"prof");
    TProfile *prof = (TProfile *) gDirectory->Get(hname);
    if (prof) {
      if (prof->GetEntries() <= 2) continue;
      prof->SetMinimum(0.8);
      prof->SetMaximum(1.2);
      f->SetLineColor(color);
      prof->Fit(f,"e");
      leg->AddEntry(prof,Form("%s %7.3f +.- %7.3f",hname.Data(),f->GetParameter(1),f->GetParError(1)));
      R->SetBinContent(r,f->GetParameter(1));
      R->SetBinError(r,f->GetParError(1));
      c1->cd();
      prof->Draw("same");
    }
  }
  c1->cd();
  leg->Draw();
  c2->cd();
  R->Fit("pol0","er","",0,13.5);
  R->Fit("pol0","er+","",13.5,45.5);
}
//________________________________________________________________________________
TProfile *Project2P(Int_t run=10, Int_t color = 1, 
		    const Char_t *title = "the beginning of Run IX PP500 and begin and end of PP200",
		    Double_t scale = 1.0) {
  TProfile *R10 =(TProfile *) gDirectory->Get(Form("R%i",run));
  if (R10) return R10;
  TNtuple *T = (TNtuple *) gDirectory->Get("T");
  if (! T ) return 0;
  R10 = new TProfile(Form("R%i",run),Form("%s",title),45,0.5,45.5);
  R10->SetStats(0);
  R10->SetMarkerStyle(20);
  R10->SetMarkerColor(color);
  R10->SetXTitle("pad row");
  R10->SetYTitle("Gais Gain [arb.units]");
  T->Draw(Form("%f/gain:row>>R%i",scale,run),Form("abs(gain-1)<0.2&&(run==%i)",run),"prof");
  return R10;
}
//________________________________________________________________________________
TH1D *Ratio(const Char_t *name = "ratio", TH1 *h1=0, TH1 *h2=0) {
  TH1D *ratio = (TH1D *) gDirectory->Get(name);
  if (ratio) return ratio;
  if (! h1 || ! h2) return 0;
  ratio = new TH1D(name,Form("Ratio %s to %s",h2->GetTitle(),h1->GetTitle()), 45,0.5,45.5);
  ratio->SetMarkerColor(h2->GetMarkerColor());
  ratio->SetMarkerStyle(h2->GetMarkerStyle());
  ratio->SetXTitle(h2->GetXaxis()->GetTitle());
  ratio->SetYTitle("ratio");
  for (Int_t i = 1; i <= 45; i++) {
    Double_t v1 = h1->GetBinContent(i);
    Double_t v2 = h2->GetBinContent(i);
    Double_t e1  = h1->GetBinError(i);
    Double_t e2  = h2->GetBinError(i);
    if (e1 > 0 && e2 > 0) {
      Double_t err = TMath::Sqrt((e1*e1)/(v1*v1) + (e2*e2)/(v2*v2));
      ratio->SetBinContent(i, v2/v1);
      ratio->SetBinError(i,err);
    }
  }
  return ratio;
}
//________________________________________________________________________________
void PadRadii() { //           1        2        3        4        5 
  Double_t innerR[13] = { 60.000,  64.800,  69.600,  74.400,  79.200, //  5 
			  84.000,  88.800,  93.600,  98.800, 104.000, // 10
			 109.200, 114.400, 119.600};
  Double_t outerR[32] = {                           127.195, 129.195, // 15
			 131.195, 133.195, 135.195, 137.195, 139.195, // 20
			 141.195, 143.195, 145.195, 147.195, 149.195, // 25
			 151.195, 153.195, 155.195, 157.195, 159.195, // 30
                         161.195, 163.195, 165.195, 167.195, 169.195, // 35
			 171.195, 173.195, 175.195, 177.195, 179.195, // 40
			 181.195, 183.195, 185.195, 187.195, 189.195};// 45
  Double_t x[45];
  Double_t y[45];
  for (Int_t r = 1; r <= 45; r++) {
    x[r-1] = r;
    if (r <= 13) y[r-1] = innerR[r-1];
    else         y[r-1] = outerR[r-14];
  }
  TGraph *gr = new TGraph(45,x,y);
  gr->SetMarkerStyle(20);
  gr->Draw("axp");
  gr->Fit("pol1","er" ,"",0,13.5);  // 5.45846e+01 + 4.95385e+00*r
  gr->Fit("pol1","er+","",13.5,46); // 9.91950e+01 + 2.00000e+00*r
}
//________________________________________________________________________________
void SigmaVsRow() {
  Int_t NF = 4;
  Char_t *SecRowFiles[4] = {
    "500NV",
    "200NV",
    "200IV",
    "200IO"};
  TProfile *pf[4];
  gStyle->SetOptDate(0);
  for (Int_t i = 0; i < NF; i++) {
    TFile *f = TFile::Open(Form("SecRow3CGFRunIX11DEV_pp%s.root",SecRowFiles[i]));
    TTree *FitP = (TTree *) gDirectory->Get("FitP");
    if (! FitP) continue;
    FitP->SetMarkerColor(i+1);
    FitP->Draw(Form("sigma:j>>pp%s(45,0.5,45.5)",SecRowFiles[i]),"i&&j","prof");
    pf[i] = (TProfile *) f->Get(Form("pp%s",SecRowFiles[i]));
    if (! pf[i]) continue;
    pf[i]->SetTitle("sigma");
    //    pf[i]->SetYTitle("sigma");
    pf[i]->SetXTitle("pad row");
    pf[i]->SetStats(0);
    TF1 *pol1 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol1");
    if (pol1) pol1->SetLineColor(i+1);
    pf[i]->Fit("pol1","er","",0.5,13.5);
    pf[i]->Fit("pol1","er+","",13.5,45.5);
  };
  TLegend *leg = new TLegend(0.1,0.6,0.9,0.9,"");;
  Char_t *Legend[4] = {
    "pp500 Nominal Volatage       : I/O = 1170 (1.00) /1390 (1.00) V",
    "pp200 Nominal Volatage       : I/O = 1170 (1.00) /1390 (1.00) V",
    "pp200 Reduced Inner          : I/O = 1135 (0.63) /1390 (1.00) V",
    "pp200 Reduced Inner and Outer: I/O = 1135 (0.63) /1345 (0.63) V"
  };
  for (Int_t i = 0; i < NF; i++) {
    if (i == 0) {pf[i]->SetMaximum(0.5); pf[i]->SetMinimum(0.4); pf[i]->Draw();}
    else        pf[i]->Draw("same");
    leg->AddEntry(pf[i],Legend[i]);
  }
  leg->Draw();
}
//________________________________________________________________________________
void AgingPlotTripped() {//
  /* Joe's List of tripped channels more than 5 times
     S  C N  Group rows
     10 6 7  6-7     sector==10&&(row>=8&&row<=10)
     10 7 6
     11 5 6  5-8     sector==11&&(row>=7&&row<=10)
     11 6 6 
     11 7 6
     11 8 6
     12 2 6  2       sector==12&&(row<=2)
     15 1 48 1-2     sector==14&&(row<=2)
     15 2 17
     17 1 12 1       sector==17&&(row==1)
     18 2 8  2,4     sector==18&&(row<=2||row==5||row==6)
     18 4 45
     19 2 6  2
     20 2 6  2
     22 1 6  1-4, 6-8 sector==22&&(row<=6||row>=8&&row<=13)
     22 2 10
     22 3 8
     22 4 6
     22 6 6
     22 7 6
     22 8 6
     23 1 6 1        sector==23&&row==1
     3 2 6  2-4      sector==3&&(row<=6)
     3 3 7
     3 4 27 
     4 1 43 1-4      sector==4&&row<=6
     4 2 43
     4 3 28
     4 4 20
     5 2 6  2
     7 1 6  1, 6     sector==7&&(row==1||row>=8&&row<=9)
     7 6 42
     9 1 7  1-2      sector==9&&(row<=2)
     9 2 6

     Gene: Channels 4-8 in sector 12, 5 in sector 20 (not on), and all channels in sector 24 never tripped.
   */
  TString Tripped("!(sector==10&&(row>=8&&row<=10) || sector==11&&(row>=7&&row<=10) || sector==12&&(row<=2) || sector==14&&(row<=2) || sector==17&&(row==1) || sector==18&&(row<=2||row==5||row==6) || sector==22&&(row<=6||row>=8&&row<=13) || sector==23&&row==1 || sector==3&&(row<=6) || sector==4&&row<=6 || sector==7&&(row==1||row>=8&&row<=9) || sector==9&&(row<=2))");
  gStyle->SetOptStat(1000000001);
  TNtuple *T = (TNtuple *) gDirectory->Get("T");
  if (! T ) return;
  TProfile *R10 = new TProfile("R10","Gain at the beginning of Run IX PP500 and PP200",45,0.5,45.5);
  R10->SetMarkerStyle(20);
  R10->SetXTitle("pad row");
  R10->SetYTitle("Gais Gain [arb.units]");
  TString cut = Tripped;
  cut += "&&abs(gain-1)<0.2&&(run==10)";
  T->Draw("1/gain:row>>R10",cut,"prof");
  TProfile *R11 = new TProfile("R11","Gas Gain at the beginning of Run IX PP200",45,0.5,45.5);
  R11->SetStats(0);
  R11->SetMarkerStyle(20);
  R11->SetMarkerColor(2);
  R11->SetLineColor(2);
  cut =  Tripped;
   cut += "&&abs(gain-1)<0.2&&(run==11)";
  T->Draw("1/gain:row>>R11",cut,"prof");
  TLegend *leg = new TLegend(0.65,0.15,0.9,0.35,"");
  R10->SetMinimum(0.85);
  TCanvas *c1 = new TCanvas("Aging","Tpc Aging");
  c1->Divide(1,2);
  c1->cd(1);
  R10->SetStats(0);
  R10->Draw(); leg->AddEntry(R10,"PP500 03/24/09");
  R11->Draw("same"); leg->AddEntry(R11,"PP200 04/28/09");
  leg->Draw();
  TH1D *ratio = new TH1D("ratio","Gas gains ratio PP200 to PP500", 45,0.5,45.5);
  ratio->SetMarkerStyle(20);
  ratio->SetXTitle("pad row");
  ratio->SetYTitle("ratio");
  for (Int_t i = 1; i <= 45; i++) {
    Double_t v1 = R10->GetBinContent(i);
    Double_t v2 = R11->GetBinContent(i);
    Double_t e1  = R10->GetBinError(i);
    Double_t e2  = R11->GetBinError(i);
    if (e1 <= 0 || e2 <= 0) continue;
    Double_t err = TMath::Sqrt((e1*e1)/(v1*v1) + (e2*e2)/(v2*v2));
    ratio->SetBinContent(i, v2/v1);
    ratio->SetBinError(i,err);
  }
  c1->cd(2);
  //  ratio->Fit("pol1","er","",0,14);
  //  TF1 *qI = new TF1("qI","[0] - 6.25e-3*89.3/([1]*(5.45846e+01 + 4.95385e+00*x))",0,13.5);
  //                             Q /    L <R>
  //  TF1 *qI = new TF1("qI","[0] - 10./1.6e5*89.3/([1]*(5.45846e+01 + 4.95385e+00*x))",0,13.5);
  //                  Q     L            <r>
  Double_t R1 = 53.2;
  Double_t R2 = 118.2;
  Double_t QOverL = 10./1.6e5*((R1 + R2)/2);
#if 0
  TF1 *qI = new TF1("qI","[2]*TMath::Exp(-TMath::Sqrt([0]/(5.45846e+01 + 4.95385e+00*x))/[1])",0,13.5);
  qI->SetParameters(QOverL,0.1,1);
  qI->SetParNames("<Q/L>","Q(C/cm)^{1/2}","N");
#else
  TF1 *qI = new TF1("qI","[2]*TMath::Exp(-[0]/(5.45846e+01 + 4.95385e+00*x)/[1])",0,13.5);
  qI->SetParameters(QOverL,0.01,1);
  qI->SetParNames("<Q/L>","R^{-1}(C/cm)","N");
#endif
  qI->FixParameter(0,QOverL);
  qI->FixParameter(2,1);
  //  qI->SetParameter(2,1);
  ratio->Fit("qI","er");
  //  TF1 *qO = new TF1("qO","[0] - 7.53e-6*158.2/([1]*(9.91950e+01 + 2.00000e+00*x))",13.5,46);
  //                             Q      /   L   <R>
  //  TF1 *qO = new TF1("qO","[0] - 10./3.18/3.6e5*158.2/([1]*(9.91950e+01 + 2.00000e+00*x))",13.5,46);
  R1 = 122.8;
  R2 = 191.2;
  QOverL = 10./3.18/3.6e5*((R1+R2)/2);
#if 0
  TF1 *qO = new TF1("qO","[2]*TMath::Exp(-TMath::Sqrt([0]/(9.91950e+01 + 2.00000e+00*x))/[1])",13.5,46);
  qO->SetParNames("<Q/L>","1/R'(C/cm)^{1/2}");
#else
  TF1 *qO = new TF1("qO","[2]*TMath::Exp(-[0]/(9.91950e+01 + 2.00000e+00*x)/[1])",13.5,46);
  qO->SetParNames("<Q/L>","R^{-1}(C/cm)");
#endif
  qO->FixParameter(0,QOverL);
  qO->FixParameter(1,qI->GetParameter(1));
  qO->FixParameter(2,qI->GetParameter(2));
  //  ratio->Fit("qO","er+");
  qO->Draw("same");
}
//________________________________________________________________________________
Double_t Gain(Double_t *x, Double_t *p) {
#if 0
  static const Double_t Ri1 = 53.2;
  static const Double_t Ri2 = 118.2;
  static const Double_t Ro1 = 122.8;
  static const Double_t Ro2 = 191.2;
#endif
  //  static const Double_t RA[2] = {(118.2 - 53.2)/TMath::Log(118.2/53.2), (191.2 - 122.8)/TMath::Log(191.2/122.8)};
  static const Double_t RA[2] = {81.42, 154.484};
  static const Double_t WireLenth[2] = {1.6e5, 3.6e5};
  Double_t Value = 0;
  Int_t model = (Int_t ) p[0]; 
  Int_t row   = (Int_t ) x[0];
  Int_t kPeriod     = (Int_t ) p[1]; // index to inner Q*<R> for two sets Middle and End
  if (x[0] > 45.5) {
    kPeriod = 1;
    row -= 45;
  }
  Double_t R = 0;
  Int_t   io = 0;
  if (row < 14) {R = 5.45846e+01 + 4.95385e+00*row; io = 0;}
  else          {R = 9.91950e+01 + 2.00000e+00*row; io = 1;}
  Double_t QL = (p[2+io+2*kPeriod]+p[7+io])/WireLenth[io];   // Q
  Double_t QL0 = (p[7+io])/WireLenth[io];   // Q
  Double_t QR = QL*RA[io]*RA[io];
  Double_t QR0 = QL0*RA[io]*RA[io];
  Double_t VL = 0;
  if (model == 0) {// exp(-R*(Q<r>/r))
    VL = -(QR/(R*R))/p[6];
  } else          {// exp(-R'*sqrt(Q<r>**2>/r**2))
    VL = -p[6]*(TMath::Sqrt(QR)/R - TMath::Sqrt(QR0)/R);
  }
  if (VL > 80.) VL = 80;
  Value = TMath::Exp(VL);
  return Value;
}
//________________________________________________________________________________
Double_t Gain45(Double_t *x, Double_t *p) {
  Double_t x45 = 38;
  if (x[0] > 45) x45 += 45;
  Double_t g45 = Gain(&x45,p);
  if (g45 <= 0) return 0;
  return Gain(x,p)/g45;
}
//________________________________________________________________________________
void AgingPlot() {// assuming 1/R**2 charge dependence and R
  gStyle->SetOptStat(1000000001);
  TNtuple *T = (TNtuple *) gDirectory->Get("T");
  if (! T ) return;
  TProfile *R10 = Project2P(10,1,"PP500 (03/24/09)");
  TProfile *R11 = Project2P(11,2,"PP200 (04/28/09)");
  TProfile *R14 = Project2P(14,3,"pp2pp (06/30/09)",1.03);
  TLegend *leg = new TLegend(0.45,0.65,0.75,0.90,"");
  leg->SetTextSize(0.04);
  R10->SetMinimum(0.85);
  TCanvas *c1 = new TCanvas("Aging","Tpc Aging");
#if 0
  c1->Divide(1,2);
  c1->cd(1);
  R10->Draw();       leg->AddEntry(R10,R10->GetTitle());
  R11->Draw("same"); leg->AddEntry(R11,R11->GetTitle());
  R14->Draw("same"); leg->AddEntry(R14,R14->GetTitle());
  leg->Draw();
#endif
  TH1D *ratio = Ratio("ratio",R10,R11);
  ratio->SetMinimum(0.92);
  TH1D *ratio2 = Ratio("ratio2",R10,R14);
  TH1D *Ratio = new TH1D("Ratio1","Combined in one histogram Middle and End ratios",90,0.5,90.5);
  Ratio->SetMarkerStyle(20);
  for (Int_t i = 0; i <= 90; i++) {
    Int_t j = i;
    TH1 *r = ratio;
    if (i > 45) {
      j = i - 45;
      r = ratio2;
    }
    Ratio->SetBinContent(i,r->GetBinContent(j));
    Ratio->SetBinError(i,r->GetBinError(j));
  }
  TF1 *qR = new TF1("qR",Gain45,1,90,9);
  qR->SetNpx(90);
  qR->SetParName( 0,"model"   ); qR->FixParameter( 0, 0);				   
  qR->SetParName( 1,"Period"  ); qR->FixParameter( 1, 0);				   
  qR->SetParName( 2,"Q_InnerM"); qR->FixParameter( 2,10.0               );
  qR->SetParName( 3,"Q_OuterM"); qR->FixParameter( 3,10.0/3.18          );
  qR->SetParName( 4,"Q_InnerE"); qR->FixParameter( 4,10.0     +12.6     );
  qR->SetParName( 5,"Q_OuterE"); qR->FixParameter( 5,10.0/3.18+12.6/2.29);
  qR->SetParName( 6,"R"       ); qR->SetParameter( 6, 0.1);                                
  qR->SetParName( 7,"Q0_inner"); qR->FixParameter( 7, 40.);// 
  qR->SetParName( 8,"Q0_outer"); qR->FixParameter( 8, 12.);// 
  Ratio->Fit(qR,"er");	
#if 1
  TF1 *qS = new TF1("qS",Gain45,1,90,9);
  qS->SetNpx(90);
  qS->SetParName( 0,"model"   ); qS->FixParameter( 0, 1);				   
  qS->SetParName( 1,"Period"  ); qS->FixParameter( 1, 0);				   
  qS->SetParName( 2,"Q_InnerM"); qS->FixParameter( 2,10.0               );
  qS->SetParName( 3,"Q_OuterM"); qS->FixParameter( 3,10.0/3.18          );
  qS->SetParName( 4,"Q_InnerE"); qS->FixParameter( 4,10.0     +12.6     );
  qS->SetParName( 5,"Q_OuterE"); qS->FixParameter( 5,10.0/3.18+12.6/2.29);
  qS->SetParName( 6,"R'"      ); qS->SetParameter( 6, 3.0);                                
  qS->SetParName( 7,"Q0_inner"); qS->FixParameter( 7, 40.);//qS->SetParLimits( 7, 0.0, 40.);// 
  qS->SetParName( 8,"Q0_outer"); qS->FixParameter( 8, 12.);//qS->SetParLimits( 8, 0.0, 12.);// 
  qS->SetLineColor(2);
  Ratio->Fit(qS,"er+");
  TCanvas("Comdined Fit","Combined Fit");
  TLegend *leg2 = new TLegend(0.4,0.1,0.9,0.36);
  ratio->SetStats(0);
  ratio->Draw(); leg2->AddEntry(ratio,ratio->GetTitle());
  ratio2->Draw("same"); leg2->AddEntry(ratio2,ratio2->GetTitle());
  TF1 *qRM = new TF1(*qR); qRM->SetRange(1.0,45.0);
  qRM->SetNpx(45);
  qRM->SetLineColor(2);
  qRM->Draw("same");
  leg2->AddEntry(qRM,Form("R^{-1} = %5.3f #pm %5.3f (C/cm)",qRM->GetParameter(6),qRM->GetParError(6)));
  TF1 *qRE = new TF1(*qR); qRE->SetRange(1.0,45.0);
  qRE->SetNpx(45);
  qRE->SetLineColor(3);
  qRE->FixParameter(1,1);
  qRE->Draw("same");
  leg2->AddEntry(qRE,"06/30/09");
  TF1 *qSM = new TF1(*qS); qSM->SetRange(1.0,45.0);
  qSM->SetNpx(45);
  qSM->SetLineColor(4);
  //  qSM->SetLineStyle(3);
  qSM->SetLineWidth(4);
  qSM->Draw("same");
  leg2->AddEntry(qSM,Form("R' = %5.3f #pm %5.3f (C/cm)^{-1/2}",qSM->GetParameter(6),qSM->GetParError(6)));
  TF1 *qSE = new TF1(*qS); qSE->SetRange(1.0,45.0);
  qSE->SetNpx(45);
  qSE->SetLineColor(6);
  //  qSE->SetLineStyle(3);
  qSE->SetLineWidth(4);
  qSE->FixParameter(1,1);
  qSE->Draw("same");
  leg2->AddEntry(qSE,"06/30/09");
  leg2->Draw();
#endif
}
//________________________________________________________________________________
void AgingPlotR2() {// assuming 1/R**2 charge dependence and R
  gStyle->SetOptStat(1000000001);
  TNtuple *T = (TNtuple *) gDirectory->Get("T");
  if (! T ) return;
  TProfile *R10 = Project2P(10,1,"PP500 (03/24/09)");
  TProfile *R11 = Project2P(11,2,"PP200 (04/28/09)");
  TProfile *R14 = Project2P(14,3,"pp2pp (06/30/09)",1.03);
  TLegend *leg = new TLegend(0.45,0.65,0.75,0.90,"");
  leg->SetTextSize(0.04);
  R10->SetMinimum(0.85);
  TCanvas *c1 = new TCanvas("Aging","Tpc Aging");
  c1->Divide(1,2);
  c1->cd(1);
  R10->Draw();       leg->AddEntry(R10,R10->GetTitle());
  R11->Draw("same"); leg->AddEntry(R11,R11->GetTitle());
  R14->Draw("same"); leg->AddEntry(R14,R14->GetTitle());
  leg->Draw();
  TH1D *ratio = Ratio("ratio",R10,R11);
  ratio->SetMinimum(0.92);
  TH1D *ratio2 = Ratio("ratio2",R10,R14);
  c1->cd(2);
  //  ratio->Fit("pol1","er","",0,14);
  //  TF1 *qI = new TF1("qI","[0] - 6.25e-3*89.3/([1]*(5.45846e+01 + 4.95385e+00*x))",0,13.5);
  //                             Q /    L <R>
  //  TF1 *qI = new TF1("qI","[0] - 10./1.6e5*89.3/([1]*(5.45846e+01 + 4.95385e+00*x))",0,13.5);
  //                  Q     L            <r>
  TLegend *leg2 = new TLegend(0.35,0.10,0.90,0.35);
  leg2->SetTextSize(0.04);
  Double_t R1 = 53.2;
  Double_t R2 = 118.2;
  Double_t RA = (R2 - R1)/TMath::Log(R2/R1);
  Double_t QOverL = 10./1.6e5*RA*RA;
  TF1 *qI = new TF1("qI","[2]*TMath::Exp(-[0]/TMath::Power((5.45846e+01 + 4.95385e+00*x),2)/[1])",0,13.5);
  qI->SetLineColor(2);
  qI->SetLineStyle(1);
  qI->SetParameters(QOverL,0.01,1);
  qI->SetParNames("<Q/L>","R^{-1}(C/cm)","N");
  qI->FixParameter(0,QOverL);
  qI->FixParameter(2,1);
  //  qI->SetParameter(2,1);
  ratio->SetStats(0);
  ratio->Fit("qI","er");

  TF1 *qISq = new TF1("qISq","[2]*TMath::Exp(-TMath::Sqrt([0])/(5.45846e+01 + 4.95385e+00*x)*[1])",0,13.5);
  qISq->SetLineColor(2);
  qISq->SetLineStyle(2);
  qISq->SetParameters(QOverL,0.01,1);
  qISq->SetParNames("<Q/L>","R'(C/cm)^{-1/2}","N");
  qISq->FixParameter(0,QOverL);
  qISq->SetParameter(1,3.0);
  qISq->FixParameter(2,1);
  ratio->Fit("qISq","er+");
  leg2->AddEntry(ratio,ratio->GetTitle());
  leg2->AddEntry(ratio2,ratio2->GetTitle());
  leg2->AddEntry(qI,Form("R^{-1} = %6.4f #pm %6.4f (C/cm)",qI->GetParameter(1),qI->GetParError(1)));
  leg2->AddEntry(qISq,Form("R' = %6.4f #pm %6.4f (C/cm)^{-1/2}",qISq->GetParameter(1),qISq->GetParError(1)));
  ratio2->Draw("same");

  //  TF1 *qI2 = new TF1(*qI);
  TF1 *qI2 = new TF1("qI2","[2]*TMath::Exp(-[0]/TMath::Power((5.45846e+01 + 4.95385e+00*x),2)/[1])",0,13.5);
  //  qI2->SetName("qI2");
  QOverL = (10.+12.6)/1.6e5*RA*RA;
  qI2->FixParameter(0,QOverL);
  qI2->FixParameter(1,qI->GetParameter(1));
  qI2->FixParameter(2,qI->GetParameter(2));
  qI2->SetLineColor(3);
  qI2->SetLineStyle(1);
  qI2->Draw("same");
  
  //  TF1 *qI2Sq = new TF1(*qISq);
  TF1 *qI2Sq = new TF1("qI2Sq","[2]*TMath::Exp(-TMath::Sqrt([0])/(5.45846e+01 + 4.95385e+00*x)*[1])",0,13.5);
  qI2Sq->SetLineColor(3);
  qI2Sq->SetLineStyle(2);
  qI2Sq->FixParameter(0,QOverL);
  qI2Sq->FixParameter(1,qISq->GetParameter(1));
  qI2Sq->FixParameter(2,qISq->GetParameter(2));
  qI2Sq->Draw("same");
#if 1 
  R1 = 122.8;
  R2 = 191.2;
  RA = (R2 - R1)/TMath::Log(R2/R1);
  QOverL = (10./3.18/3.6e5)*RA*RA;
  
  TF1 *qO = new TF1("qO","[2]*TMath::Exp(-[0]/TMath::Power((9.91950e+01 + 2.00000e+00*x),2)/[1])",13.5,46);
  qO->SetLineColor(2);
  qO->SetParNames("<Q/L>","R^{-1}(C/cm)^(C/cm)");
  qO->FixParameter(0,QOverL);
  qO->FixParameter(1,qI->GetParameter(1));
  qO->FixParameter(2,qI->GetParameter(2));
  //  ratio->Fit("qO","er+");
  qO->Draw("same");
  TF1 *qOSq =  new TF1("qOSq","[2]*TMath::Exp(-TMath::Sqrt([0])/(5.45846e+01 + 4.95385e+00*x)*[1])",13.5,46);
  qOSq->SetLineColor(2);
  qOSq->SetLineStyle(2);
  qOSq->SetParNames("<Q/L>","R'(C/cm)^{-1/2}","N");
  qOSq->FixParameter(0,QOverL);
  qOSq->FixParameter(1,qISq->GetParameter(1));
  qOSq->FixParameter(2,1);
  qOSq->Draw("same");
  //  TF1 *qO2 = new TF1(*qO);
  TF1 *qO2 = new TF1("qO2","[2]*TMath::Exp(-[0]/TMath::Power((9.91950e+01 + 2.00000e+00*x),2)/[1])",13.5,46);
  qO2->SetLineColor(3);
  qO2->SetParNames("<Q/L>","R^{-1}(C/cm)^(C/cm)");
  qO2->SetLineColor(3);
  QOverL = ((10./3.18+12.6/2.29)/3.6e5)*RA*RA;
  qO2->FixParameter(0,QOverL);
  qO2->FixParameter(1,qI->GetParameter(1));
  qO2->FixParameter(2,qI->GetParameter(2));
  qO2->Draw("same");
  //  TF1 *qO2Sq = new TF1(*qOSq);
  TF1 *qO2Sq =  new TF1("qO2Sq","[2]*TMath::Exp(-TMath::Sqrt([0])/(5.45846e+01 + 4.95385e+00*x)*[1])",13.5,46);
  qO2Sq->SetParNames("<Q/L>","R'(C/cm)^{-1/2}","N");
  qO2Sq->SetLineColor(3);
  qO2Sq->SetLineStyle(2);
  qO2Sq->FixParameter(0,QOverL);
  qO2Sq->FixParameter(1,qISq->GetParameter(1));
  qO2Sq->FixParameter(2,qISq->GetParameter(2));
  qO2Sq->Draw("same");
#endif
  leg2->Draw();
  new TCanvas("pp200Vspp2pp","pp200Vspp2pp");
  TH1D *ratio3 = Ratio("ratio3",R11,R14);
  ratio3->Draw();
  R1 = 53.2;
  R2 = 118.2;
  RA = (R2 - R1)/TMath::Log(R2/R1);
  QOverL = 12.6/1.6e5*RA*RA;
  TLegend *leg3 = new TLegend(0.35,0.10,0.90,0.35);
  leg3->SetTextSize(0.022);
  leg3->AddEntry(ratio3,ratio3->GetTitle());
  
  TF1 *qI3 = new TF1("qI3","[2]*TMath::Exp(-[0]/TMath::Power((5.45846e+01 + 4.95385e+00*x),2)/[1])",0,13.5);
  qI3->SetLineColor(2);
  qI3->SetLineStyle(1);
  qI3->SetParameters(QOverL,0.01,1);
  qI3->SetParNames("<Q/L>","R^{-1}(C/cm)","N");
  qI3->FixParameter(0,QOverL);
  qI3->FixParameter(2,1);
  //  qI3->SetParameter(2,1);
  ratio3->SetStats(0);
  ratio3->Fit("qI3","er");
  TF1 *qI3Sq = new TF1("qI3Sq","[2]*TMath::Exp(-TMath::Sqrt([0])/(5.45846e+01 + 4.95385e+00*x)*[1])",0,13.5);
  qI3Sq->SetLineColor(2);
  qI3Sq->SetLineStyle(2);
  qI3Sq->SetParameters(QOverL,0.01,1);
  qI3Sq->SetParNames("<Q/L>","R'(C/cm)^{-1/2}","N");
  qI3Sq->FixParameter(0,QOverL);
  qI3Sq->SetParameter(1,3.0);
  qI3Sq->FixParameter(2,1);
  ratio3->Fit("qI3Sq","er+");
  leg3->AddEntry(qI3,Form("R^{-1} = %6.4f #pm %6.4f (C/cm)",qI3->GetParameter(1),qI3->GetParError(1)));
  leg3->AddEntry(qI3Sq,Form("R' = %6.4f #pm %6.4f (C/cm)^{-1/2}",qI3Sq->GetParameter(1),qI3Sq->GetParError(1)));

  R1 = 122.8;
  R2 = 191.2;
  RA = (R2 - R1)/TMath::Log(R2/R1);
  QOverL = (12.6/2.29/3.6e5)*RA*RA;
  TF1 *qO3 = new TF1("qO3","[2]*TMath::Exp(-[0]/TMath::Power((9.91950e+01 + 2.00000e+00*x),2)/[1])",13.5,46);
  qO3->SetLineColor(2);
  qO3->SetParNames("<Q/L>","R^{-1}(C/cm)^(C/cm)");
  qO3->FixParameter(0,QOverL);
  qO3->FixParameter(1,qI3->GetParameter(1));
  qO3->FixParameter(2,qI3->GetParameter(2));
  //  ratio->Fit("qO3","er+");
  qO3->Draw("same");
  TF1 *qO3Sq =  new TF1("qO3Sq","[2]*TMath::Exp(-TMath::Sqrt([0])/(5.45846e+01 + 4.95385e+00*x)*[1])",13.5,46);
  qO3Sq->SetLineColor(2);
  qO3Sq->SetLineStyle(2);
  qO3Sq->SetParNames("<Q/L>","R'(C/cm)^{-1/2}","N");
  qO3Sq->FixParameter(0,QOverL);
  qO3Sq->FixParameter(1,qI3Sq->GetParameter(1));
  qO3Sq->FixParameter(2,1);
  qO3Sq->Draw("same");
  leg3->Draw();
}
