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
#else
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
#endif
#define __MakeNTuple__
#ifdef __MakeNTuple__
//#include "StBFChain.h"
//#include "StTpcDb/StTpcdEdxCorrection.h"
//#include "St_db_Maker/St_db_Maker.h"
//#include "TInterpreter.h"
// void bfc (const Int_t Last, 
// 	  const Char_t *Chain,
// 	  const Char_t *infile, 
// 	  const Char_t *outfile, 
// 	  const Char_t *TreeFile);
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
  Float_t time, d, t, sector, row, dEdxCor, Z, run;
};
//________________________________________________________________________________
void TpcZ(){ 
/*
 mysql -h dbx.star.bnl.gov --port=3316 -u "" Calibrations_tpc -e 'select beginTime  from TpcZCorrectionB where elementID=1 and npar >0 and deactive = 0  order by beginTime; select beginTime  from TpcDriftDistOxygen where elementID=1 and npar >0 and deactive = 0  order by beginTime; ' | grep ':' | sort -u 
2001-07-01 12:00:00  Hist238  for Run I  (2001) TpcDriftDistCorr 2000-06-17 17:54:30 hist238
                                         (2002) -"-              2001-07-01 00:00:00 RunII08
		     Hist312  for Run II (2002) -"-              2001-07-01 12:00:00
2003-01-06 00:00:00  Hist543  for Run III(2003) -"-              2002-11-05 00:00:00
                     Hist815  for Run III(2003) TpcDriftDistOxygen 2003-01-06 00:00:00 
2003-11-20 00:00:00  Hist970  for Run IV (2004)	-"-                2003-11-20 00:00:00
                     Hist032  for Run V  (2005)	-"-                    
2006-02-10 18:00:00  Hist128  for Run VI (2006)	    
                     RunVII69            (2007)
		     RunVIII20           (2008)
2009-03-01 00:00:39  	                        TpcDriftDistOxygen 2009-04-15 00:00:57 Blair
2009-03-01 00:00:51       
2009-03-01 00:00:58  
2009-04-15 00:00:56  
2009-04-15 00:00:57  
2010-01-01 00:00:30  
2010-01-03 00:00:32
2010-02-04 18:00:30
2010-03-18 20:00:30
2010-04-09 00:00:30
2010-04-24 04:00:30
2010-05-27 02:00:30
2011-01-01 00:01:14                             TpcDriftDistOxygen  2011-01-01 00:01:14
2011-01-01 00:02:06
2011-04-21 00:01:19                              -"-                2011-04-21 00:01:19
2011-04-21 00:02:06        
2011-05-03 00:00:01                              -"-                2011-05-03 00:00:01
2011-06-02 00:00:01                              -"-               
2011-06-20 15:00:00                              -"-
2011-12-10 00:01:00                             
2011-12-20 00:01:00
2012-03-13 14:00:00
2012-03-13 14:00:26
2012-04-23 11:24:12
2012-12-10 00:01:00
2012-12-20 00:01:00
2013-03-05 00:00:13
2013-12-10 00:01:00
2013-12-20 00:01:00
2014-01-01 00:00:05
2020-12-10 00:01:00
2020-12-15 00:01:00

*/
  Date_t dates[] = {
#if 1
    {20010701,120000, 1},
    //    {20030106,     0, 2},
    {20060210,180000, 6},
    {20090628, 70000, 9},
    {20100528, 50000,10},
    {20110620,150000,11},
#endif
    {20120415, 50000,12},
#if 1
    {20130421, 50000,13},
    {20140309, 90530,14},
#endif
    {       0,     0, 0}
  };
#if 1
  gROOT->LoadMacro("bfc.C");
  TString Chain("ry2001,mysql,StarMagField,StDbT,TpcDb,dEdxY2,Nodefault");
  bfc(-1,Chain.Data(),0,0,0);
  dbMk = (St_db_Maker *) chain->Maker("db"); 
#endif
  chain->Init(); 
  //  Int_t m_Mask = -1;
  //  SETBIT(m_Mask,StTpcdEdxCorrection::kTpcLast);
  Int_t m_Mask = 0;
  Int_t kzCorrection = 13;
  Int_t kDrift       = 11;
  SETBIT(m_Mask, kzCorrection);
  SETBIT(m_Mask, kDrift);
  Row_t row;
  TFile *f = new TFile("TpcZ.root","RECREATE");
  TNtuple *FitP = new TNtuple("T","Z dependence","time:d:t:sector:row:dEdxCor:Z:run");
  FitP->SetMarkerStyle(20);
  dEdxY2_t CdEdx;
  Double_t dEdxFixed = 2.54e-6;
  StTpcdEdxCorrection *TpcdEdxCorrection = 0;
  Int_t i = 0;
  while (dates[i].date) {
    cout << "i = " << i << " d " << dates[i].date << " t " << dates[i].time << endl;
    StEvtHddr   *hddr = chain->GetEvtHddr();
    hddr->SetRunNumber(i+1);
    hddr->SetDateTime(dates[i].date,dates[i].time);
    TDatime t(dates[i].date,dates[i].time);
    row.time = t.Convert();
    row.d    = dates[i].date;
    row.t    = dates[i].time;
    row.run  = dates[i].run;
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
    for (Int_t sector = 1; sector <= 24; sector++) {
      row.sector = sector;
      Double_t R45 = 0;
      Double_t R13 = 0;
      for (Int_t r = 45; r >= 14; r--) {
	if (! St_tpcAnodeHVavgC::instance()->livePadrow(sector,r)) continue;
	Int_t j = 0;
	Double_t dEdx = dEdxFixed;
	for (Double_t Z = 40; Z <=200; Z+= 2) {
	  CdEdx.Reset();
	  CdEdx.sector = sector; 
	  CdEdx.row    = r;
	  CdEdx.adc    = 100;
	  CdEdx.dEdx   = dEdx;
	  CdEdx.ZdriftDistance = Z;
	  CdEdx.xyzD[0] = 1.;
	  CdEdx.xyzD[1] = CdEdx.xyzD[2] = 0;
	  CdEdx.edge   = 50.;
          CdEdx.PhiR   = 50.;
	  if (r <= 13) {
	    CdEdx.dx     = 1.2;
	  } else {
	    CdEdx.dx     = 2.4;
	  }
	  CdEdx.dE = CdEdx.dEdx*CdEdx.dx;
	  Int_t iok    = TpcdEdxCorrection->dEdxCorrection(CdEdx);
	  if (iok) {
	    cout << dates[i].date << " Z " << Z
		 << " s/r " << sector << "/" << r << " ok = " << iok << " dEdx : " << dEdxFixed << " => " << CdEdx.dEdx <<endl;
	    //	    return;
	  }
	  if (iok) continue;
	//	row.gain     = CdEdx.C[StTpcdEdxCorrection::kTpcLast].dE*1.e6;
	  row.dEdxCor     = dEdxFixed/CdEdx.dEdx;
	  row.row = r;
	  row.Z  = Z;
	  FitP->Fill(&row.time);
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
