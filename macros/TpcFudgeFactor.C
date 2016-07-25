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
#include "StDetectorDbMaker/St_tpcAnodeHVC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0C.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StBFChain.h"
#include "StTpcDb/StTpcdEdxCorrection.h"
#include "St_db_Maker/St_db_Maker.h"
#include "TInterpreter.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
#else
//R__EXTERN StBFChain *chain;
class St_tpcGas;
class tpcGas_st;
class StTpcdEdxCorrection;
class StBFChain;        
class StMessMgr;
class St_db_Maker;
#endif
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
void TpcFudgeFactor(){ 
  //  2006-04-06 - 2006-06-05
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
  TFile *f = new TFile("TpcFudgeFactorIV.root","RECREATE");
  TNtuple *FitP = new TNtuple("T","Aging","time:d:t:sector:row:gain:AvPadGain:GasGain:GasGain45:GasGain13:run");
  FitP->SetMarkerStyle(20);
  dEdxY2_t CdEdx;
  Double_t dEdxFixed = 2.54e-6;
  Double_t ADCfixedInner = dEdxFixed/St_tss_tssparC::instance()->gain_in();
  Double_t ADCfixedOuter = dEdxFixed/St_tss_tssparC::instance()->gain_out();
  Double_t ZdriftDistance = 100.;
  StTpcdEdxCorrection *TpcdEdxCorrection = 0;
  Int_t i = 7000000;
  St_tpcPadGainT0C *tpcPadGainT0 = 0;
  TDatime t1(20060406,0); UInt_t u1 = t1.Convert();
  TDatime t2(20060605,0); UInt_t u2 = t2.Convert();
  TDatime t;
  for (UInt_t u = u1; u <= u2; u += 3600) {
    t.Set(u);
    UInt_t date = t.GetDate();
    UInt_t time = t.GetTime();
    i++;
    cout << "i = " << i << " d " << date << " t " << time << "\t" << t.AsString() << endl;
    StEvtHddr   *hddr = chain->GetEvtHddr();
    hddr->SetRunNumber(i+1);
    hddr->SetDateTime(date,time);
    row.time = u;
    row.d    = date;
    row.t    = time;
    row.run  = i+1;
    dbMk->SetDateTime(date,time+1); 
    chain->MakeEvent();
    //    dbMk->Make();
    if (TpcdEdxCorrection) delete TpcdEdxCorrection;
    TpcdEdxCorrection = new StTpcdEdxCorrection(m_Mask, 1);
    //    TpcdEdxCorrection->ReSetCorrections();
#if 0
    St_tpcGas           *tpcGas = TpcdEdxCorrection->tpcGas();
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
    if (tpcPadGainT0) delete tpcPadGainT0;
    tpcPadGainT0 = St_tpcPadGainT0C::instance();
    for (Int_t sector = 1; sector <= 24; sector++) {
      row.sector = sector;
      Double_t R45 = 0;
      Double_t R13 = 0;
      for (Int_t r = 45; r >= 1; r--) {
#if 0
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
	Int_t naccepted = 0;
	Double_t Gain = 0;
	row.AvPadGain = 0;
	for (Int_t pad = 1; pad <= 182; pad++) {
	  Gain   = tpcPadGainT0->Gain(sector,r,pad);
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
