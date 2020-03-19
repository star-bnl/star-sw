#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
//#endif
//________________________________________________________________________________
#if !defined(__CINT__) && ! defined(__MAKECINT__) 
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TClassTable.h"
#ifdef __USE_ROOFIT__
#include "RooRealVar.h"
#include "RooDataSet.h"
#include "RooGaussian.h"
#include "RooFFTConvPdf.h"
#include "RooPlot.h"
#include "RooCFunction1Binding.h" 
#include "RooCFunction3Binding.h"
#include "RooTFnBinding.h" 
#include "RooDataHist.h"
#include "RooAbsPdf.h"
#include "RooRealProxy.h"
#include "RooFit.h"
#include "RooRandom.h"
#include "RooFitResult.h"
#include "RooWorkspace.h"
using namespace RooFit ;
#endif /* __USE_ROOFIT__ */
#include "TObjectTable.h"
#include "StBFChain/StBFChain.h"
#include "St_db_Maker/St_db_Maker.h"
#include "St_base/StMessMgr.h"
#include "StRoot/macros/bfc.C"
#else
class TMinuit;
class TF1;
class TH1F;
class TH2F;
class TH3F;
class TProfile;
class TH2D;
class TCanvas;
class TSpectrum;
class TSystem;
class Bichsel;
class St_db_Maker;
class TTable;
class StBFChain;
// Refer to a class implemented in libRooFit to force its loading
// via the autoloader.
#ifdef __USE_ROOFIT__
class Roo2DKeysPdf;
#endif /* __USE_ROOFIT__ */
#endif
St_db_Maker *dbMk = 0;
TTable *table = 0;
#if 0
//________________________________________________________________________________
StBFChain * bfc(Int_t First, Int_t Last,const Char_t *Chain = "", // + ",Display",
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0, const Char_t *chainName=0);
StBFChain *bfc(Int_t First, const Char_t *Chain = "MC2016,20Muons,vmc,Rung.1",
 	       const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0, const Char_t *chainName = "");
#endif
//________________________________________________________________________________
void DbLoad() {
  //  if (gClassTable->GetID("StDbManager") < 0) {
    gROOT->LoadMacro("bfc.C");
    //    bfc(-1,"tpcDb,detDb,CorrX,nodefault");
    bfc(-1,"tpcDb,detDb,mysql,nodefault,CorrX"); // ,dbV20151120");
    dbMk = (St_db_Maker *) chain->Maker("db");
    //  }    
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void Db(const Char_t *tabNam  = 
	//	"Geometry/tpc/tpcPadConfig",
	"Geometry/tpc/itpcPadPlanes",
	Int_t date = -1, Int_t time = 0,
	Int_t debugL = 1,
	const Char_t *flavor="sim+ofl+laserDV"
	){ 
  if (dbMk == 0) DbLoad();
  dbMk->SetDebug(debugL);
  Int_t D = date;
  Int_t T = time;
  if (D <= 0) {
    TDatime dt;
    Int_t i = dt.Convert(kTRUE); // to GMT
    dt.Set(i);
    D = dt.GetDate();
    T = dt.GetTime();
    cout << "Set GMT Date " << D << " Time " << T << endl;
  }
  dbMk->SetDateTime(D,T); 
  TString TabNam(tabNam);
  if (TabNam.BeginsWith("StarDb/")) TabNam.ReplaceAll("StarDb/","");
  TString name(gSystem->BaseName(tabNam));
  TString Flavor(flavor);
  if (Flavor != "")   dbMk->SetFlavor(flavor,name);
  dbMk->Init();
  table = (TTable *) dbMk->GetDataBase(TabNam);
  if (table) {
    TDatime t[2];
    dbMk->GetValidity(table,t);
    cout << "==============================================" << endl;
    Int_t Nrows = table->GetNRows();
    cout << "Found table " << table->GetName() << " with NRows = " << Nrows << " in db" << endl;
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
#if 0
    if (name == "tpcPadrowT0") {
      Double_t t0Inner = 0, t0Outer = 0;
      Int_t      Inner = 0,   Outer = 0;
      tpcPadrowT0_st *row = ((St_tpcPadrowT0 *) table)->GetTable();
      for (Int_t sec = 0; sec < 24; sec++, row++) {
	for (Int_t r = 0; r < 45; r++) {
	  if (row->T0[r]) {
	    if (r < 13) {t0Inner += row->T0[r]; Inner++;}
	    else        {t0Outer += row->T0[r]; Outer++;}
	  }
	}
      }
      if (Inner > 0) t0Inner /= Inner;
      if (Outer > 0) t0Outer /= Outer;
      cout << name.Data() << "\tInner <T0> = " << t0Inner << "\tOuter <T0> = " << t0Outer << endl;
    }
#endif
    if (Nrows > 10) Nrows = 10;
    if (table->GetRowSize() < 256) {
      table->Print(0,Nrows);
      cout << "==============================================" << endl;
      name += Form(".%06i.%06i.C",t[0].GetDate(),t[0].GetTime());
      ofstream out;
      out.open(name, ios::out);
      table->SavePrimitive(out,"");
    } else {
      name += Form(".%06i.%06i.root",t[0].GetDate(),t[0].GetTime());
      TFile *f = new TFile(name.Data(),"RECREATE");
      table->Write();
      delete f;
    }
  }
  else cout << "Table:" << tabNam << " has not been found" << endl;
}
//________________________________________________________________________________
void Db(const Char_t *tabNam,  const Char_t *tag){ 
  if (dbMk == 0) Load();
  cout << "Db(" << tabNam << "," << tag << ")" << endl;
  Int_t date = StMaker::AliasDate(tag);
  Int_t time = StMaker::AliasTime(tag);
  Db(tabNam,date,time);
}
