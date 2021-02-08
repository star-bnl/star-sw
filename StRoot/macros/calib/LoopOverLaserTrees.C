// $Id: LoopOverLaserTrees.C,v 1.15 2014/03/19 20:52:37 fisyak Exp $
// $Log: LoopOverLaserTrees.C,v $
// Revision 1.15  2014/03/19 20:52:37  fisyak
// Tight good velocity cut
//
// Revision 1.14  2014/03/13 21:58:43  fisyak
// Add more plots
//
// Revision 1.13  2013/05/28 20:08:49  fisyak
// Increase cut on membrane drift distance precision 1e-3 => 2e-3
//
// Revision 1.12  2013/05/28 06:59:56  fisyak
// Add drift velocity estimation from membrane cluster positions if standard procedure fails
//
// Revision 1.11  2013/05/20 12:35:12  fisyak
// Set time stamp at the begin of the laser run
//
// Revision 1.10  2013/05/16 15:25:39  fisyak
// Relax cut on drift velocity precision : 5e-4 => 1e-3
//
// Revision 1.9  2010/01/02 23:41:19  genevb
// Fix issues with dates starting with 2010
//
// Revision 1.8  2010/01/02 19:29:35  genevb
// switch to laser.root files as default
//
// Revision 1.7  2009/03/11 14:39:33  fisyak
// relax cuts on no. of good lasers and drift velocity precision
//
// Revision 1.6  2009/03/06 22:46:43  fisyak
// Increase acceptable drift velocity interval from [5.5,5.9] to [5.2,5.9]
//
// Revision 1.2  2008/04/25 15:25:15  fisyak
// Freeze macros
//
// Revision 1.4  2007/12/28 13:20:25  fisyak
// Use average drift velocity from East and West
//
// Revision 1.3  2007/12/10 19:54:02  fisyak
// Add Id and Log, correct spelling error in README
//
/* 
   foreach r (53004 53015 53022)
      root.exe -q -b 'LoopOverLaserTrees.C+("*'${r}'*.laser.root","LaserPlots.'${r}'.root")' >& ${r}.log
   end
 */
//#define ADJUSTABLE_BINNING
//#define __REFIT__
//#define INTEGRATE_OVER_HOURS
#define SeparateWestandEast
#if !defined(__CINT__) || defined(__MAKECINT__)
//#include <ostream>
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
#include "TString.h"
#include "TList.h"
#include "TLegend.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#endif
static Int_t run = 0;
static Int_t date = 0;
static Int_t Time = 0;
//             Delta/dv  All,West,East
static Double_t  DVAll[2][3];
static Double_t dDVAll[2][3];
static Int_t  _debug = 0; 
TH2D *dv = 0;
TH2D *slope = 0;
TH2D *memAdc = 0;
//             io
TH2D *zMembrane[2];
//              io  we xy
TH2D *dMembraneY[2][2][2];
TNtuple *runNT = 0;
struct Run_t {
  Float_t run, date, time, events, day, dvAll, ddvAll, dvWest, ddvWest, dvEast, ddvEast, slAll, dslAll, slWest, dslWest, slEast, dslEast;
  Float_t vWest, vEast, zWI, dzWI, zEI, dzEI, zWO, dzWO, zEO, dzEO, utime, ok, dvSet, XWI, dXWI, XEI, dXEI, XWO, dXWO, XEO, dXEO, YWI, dYWI, YEI, dYEI, YWO, dYWO, YEO, dYEO;
};
const Char_t *vRun = "run:date:time:events:day:dvAll:ddvAll:dvWest:ddvWest:dvEast:ddvEast:slAll:dslAll:slWest:dslWest:slEast:dslEast:vWest:vEast:zWI:dzWI:zEI:dzEI:zWO:dzWO:zEO:dzEO:utime:ok:dvSet:XWI:dXWI:XEI:dXEI:XWO:dXWO:XEO:dXEO:YWI:dYWI:YEI:dYEI:YWO:dYWO:YEO:dYEO";
Run_t Run;
//________________________________________________________________________________
Double_t ScaleE2W(Double_t day) {// scale East to West drift velocity
  //  RunNT->Draw("1e3*(dvEast/dvWest-1):day>>diffE(30,90,180)","(ddvWest>0&&ddvWest<4e-5&&ddvEast>0&&ddvEast<4e-5)/((ddvWest/dvWest)**2+(ddvEast/dvEast)**2)","profw")
  static Double_t par[2] = {1.57361e-01,-4.59752e-03};
  return par[0] + par[1]*day;
}
//________________________________________________________________________________
void MakeTable() {
  Double_t dv      =  DVAll[0][0];
  Double_t ddv     = dDVAll[0][0];
  Double_t dvWest  =  DVAll[0][1];
  Double_t ddvWest = dDVAll[0][1];
  Double_t dvEast  =  DVAll[0][2]; 
  Double_t ddvEast = dDVAll[0][2]; 
  Run.ok = 0; // ok == 0 => use both: west and east; ok == 1 => use averaged drift velocities; ok > 1 ==> no. acceptable drift velocities
  if (dvWest < 5.3 || dvWest > 5.9 || ddvWest <= 0 || ddvWest> 5e-5 ||
      dvEast < 5.3 || dvEast > 5.9 || ddvEast <= 0 || ddvEast> 5e-5) {
    //  if (! (dvWest < 5.3 && dvWest > 5.9 && ddvWest < 0 && ddvWest> 5e-5) ||
    //      ! (dvEast < 5.3 && dvEast > 5.9 && ddvEast < 0 && ddvEast> 5e-5)) {
    cout << "Run " << run << " fails ============================= to make separated East and West drift velocities" << endl;
    cout << "vWest = " << dvWest << " +/- " << ddvWest 
	 << "\tvEast = " << dvEast << " +/- " << ddvEast << endl;
    Run.ok = 1;
  }
#ifndef SeparateWestandEast
  Run.ok = 1;
#endif
  //  if (ok == 1 && ! (dv > 5.3 && dv < 5.9 && ddv > 0 && ddv< 5e-5)) {
  if (Run.ok == 1 && (dv < 5.3 || dv > 5.9 || ddv <= 0 || ddv > 5e-5)) {
    cout << "Run " << run << " fails ============================= to make averaged drift velocities" << endl;
    cout << "v = " << dv << " +/- " << ddv << endl;
    Run.ok = 2;
  }
  if (Run.ok == 2 && (Run.date < 130101 ||  Run.date >  140000)) return;
  if (Run.ok == 2) { // try to drift length for Run XIII
    if (Run.dzWO > 2e-3 || Run.dzEO > 2e-3 ||
	Run.zWO + Run.zEO < 409.9 ||
	Run.zWO + Run.zEO > 410.5) {Run.ok = 3; return;}
    /*
      RunNT->Draw("1e6*dvAll/vWest-1:zEO+zWO>>D(10,409.9,410.5)","dzWO<1e-3&&dzEO<1e-3&&ddvAll<1e-3&&ok<2","prof")
      D->Fit("pol1","e")
      FCN=4.90371 FROM MINOS     STATUS=FAILURE       156 CALLS         389 TOTAL
      EDM=5.83814e-09    STRATEGY= 1      ERR MATRIX NOT POS-DEF
      EXT PARAMETER                APPROXIMATE        STEP         FIRST   
      NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
      1  p0           5.85670e-01   2.04554e-04   2.04554e-04   2.08675e-04
      2  p1          -1.42815e-03   4.98660e-07   4.98660e-07   3.42967e+03
     */
    dv = 1e-6*Run.vWest*(1. + (5.85670e-01 -1.42815e-03*(Run.zWO + Run.zEO)));
  }
  TString fOut =  Form("tpcDriftVelocity.%8i.%06i.C",date,Time);
  Double_t scaleY = 0;
  Double_t dscaleY = 0;
  Float_t *par = &Run.YWI;
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t we = 0; we < 2; we++) {
      if (par[4*io+2*we+1] > 0) {
	Double_t w2 = par[4*io+2*we+1]*par[4*io+2*we+1];
	scaleY  += par[4*io+2*we]/w2;
	dscaleY += 1./w2;
      }
    }
  }
  if (dscaleY > 0) {
    scaleY  /= dscaleY;
    dscaleY = 1./TMath::Sqrt(dscaleY);
  }
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcDriftVelocity\")) return 0;" << endl;
  out << "  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity(\"tpcDriftVelocity\",1);" << endl;
  out << "  tpcDriftVelocity_st row;// Laser Run " << run << endl;
  out << "  memset(&row, 0, tableSet->GetRowSize());"<< endl;
  if (! Run.ok) {// ok == 0 => use both: west and east
    Run.dvSet = dvWest;
#if 0
    if (! (dvWest > 5.5 && dvWest < 5.9 && ddvWest > 0 && ddvWest< 1e-3) ) {// West From East
      ddvWest = -1;
      dvWest = dvEast/(1 + 1e-3*ScaleE2W(Run.day));
    } 
    if (! (dvEast > 5.5 && dvEast < 5.9 && ddvEast > 0 && ddvEast< 1e-3) ) {// East from West
      ddvEast = -1;
      dvEast = dvWest*(1 + 1e-3*ScaleE2W(Run.day));
    }
#endif 
    
    out << "  row.laserDriftVelocityEast	 =   " << dvEast << "; // +/- " << ddvEast 
	<< " cm/us East: Slope = " << DVAll[1][2] << " +/- " << dDVAll[1][2] << " DV = " << dvEast << " +/- " << ddvEast
	<< endl;
    out << "  row.laserDriftVelocityWest	 =   " << dvWest << "; // +/- " << ddvWest 
	<< " cm/us West: Slope = " << DVAll[1][1] << " +/- " << dDVAll[1][1] << " DV = " << dvWest << " +/- " << ddvWest<< endl;
#if 0
    out << "  row.cathodeDriftVelocityEast	 =          0; // cm/us : from cathode emission  ;" << endl;
    out << "  row.cathodeDriftVelocityWest	 =          0; // cm/us : from cathode emission  ;" << endl;
#endif
    out << "//row.scaleY                  	 = " << scaleY << ";// +/-" << dscaleY << endl;
    out << "  tableSet->AddAt(&row); " << endl;
    out << "  return (TDataSet *)tableSet; // 1e3*Delta: All = " << dv << " +/- " << ddv << endl;
  } else { // averaged drif tvelocity
    Run.dvSet = dv;
    out << "  row.laserDriftVelocityEast	 =   " << dv << "; // +/- " << ddv 
	<< " cm/us All: East = " << DVAll[1][2] << " +/- " << dDVAll[1][2];
    if (Run.ok == 2) out << " From Membrane";
    out << endl;
    out << "  row.laserDriftVelocityWest	 =   " << dv << "; // +/- " << ddv 
	<< " cm/us All: West = " << DVAll[1][1] << " +/- " << dDVAll[1][1] << endl;
#if 0
    out << "  row.cathodeDriftVelocityEast	 =          0; // cm/us : from cathode emission  ;" << endl;
    out << "  row.cathodeDriftVelocityWest	 =          0; // cm/us : from cathode emission  ;" << endl;
#endif
    out << "  tableSet->AddAt(&row);// 1e3*Delta: All = " << dv << " +/- " << ddv << endl;
    out << "  return (TDataSet *)tableSet;//" 
	<< " West = " << dvWest << " +/- " << ddvWest
	<< " East = " << dvEast << " +/- " << ddvEast << endl;
  }
  out << "};" << endl;
}
//________________________________________________________________________________
void Fit() {
  static TDatime t0(2007,1,1,0,0,0);
  static UInt_t u0 = t0.Convert();
  date = 20000000 + (Int_t) Run.date;
  Time = (Int_t) Run.time;
  TDatime t(date, Time);
  Run.utime = t.Convert();
  Run.day = 1. + (Run.utime - u0)/(24.*60.*60.);
  run  = (Int_t) (1000000*((Int_t) (Run.date/1000000)) + Run.run);
  memset(&DVAll[0][0], 0, 6*sizeof(Double_t));
  memset(&dDVAll[0][0], 0, 6*sizeof(Double_t));
  Run.events = slope->GetEntries();
  cout << "Run " << run << " has " << Run.events << " entries" <<  endl;
  TH2D *hist[2] = {dv, slope};
  Float_t *par = &Run.dvAll;
  for (Int_t l = 0; l < 2; l++) {
#ifdef ADJUSTABLE_BINNING
    hist[l]->BufferEmpty();
#endif
    hist[l]->Write();
    hist[l]->FitSlicesY(0,1,0,10,"QNRI");
    TString fitN(hist[l]->GetName());
    fitN += "_1";
    TH1D *fit = (TH1D *) gDirectory->Get(fitN);
    if (! fit) continue;
    fit->SetMarkerStyle(20);
    Double_t xmin = fit->GetXaxis()->GetXmin();
    Double_t xmax = fit->GetXaxis()->GetXmax();
    TF1 *pol0 = (TF1*) gROOT->GetFunction("pol0");
    if (pol0) {
      for (Int_t k = 0; k < 3; k++) {
	pol0->SetLineColor(k+1);
	if (k == 0) fit->Fit(pol0,"er","",xmin,xmax); 
	if (k == 1) {
	  if (xmin >= 13) continue;
	  fit->Fit(pol0,"er+","",xmin,12.5);
	}
	if (k == 2) {
	  if (xmax <= 15) continue;
	  fit->Fit(pol0,"er+","",13.5,xmax);
	}
	DVAll[l][k]  = pol0->GetParameter(0);
	dDVAll[l][k] = pol0->GetParError(0);
	par[2*(k+3*l)] = DVAll[l][k];
	par[2*(k+3*l)+1] = dDVAll[l][k];
      }
    }
    fit->Write();
  }
  // Memberane
  for (Int_t io = 0; io < 2; io++) {
    zMembrane[io]->FitSlicesY(0,1,0,10,"QNRI");
    TString fitN(zMembrane[io]->GetName());
    fitN += "_1";
    TH1D *fit = (TH1D *) gDirectory->Get(fitN);
    if (! fit) continue;
    fit->SetMarkerStyle(20);
    Float_t *par = &Run.zWI;
    for (Int_t we = 0; we < 2; we++) {
      TF1 *pol0 = (TF1*) gROOT->GetFunction("pol0");
      if (we == 0) fit->Fit(pol0,"er","",0.5,12.5);
      else         fit->Fit(pol0,"er+","",12.5,24.5);
      par[4*io+2*we  ] = pol0->GetParameter(0);
      par[4*io+2*we+1] = pol0->GetParError(0);
    }
    fit->Write();
  }
  // X & Y slope from Memberane
  for (Int_t xy = 0; xy < 2; xy++) {
    for (Int_t io = 0; io < 2; io++) {
      Float_t *par = &Run.XWI;
      for (Int_t we = 0; we < 2; we++) {
	dMembraneY[io][we][xy]->FitSlicesY(0,1,0,10,"QNRI");
	TString fitN(dMembraneY[io][we][xy]->GetName());
	fitN += "_1";
	TH1D *fit = (TH1D *) gDirectory->Get(fitN);
	if (! fit) continue;
	fit->SetMarkerStyle(20);
	TF1 *pol1 = (TF1*) gROOT->GetFunction("pol1");
	fit->Fit(pol1);
	par[8*xy+4*io+2*we  ] = pol1->GetParameter(1)/210;
	par[8*xy+4*io+2*we+1] = pol1->GetParError(1)/210;
	fit->Write();
      }
    }
  }
   MakeTable();
  runNT->Fill(&Run.run);
}
//________________________________________________________________________________
void LoopOverLaserTrees(const Char_t *files="./st_laser_*.laser.root", const Char_t *Out = "LaserPlots.root") {
  TDirIter Dir(files);
  TTreeIter iter("laser");
  iter.AddFile(files);
  //  const Int_t&       fEvtHdr_fEvtNum                          = iter("fEvtHdr.fEvtNum");
  const Int_t&       fEvtHdr_fRun                             = iter("fEvtHdr.fRun");
  const Int_t&       fEvtHdr_fDate                            = iter("fEvtHdr.fDate");
  const Int_t&       fEvtHdr_fTime                            = iter("fEvtHdr.fTime");
  //  const Float_t&     fEvtHdr_ftZero                           = iter("fEvtHdr.ftZero");
  const Float_t&     fEvtHdr_fDriVel                          = iter("fEvtHdr.fDriVel");
  const Float_t&     fEvtHdr_fDriVelWest                      = iter("fEvtHdr.fDriVelWest");
  const Float_t&     fEvtHdr_fDriVelEast                      = iter("fEvtHdr.fDriVelEast");
  const Float_t&     fEvtHdr_fClock                           = iter("fEvtHdr.fClock");
  //  const Float_t&     fEvtHdr_fTrigger                         = iter("fEvtHdr.fTrigger");
  //  const Float_t&     fEvtHdr_fDriftDistance                   = iter("fEvtHdr.fDriftDistance");
  //  const Float_t&     fEvtHdr_fInnerSectorzOffset              = iter("fEvtHdr.fInnerSectorzOffset");
  //  const Float_t&     fEvtHdr_fOuterSectorzOffset              = iter("fEvtHdr.fOuterSectorzOffset");
  //  const Float_t&     fEvtHdr_ftriggerTimeOffset               = iter("fEvtHdr.ftriggerTimeOffset");
  const Int_t&       fNhit                                    = iter("fNhit");
  const UShort_t*&   fHits_sector                             = iter("fHits.sector");
  const UShort_t*&   fHits_row                                = iter("fHits.row");
  const Float_t*&    fHits_xyz_mX1                            = iter("fHits.xyz.mX1");
  const Float_t*&    fHits_xyz_mX2                            = iter("fHits.xyz.mX2");
  const Float_t*&    fHits_xyz_mX3                            = iter("fHits.xyz.mX3");
  const Float_t*&    fHits_xyzL_mX3                            = iter("fHits.xyzL.mX3");
  const Float_t*&    fHits_xyzTpcL_mX1                            = iter("fHits.xyzTpcL.mX1");
  const Float_t*&    fHits_xyzTpcL_mX2                            = iter("fHits.xyzTpcL.mX2");
  const Float_t*&    fHits_xyzTpcL_mX3                            = iter("fHits.xyzTpcL.mX3");
  const UShort_t*&    Adc                                      = iter("fHits.hit.mAdc"); 
  const Float_t&     fEvtHdr_fOnlClock                        = iter("fEvtHdr.fOnlClock");
  const Int_t*&      fFit_Sector                              = iter("fFit.Sector");
#ifdef __REFIT__
  const Int_t*&     fFit_N                                   = iter("fFit.N");
  const Int_t*&     fFit_Bundle                              = (Int_t **) iter("fFit.Bundle[42]");
  const Int_t*&     fFit_Mirror                              = (Int_t **) iter("fFit.Mirror[42]");
  const Double32_t*& fFit_offset                              = iter("fFit.offset");
  const Double32_t*& fFit_doffset                             = iter("fFit.doffset");
  const Double32_t*& fFit_dslope                              = iter("fFit.dslope");
  const Double32_t*& fFit_chisq                               = iter("fFit.chisq");
  const Double32_t**&  fFit_X                                   = (Double_t **) iter("fFit.X[42]");
  const Double32_t**&  fFit_Y                                   = (Double_t **) iter("fFit.Y[42]");
#endif
  const Double32_t*& fFit_slope                               = iter("fFit.slope");
  const Double32_t*& fFit_Prob                                = iter("fFit.Prob");
  const Int_t*&      fFit_ndf                                 = iter("fFit.ndf");
  //  const Int_t*&      fFit_Flag                                = (Int_t **) iter("fFit.Flag[42]");
  TFile *fOut = new TFile(Out,"recreate");
  runNT = new TNtuple("RunNT","Run date time",vRun);
  static const Double_t EastWRTWestDiff = 0;//3.55700e-01; // +/- 1.77572e-01   3.38530e-01; // +/- 1.39566e-01 permill

  Double_t OnlFreq = 0;
  Int_t oldRun = -1;
  Double_t oldDate = -1;
  TDatime t0(2007,1,1,0,0,0);
  UInt_t ut0 = t0.Convert();
  while (iter.Next()) {
#if 0
    Int_t iok = -1;
    for (Int_t i = 0; i < NZFR; i++) {if (ZFieldRuns[i] == fEvtHdr_fRun) {iok = i; break;}}
    if (iok >= 0) continue;
#endif
    if (fEvtHdr_fRun != oldRun) {
      OnlFreq = fEvtHdr_fOnlClock;
      fOut->cd();
      TDatime t(fEvtHdr_fDate,fEvtHdr_fTime);
      UInt_t ut = t.Convert();
      Double_t Date = 1. + (ut - ut0)/(24.*60.*60.);
#ifdef INTEGRATE_OVER_HOURS
      if (Date - oldDate > 0.125) { // 3/24 = 1/8 
#endif
	if (oldRun != -1) Fit();
	cout << "New run " << fEvtHdr_fRun << " Date Old/New " << oldDate << "/" << Date << endl;
	Run.run  = fEvtHdr_fRun%1000000;
	Run.date = fEvtHdr_fDate%1000000;
	Run.time = fEvtHdr_fTime;
	Run.vWest = fEvtHdr_fDriVelWest;
	Run.vEast = fEvtHdr_fDriVelEast;
	Run.events = 0;
#ifdef ADJUSTABLE_BINNING
	dv = new TH2D(Form("DV%i",fEvtHdr_fRun%1000000),Form("Drift Velocity for run %i",fEvtHdr_fRun%1000000),12,1,25,400,1,-1);
	dv->SetBuffer(100000);
#else
	dv = new TH2D(Form("DV%i",fEvtHdr_fRun%1000000),Form("Drift Velocity for run %i",fEvtHdr_fRun%1000000),12,1,25,2000,5.3,5.9);
#endif
	dv->SetXTitle("Sector");
	dv->SetYTitle("Drift Velocity ");
#ifdef ADJUSTABLE_BINNING
	slope = new TH2D(Form("SL%i",fEvtHdr_fRun%1000000),Form("Slope for run %i",fEvtHdr_fRun%1000000),12,1,25,400,1,-1);
	slope->SetBuffer(100000);
#else
	slope = new TH2D(Form("SL%i",fEvtHdr_fRun%1000000),Form("Slope for run %i",fEvtHdr_fRun%1000000),12,1,25,2000,-10.,10.);
#endif
	slope->SetXTitle("Sector");
	slope->SetYTitle("Difference wrt reference Drift Velocity in pemill");
	memAdc = new TH2D(Form("memAdc%i", fEvtHdr_fRun%1000000),
			  Form("Log(Adc) @ Membrane for West and East, Inner and Outer sectors for run %i",fEvtHdr_fRun%1000000),
			  4, 0.5, 4.5, 100, 0, 10);
	for (Int_t io = 0; io < 2; io++) {// 0 => Inner, 1 => Outer
	  TString name("Z");
	  TString title("Z[cm] of Membrane");
	  if (io == 0)  {name += "I"; title += " Inner";}
	  else          {name += "O"; title += " Outer";}
	  name += Form("%i",fEvtHdr_fRun%1000000);
	  title += Form(" for run %i",fEvtHdr_fRun%1000000); 
	  //	  dMembraneY[io] = new TH2D(name,title,24,0.5,24.5,2000,-10.,10.);
	  zMembrane[io] = new TH2D(name,title,24,0.5,24.5,2000,200,210);
	  for (Int_t we = 0; we < 2; we++) {
	    for (Int_t xy = 0; xy < 2; xy++) {
	      name  = "R"; name += Form("%i",fEvtHdr_fRun%1000000); 
	      title = "Drift length for Membrane clusters versus";
	      if (xy == 0) {name += "X"; title += " X. ";}
	      else         {name += "Y"; title += " Y. ";}
	      if (io == 0)  {name += "I"; title += " Inner";} 
	      else          {name += "O"; title += " Outer";}
	      if (we == 0)  {name += "W"; title += " West";}
	      else          {name += "E"; title += " East";}
	      dMembraneY[io][we][xy] = new TH2D(name,title,100,-200,200,1000,-10,10);
	    }
	  }
	}
	oldRun = (Int_t) fEvtHdr_fRun;
	oldDate = Date;
#ifdef INTEGRATE_OVER_HOURS
      }
#endif
    }
    for (Int_t i = 0; i <  fNhit; i++) {
      Int_t io = 0; if (fHits_row[i]    > 13) io = 1;
      zMembrane[io]->Fill(fHits_sector[i],fHits_xyzL_mX3[i]);
      Int_t we = 0; if (fHits_sector[i] > 12) we = 1;
      dMembraneY[io][we][0]->Fill(fHits_xyz_mX1[i],fHits_xyzTpcL_mX3[i]);
      dMembraneY[io][we][1]->Fill(fHits_xyz_mX2[i],fHits_xyzTpcL_mX3[i]);
      if (TMath::Abs(TMath::Abs(fHits_xyzTpcL_mX3[i])-205) < 25 && Adc[i] > 0) memAdc->Fill(2.*we + io + 1., TMath::Log(Adc[i]));
    }
    Double_t dt =  fEvtHdr_fDate%1000000 + ((Double_t) fEvtHdr_fTime)*1e-6;
    Double_t DT =  Run.date + Run.time*1e-6;
    if (dt < DT) {
      Run.date = fEvtHdr_fDate%1000000;
      Run.time = fEvtHdr_fTime;
    }
    for (Int_t k = 0; k < 12; k++) {
#ifndef __REFIT__
      if (fFit_ndf[k] > 2 && fFit_Prob[k] > 1.e-2) {
	Double_t Slope = 1e3*fFit_slope[k];
	if (fFit_Sector[k] > 12) Slope -= EastWRTWestDiff;
	slope->Fill(fFit_Sector[k], Slope);
	Double_t Vm = OnlFreq/fEvtHdr_fClock*fEvtHdr_fDriVel;
	Double_t V = 1e-6*Vm/(1.+1e-3*Slope-Vm/TMath::Ccgs());
	dv->Fill(fFit_Sector[k],V);
      }
#else
      Int_t N = fFit_N[k];
      if (N > 2) {
	Double_t Flag[42];
	memset (Flag, 0, 42*sizeof(Double_t));
	Double_t YA = 0;
	for (Int_t i = 0; i < N; i++) {
	  YA += fFit_Y[42*k+i];
	}
	YA /= N;
	for (Int_t i = 0; i < N; i++) {if (TMath::Abs(fFit_Y[42*k+i] - YA) > 10) Flag[i] = 1;}
	for (;;) {
	  TRVector Y;
	  TRMatrix A(0,2);
	  for (Int_t i = 0; i < N; i++) {
	    if (! fFit_Flag[42*k+i]) {
	      Double_t dev = fFit_Y[42*k+i]/sigma;
	      Y.AddRow(&dev);
	      Double_t a[2] = {1./sigma, fFit_X[42*k+i]/sigma};
	      A.AddRow(a);
	    }
	  }
	  Int_t ndf = A.GetNrows() - 2;
	  if (ndf < 1) break;
	  TRSymMatrix S(A,TRArray::kATxA);        if (_debug) cout << "S: " << S << endl;
	  TRVector    B(A,TRArray::kATxB,Y);      if (_debug) cout << "B: " << B << endl;
	  TRSymMatrix SInv(S,TRArray::kInverted); if (_debug) cout << "SInv: " << SInv << endl;
	  TRVector    X(SInv,TRArray::kSxA,B);    if (_debug) cout << "X: " << X << endl;
	  TRVector    R(Y);               
	  R -= TRVector(A,TRArray::kAxB,X);       if (_debug) cout << "R: " << R << endl;
	  Double_t chisq = R*R;
	  Double_t prob = TMath::Prob(chisq,ndf);
	  if (prob > 0.01) {
	    Double_t offset  = X[0];
	    Double_t slope   = X[1];
	    Double_t chisq   = chisq;
	    Double_t dslope  = SInv[2];
	    Double_t doffset = SInv[0];
	    if (ndf > 4 && prob > 1.e-2) {
	      Double_t Slope = 1e3*slope;
	      if (fFit_Sector[k] > 12) Slope -= EastWRTWestDiff;
	      slope->Fill(fFit_Sector[k], Slope);
	      Double_t Vm = OnlFreq/fEvtHdr_fClock*fEvtHdr_fDriVel;
	      Double_t V = 1e-6*Vm/(1.+1e-3*Slope-Vm/TMath::Ccgs());
	      dv->Fill(fFit_Sector[k],V);
	    }
	  }
	  break;
	} else {
	  Int_t j = -1;
	  Int_t imax = -1;
	  Double_t Rmax = 0;
	  for (Int_t i = 0; i < N; i++) {
	    if (! fit->Flag[i]) {
	      j++;
	      Double_t RR = TMath::Abs(R[j]);
	      if (RR > Rmax) {
		imax = i;
		Rmax = RR;
	      }
	    }
	  }
	  if (imax < 0) break;
	  Flag[imax] = 1;
	}
      }
#endif
    }
  }  
  Fit();
  //  runNT->Write();
  fOut->Write();
}
//________________________________________________________________________________
/*
  A.Lebedev 02/11/08
  Delays for laser rafts.
  T-zero is a moment, when a laser arrived to TPC wheel surface. 
  All other numbers corresponded to time for laser light to propagate to particular raft (TPC sector). 
  Estimated error in table   0.1ns
  West:     sector             2             4         6        8             12
  Time(ns)                 10.33          3.34      6.14    13.11          17.31
  East:     sector            14            16        18       20      22     24
  Time(ns)                 19.88         12.95      5.97     3.18   10.17  17.14
  
*/  
//________________________________________________________________________________
Double_t OffSets(Double_t *x/*, Double_t *par = 0 */) {
  // TF1 *f = new TF1("Off",OffSets,0,24,0)
  //                                  2      4      6      8    10      12
  static Double_t Toffsets[12] = {10.33,  3.34,  6.14, 13.11,   -1., 17.31, // ns
				  19.88, 12.95,  5.97,  3.18, 10.17, 17.14};
  static Double_t DV = 55.42840e-4; // cm/ns
  Int_t sector2 = (Int_t) ((x[0]-1)/2.);
  Double_t off = -1;
  if (sector2 < 0 || sector2 > 12) return off;
  return DV*Toffsets[sector2];
}
/*
  RunNT->Draw("1e6*dvAll/vWest-1:zWO-zEO>>zO(10,7,7.5)","ok<2&&zEO<0","prof")


c1 = new TCanvas()
TH1* frame = c1->DrawFrame(575e6,5.4,581.e6,5.6)
frame->SetTitle("Drift velocitry")
frame->GetXaxis()->SetTimeDisplay(1)
c1->Update()
TLegend *l = new TLegend(0.2,0.2,0.4,0.4)
RunNT->Draw("dvWest:utime-788936400>>west","ok==0","same")
l->AddEntry(west,"West")
l->Draw()
RunNT->SetMarkerColor(2)
RunNT->Draw("dvEast:utime-788936400>>east","ok==0","same")
l->AddEntry(east,"East")
RunNT->SetMarkerColor(3)
RunNT->Draw("dvAll:utime-788936400>>all","ok==1","same")
l->AddEntry(all,"All")
RunNT->SetMarkerColor(4)
RunNT->Draw("dvSet:utime-788936400>>memb","ok==2","same")
l->AddEntry(memb,"Membrane")






 */
