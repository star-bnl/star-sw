// $Id: LoopOverLaserTrees.C,v 1.3 2007/12/10 19:54:02 fisyak Exp $
// $Log: LoopOverLaserTrees.C,v $
// Revision 1.3  2007/12/10 19:54:02  fisyak
// Add Id and Log, correct spelling error in README
//
#define ADJUSTABLE_BINNING
#define INTEGRATE_OVER_HOURS
#define SeparateWestandEast
#if !defined(__CINT__) || defined(__MAKECINT__)
// $Id: LoopOverLaserTrees.C,v 1.3 2007/12/10 19:54:02 fisyak Exp $
// $Log: LoopOverLaserTrees.C,v $
// Revision 1.3  2007/12/10 19:54:02  fisyak
// Add Id and Log, correct spelling error in README
//
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
TH2D *dv = 0;
TH2D *slope = 0;
TNtuple *runNT = 0;
struct Run_t {
  Float_t run, date, time, events, day, dvAll, ddvAll, dvWest, ddvWest, dvEast, ddvEast, slAll, dslAll, slWest, dslWest, slEast, dslEast;
};
const Char_t *vRun = "run:date:time:events:day:dvAll:ddvAll:dvWest:ddvWest:dvEast:ddvEast:slAll:dslAll:slWest:dslWest:slEast:dslEast";
Run_t Run;
//________________________________________________________________________________
Double_t ScaleE2W(Double_t day) {// scale East to West drift velocity
  //  RunNT->Draw("1e3*(dvEast/dvWest-1):day>>diffE(30,90,180)","(ddvWest>0&&ddvWest<4e-5&&ddvEast>0&&ddvEast<4e-5)/((ddvWest/dvWest)**2+(ddvEast/dvEast)**2)","profw")
  static Double_t par[2] = {1.57361e-01,-4.59752e-03};
  return par[0] + par[1]*day;
}
//________________________________________________________________________________
void MakeTable() {
#ifndef SeparateWestandEast
  if (! (DVAll[0][0] > 5.5 && DVAll[0][0] < 5.9 && dDVAll[0][0] > 0 && dDVAll[0][0]< 1e-4)) {
    cout << "Run " << run << " fails =============================" << endl;
    return;
  }
#else
  if (! (DVAll[0][1] > 5.5 && DVAll[0][1] < 5.9 && dDVAll[0][1] > 0 && dDVAll[0][1]< 4e-5 ||
	 DVAll[0][2] > 5.5 && DVAll[0][2] < 5.9 && dDVAll[0][2] > 0 && dDVAll[0][2]< 4e-5)) {
    cout << "Run " << run << " fails =============================" << endl;
    return;
  }
#endif
  TString fOut =  Form("tpcDriftVelocity.%8i.%06i.C",date,Time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcDriftVelocity\")) return 0;" << endl;
  out << "  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity(\"tpcDriftVelocity\",1);" << endl;
  out << "  tpcDriftVelocity_st row;// Laser Run " << run << endl;
  Double_t dvEast  =  DVAll[0][2]; 
  Double_t ddvEast = dDVAll[0][2]; 
  Double_t dvWest  =  DVAll[0][1];
  Double_t ddvWest = dDVAll[0][1];
#ifdef SeparateWestandEast
  if (! (dvWest > 5.5 && dvWest < 5.9 && ddvWest > 0 && ddvWest< 4e-5) ) {// West From East
    ddvWest = -1;
    dvWest = dvEast/(1 + 1e-3*ScaleE2W(Run.day));
  } 
  if (! (dvEast > 5.5 && dvEast < 5.9 && ddvEast > 0 && ddvEast< 4e-5) ) {// East from West
    ddvEast = -1;
    dvEast = dvWest*(1 + 1e-3*ScaleE2W(Run.day));
  } 
  out << "  row.laserDriftVelocityEast	 =   " << dvEast << "; // +/- " << ddvEast 
      << " cm/us East: Slope = " << DVAll[1][2] << " +/- " << dDVAll[1][2] << " DV = " << DVAll[0][2] << " +/- " << dDVAll[0][2]<< endl;
  out << "  row.laserDriftVelocityWest	 =   " << dvWest << "; // +/- " << ddvWest 
      << " cm/us West: Slope = " << DVAll[1][1] << " +/- " << dDVAll[1][1] << " DV = " << DVAll[0][1] << " +/- " << dDVAll[0][1]<< endl;
  out << "  row.cathodeDriftVelocityEast	 =          0; // cm/us : from cathode emission  ;" << endl;
  out << "  row.cathodeDriftVelocityWest	 =          0; // cm/us : from cathode emission  ;" << endl;
  out << "  tableSet->AddAt(&row); " << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
#else
  out << "  row.laserDriftVelocityEast	 =   " << DVAll[0][0] << "; // +/- " << dDVAll[0][0] 
      << " cm/us All: East = " << DVAll[1][2] << " +/- " << dDVAll[1][2] << endl;
  out << "  row.laserDriftVelocityWest	 =   " << DVAll[0][0] << "; // +/- " << dDVAll[0][0] 
      << " cm/us All: West = " << DVAll[1][1] << " +/- " << dDVAll[1][1] << endl;
  out << "  row.cathodeDriftVelocityEast	 =          0; // cm/us : from cathode emission  ;" << endl;
  out << "  row.cathodeDriftVelocityWest	 =          0; // cm/us : from cathode emission  ;" << endl;
  out << "  tableSet->AddAt(&row);// 1e3*Delta: All = " << DVAll[0][0] << " +/- " << dDVAll[0][0] << endl;
  out << "  return (TDataSet *)tableSet;//" 
      << " West = " << DVAll[0][1] << " +/- " << dDVAll[0][1]
      << " East = " << DVAll[0][2] << " +/- " << dDVAll[0][2] << endl;
#endif
  out << "};" << endl;
}
//________________________________________________________________________________
void Fit() {
  static TDatime t0(2007,1,1,0,0,0);
  static UInt_t u0 = t0.Convert();
  date = 20000000 + (Int_t) Run.date;
  Time = (Int_t) Run.time;
  TDatime t(date, Time);
  UInt_t u = t.Convert();
  Run.day = 1. + (u - u0)/(24.*60.*60.);
  run  = (Int_t) (1000000*((Int_t) (Run.date/100000)) + Run.run);
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
	  fit->Fit(pol0,"er+","",xmin,14);
	}
	if (k == 2) {
	  if (xmax <= 15) continue;
	  fit->Fit(pol0,"er+","",14,xmax);
	}
	DVAll[l][k]  = pol0->GetParameter(0);
	dDVAll[l][k] = pol0->GetParError(0);
	par[2*(k+3*l)] = DVAll[l][k];
	par[2*(k+3*l)+1] = dDVAll[l][k];
      }
    }
    fit->Write();
  }
  runNT->Fill(&Run.run);
  MakeTable();
}
//________________________________________________________________________________
void LoopOverLaserTrees(const Char_t *files="./2007B/st_laser_*.tags.root") {
  TDirIter Dir(files);
  TTreeIter iter("laser");
  iter.AddFile(files);
  //  const Int_t&       fEvtHdr_fEvtNum                          = iter("fEvtHdr.fEvtNum");
  const Int_t&       fEvtHdr_fRun                             = iter("fEvtHdr.fRun");
  const Int_t&       fEvtHdr_fDate                            = iter("fEvtHdr.fDate");
  const Int_t&       fEvtHdr_fTime                            = iter("fEvtHdr.fTime");
  //  const Float_t&     fEvtHdr_ftZero                           = iter("fEvtHdr.ftZero");
  const Float_t&     fEvtHdr_fDriVel                          = iter("fEvtHdr.fDriVel");
  const Float_t&     fEvtHdr_fClock                           = iter("fEvtHdr.fClock");
  //  const Float_t&     fEvtHdr_fTrigger                         = iter("fEvtHdr.fTrigger");
  //  const Float_t&     fEvtHdr_fDriftDistance                   = iter("fEvtHdr.fDriftDistance");
  //  const Float_t&     fEvtHdr_fInnerSectorzOffset              = iter("fEvtHdr.fInnerSectorzOffset");
  //  const Float_t&     fEvtHdr_fOuterSectorzOffset              = iter("fEvtHdr.fOuterSectorzOffset");
  //  const Float_t&     fEvtHdr_ftriggerTimeOffset               = iter("fEvtHdr.ftriggerTimeOffset");
  const Float_t&     fEvtHdr_fOnlClock                        = iter("fEvtHdr.fOnlClock");
  //  const Int_t*&      fTracks_Flag                             = iter("fTracks.Flag");
  //  const UShort_t*&   fHits_sector                             = iter("fHits.sector");
  const Int_t*&      fFit_Sector                              = iter("fFit.Sector");
  //  const Double32_t*& fFit_offset                              = iter("fFit.offset");
  const Double32_t*& fFit_slope                               = iter("fFit.slope");
//   const Double32_t*& fFit_doffset                             = iter("fFit.doffset");
//   const Double32_t*& fFit_dslope                              = iter("fFit.dslope");
//   const Double32_t*& fFit_chisq                               = iter("fFit.chisq");
  const Double32_t*& fFit_Prob                                = iter("fFit.Prob");
  const Int_t*&      fFit_ndf                                 = iter("fFit.ndf");
  TFile *fOut = new TFile("LaserPlots.root","recreate");
  runNT = new TNtuple("RunNT","Run date time",vRun);
  /* Wrong frequency 
     mysql RunLog -h onldb.starp.bnl.gov -P 3501 -e 'select beginTime,runNumber,entryTag,frequency from clockFrequency  where entryTag = 0 and runNumber in 
     (select runNumber  from runDescriptor where glbSetupName = "laser_localclock") and abs(frequency-9.21588993)>0.0001;'
     +---------------------+-----------+----------+------------+
     | beginTime           | runNumber | entryTag | frequency  |
     +---------------------+-----------+----------+------------+
     | 2007-04-04 11:44:07 |   8094011 |        0 | 9.38315010 |
     | 2007-04-04 12:59:13 |   8094013 |        0 | 9.38315010 |
     | 2007-05-04 18:56:46 |   8124070 |        0 | 9.38314915 |
     | 2007-05-10 15:38:06 |   8130015 |        0 | 9.38314915 |
     | 2007-05-17 14:48:42 |   8137035 |        0 | 9.38314915 |
     | 2007-05-18 13:51:40 |   8138043 |        0 | 9.38314915 |
     | 2007-05-24 12:57:58 |   8144002 |        0 | 9.38314915 |
     | 2007-06-06 18:53:38 |   8157053 |        0 | 9.38314915 |
     | 2007-06-12 17:54:07 |   8163044 |        0 | 9.38314915 |
     | 2007-06-12 17:57:49 |   8163045 |        0 | 9.38314915 |
     | 2007-06-16 00:26:28 |   8166085 |        0 | 9.38314915 |
     | 2007-06-21 05:37:45 |   8172017 |        0 | 9.38314915 |
     +---------------------+-----------+----------+------------+
zero Field runs
8086022
8086061
8087063
8101057
8101058
8157053
big Laser Runs
+-----------+----------------+
| runNumber | numberOfEvents |
+-----------+----------------+
|   8101058 |          29670 |
|   8102110 |          29975 |+
|   8144002 |           9285 |
|   8163045 |          29959 |
+-----------+----------------+

  */
  static const Double_t EastWRTWestDiff = 0;//3.55700e-01; // +/- 1.77572e-01   3.38530e-01; // +/- 1.39566e-01 permill
  static const Int_t NBad = 12;
  static const Int_t BadRuns[NBad] = {
    8094011,   8094013,   8124070,   8130015,   8137035,   8138043,
    8144002,   8157053,   8163044,   8163045,   8166085,   8172017  
  };
  //  static const Int_t NZFR = 6;
  //  static const Int_t ZFieldRuns[NZFR] = {8086022,8086061,8087063,8101057,8101058,8157053};

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
      for (Int_t i = 0; i < NBad; i++) {if (fEvtHdr_fRun == BadRuns[i]) {OnlFreq = 9.21588898; break;}}
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
	Run.date = fEvtHdr_fDate%100000;
	Run.time = fEvtHdr_fTime;
	Run.events = 0;
#ifdef ADJUSTABLE_BINNING
	dv = new TH2D(Form("DV%i",fEvtHdr_fRun%1000000),Form("Drift Velocity for run %i",fEvtHdr_fRun%1000000),12,1,25,400,1,-1);
	dv->SetBuffer(100000);
#else
	dv = new TH2D(Form("DV%i",fEvtHdr_fRun%1000000),Form("Drift Velocity for run %i",fEvtHdr_fRun%1000000),12,1,25,2000,5.35,5.85);
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
	oldRun = (Int_t) fEvtHdr_fRun;
	oldDate = Date;
#ifdef INTEGRATE_OVER_HOURS
      }
#endif
    }
    Double_t dt =  fEvtHdr_fDate%100000 + ((Double_t) fEvtHdr_fTime)*1e-6;
    Double_t DT =  Run.date + Run.time*1e-6;
    if (dt < DT) {
      Run.date = fEvtHdr_fDate%100000;
      Run.time = fEvtHdr_fTime;
    }
    for (Int_t k = 0; k < 12; k++) {
      if (fFit_ndf[k] > 4 && fFit_Prob[k] > 1.e-2) {
	Double_t Slope = 1e3*fFit_slope[k];
	if (fFit_Sector[k] > 12) Slope -= EastWRTWestDiff;
	slope->Fill(fFit_Sector[k], Slope);
	Double_t Vm = OnlFreq/fEvtHdr_fClock*fEvtHdr_fDriVel;
	Double_t V = 1e-6*Vm/(1.+1e-3*Slope-Vm/TMath::Ccgs());
	  dv->Fill(fFit_Sector[k],V);
      }
    }    
  }  
  Fit();
  runNT->Write();
  delete fOut;
}
  
