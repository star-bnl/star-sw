/* 
Run XIII
   root.exe -q -b lDb.C st_laser_adc_14044071_raw.ZF.Fit.g3.LandauIFreQ.SecAll.root  MembraneClusters.C+ | tee ZF26.log
   root.exe -q -b lDb.C st_laser_adc_14046081_raw.F.Fit.g3.LandauIFreQ.SecAll.root   MembraneClusters.C+ | tee FF26.log
   root.exe -q -b lDb.C st_laser_adc_14072106_raw.RF.Fit.g3.LandauIFreQ.SecAll.root  MembraneClusters.C+ | tee RF26.log
   root.exe -q -b lDb.C st_laser_adc_14158028.raw.RF.Fit.g3.LandauIFreQ.SecAllB.root MembraneClusters.C+ | tee RC26.log
   root.exe -q -b lDb.C st_laser_adc_14161023.raw.ZF.Fit.g3.LandauIFreQ.SecAllB.root MembraneClusters.C+ | tee ZC26.log
Run XIV
   root.exe -q -b lDb.C 15040047.ClnoW.Fit.g3.LangauIFreQ.All.root MembraneClusters.C+ | tee FF419.log
   root.exe -q -b lDb.C 15040049.ClnoW.Fit.g3.LangauIFreQ.All.root MembraneClusters.C+ | tee ZF419.log
   root.exe -q -b lDb.C 15050194.ClnoW.Fit.g3.LangauIFreQ.All.root MembraneClusters.C+ | tee ZR419.log
   root.exe -q -b lDb.C 15051088.ClnoW.Fit.g3.LangauIFreQ.All.root MembraneClusters.C+ | tee RF419.log 
   root.exe -q -b lDb.C 15064050.ClnoW.Fit.g3.LangauIFreQ.All.root MembraneClusters.C+ | tee ZQ419.log 
   root.exe -q -b lDb.C 15070022.ClnoW.Fit.g3.LangauIFreQ.All.root MembraneClusters.C+ | tee RQ419.log 
   root.exe TpcSurvey.C+
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
#include "TVector2.h"
#include "TVector3.h"
#include "StRoot/St_db_Maker/St_db_Maker.h"
#include "StRoot/StTpcDb/StTpcDbMaker.h"
#include "StRoot/StChain/StChain.h"
#include "StRoot/StDbUtilities/StTpcCoordinateTransform.hh"
#include "StRoot/StTpcHitMoverMaker/StTpcHitMoverMaker.h"
#include "StRoot/StDetectorDbMaker/St_tpcPadGainT0BC.h"
#include "StRoot/StDetectorDbMaker/St_starClockOnlC.h"
//#include "T0X.C"
#include "TClassTable.h"
#include "TSystem.h"
#include "TDirectory.h"
#define LaserCluster_cxx
#include "LaserCluster.h"
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#endif
//________________________________________________________________________________
TH2F *Defuser(Bool_t AccountLaserDelays = kTRUE) {
  Double_t Rdefuser = 197;
  Double_t RminIFC =       46.6;
  Double_t RadInner[13] = {    60,  64.8,  69.6,  74.4,  79.2,    84,  88.8,  93.6,  98.8,   104, 109.2, 114.4, 119.6};
  Double_t outerRowRadii[32];
  Double_t firstOuterSectorPadRow  =    127.195; // ;
  Double_t outerSectorRowPitch	   =          2;
  Double_t R[45];
  for (Int_t i = 0; i < 45; i++) {
    if (i < 13) R[i] = RadInner[i];
    else        R[i] = firstOuterSectorPadRow + outerSectorRowPitch*(i-14);
  }
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
  For diffusers West <=> East
*/  
  //             WE  D
  Int_t difSector[2][4] = {
    { 2, 4, 8, 10},
    {22,20,18, 16}
  };
  Double_t T0WE[2][4] = {
    {10.17, 3.18, 12.95, 19.88},
    {10.33, 3.34,  6.14, 13.11}
  };
  Double_t zMem = 210;
  Int_t nphi = 12; // *5
  TString Title("Distance from Membrane to defuser [cm]");
  TString Name("Defuse");
  if (AccountLaserDelays) {
    Title += " with accounting delays in rafts";
    Name  += "D";
  }
  TH2F *Defuse = new TH2F(Name,Title,nphi*2,0.5,24.5,45,0.5,45.5);
  Defuse->SetXTitle("sector");
  Defuse->SetYTitle("row");
  Defuse->SetStats(0);
  Int_t iphi, iphiD;
  for (Int_t sector = 1; sector <= 24; sector++) {
    if (sector <= 12) {iphi = (360 + 90 - 30* sector      )%360;}
    else              {iphi = (      90 + 30*(sector - 12))%360;}
    Int_t we = 0;
    if (sector > 12) we = 1;
    Double_t Phi = TMath::DegToRad()*iphi;
    for (Int_t row = 1; row <= 45; row++) {
      Double_t r = R[row-1];
      Double_t y2 = r*TMath::Sin(Phi);
      Double_t x2 = r*TMath::Cos(Phi);
      TVector3 R2(x2, y2, 0);
      Double_t distMin = 100000;
      Int_t    secM = -1;
      for (Int_t dif = 0; dif < 4; dif++) {
	//      for (Int_t dif = 1; dif <= 1; dif++) {
	Int_t difSec = difSector[we][dif];
	if (difSec <= 12) {iphiD = (360 + 90 - 30* difSec      )%360;}
	else              {iphiD = (      90 + 30*(difSec - 12))%360;}
 	Double_t PhiDif = TMath::DegToRad()*iphiD;
	Double_t y1 = Rdefuser*TMath::Sin(PhiDif);
	Double_t x1 = Rdefuser*TMath::Cos(PhiDif);	  
	TVector3 R1(x1,y1,zMem);
	TVector3 dR = R2 - R1;
	Double_t distance = dR.Mag();
	if (AccountLaserDelays) distance += TMath::Ccgs()*1e-9*T0WE[we][dif];
	Double_t r2 = dR.XYvector().Mod2();
	TVector2 n = dR.XYvector().Unit();
	Double_t s = - (R1.XYvector()*n);
	TVector2 R = R1.XYvector() + n*s;
	if (R.Mod() <= RminIFC) {
	  TVector2 dRM = R2.XYvector() - R;
	  Double_t x1n = R1.XYvector()*n;
	  Double_t s1 = - x1n - TMath::Sqrt(x1n*x1n + Rdefuser*Rdefuser - R1.XYvector().Mod2());
	  if (s1 > s) continue;
	}
#if 0
	cout << Form("%2i => %2i",sector,difSec)
	     << Form(" xyS %8.3f %8.3f",x2,y2)  << Form("\tdif %8.3f %8.3f",x1,y1)
	     << Form(" distance %8.3f s %8.3f",distance,s) << endl;
#endif
	if (distance < distMin) {
	  distMin = distance; 
	  secM = difSec;
	}
      }
      if(secM < 0) continue;  
      Defuse->SetBinContent(sector,row,distMin);
#if 0
      cout << Form("%2i => %2i distMin = %8.3f",sector,secM,distMin) << endl;
#endif
    }
  }
#if 0
  Defuse->SetMinimum(zMem);
  Defuse->Draw("colz");
#else
  return Defuse;
#endif
}
//________________________________________________________________________________
void LaserCluster::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L LaserCluster.C
//      Root > LaserCluster t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
   if (fChain == 0) return;

   Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
   }
}
//________________________________________________________________________________
void MembraneClusters() {
  TDirectory *dir = gDirectory;
  St_db_Maker *dbMk = 0;
  StTpcDbMaker *tpcDbMk = 0;
  StChain *chain;
  chain = (StChain *) StMaker::GetChain();
  dbMk = (St_db_Maker *) chain->Maker("db");
  tpcDbMk = (StTpcDbMaker *) chain->Maker("tpcDB");
  dbMk->SetDebug();
  TString RunName(gDirectory->GetName());
  Int_t date = 20130301;
  Int_t time =        0;
  TString Set;
  // Run 13
  if (RunName.Contains("14044071")) {date = 20130214; time =  30457; Set = "ZF";}
  if (RunName.Contains("14046081")) {date = 20130215; time = 193740; Set = "FF";}
  if (RunName.Contains("14072106")) {date = 20130313; time = 152600; Set = "RF";}
  if (RunName.Contains("14158028")) {date = 20130607; time = 135535; Set = "RC";}
  if (RunName.Contains("14161023")) {date = 20130610; time = 125720; Set = "ZC";}
  // Run 14
  if (RunName.Contains("15040047")) {date = 20140209; time = 233539; Set = "FF"; /* 20k <====== */}
  if (RunName.Contains("15040049")) {date = 20140210; time =  11002; Set = "ZF"; /* 20k <====== ZF after FF */}
  if (RunName.Contains("15050194")) {date = 20140219; time = 172943; Set = "ZR"; /* 14k <====== ZF after RF */}
  if (RunName.Contains("15051088")) {date = 20140220; time = 155822; Set = "RF"; /* 20k <========= */}
  if (RunName.Contains("15064050")) {date = 20140305; time = 193150; Set = "ZQ"; /* 20k <========= */}
  if (RunName.Contains("15070022")) {date = 20140311; time = 120746; Set = "RQ"; /* 20k <========= */}
  cout << "Run " << RunName.Data() << " Set date = " << date << " time = " << time << "\t" << Set.Data() << endl;
  dbMk->SetDateTime(date,time);
  tpcDbMk->SetDebug();
  chain->Init();
  St_trgTimeOffsetC::instance()->SetLaser(kTRUE);
  chain->Make();
  StTpcCoordinateTransform tran(gStTpcDb);
  dir->cd();
  TTree *fitP = (TTree *) gDirectory->Get("FitP");
  if (! fitP) {cout << "FitP TTree has not been found" << endl; return;}
  LaserCluster t(fitP);
  Long64_t nentries = t.fChain->GetEntriesFast();
  Long64_t nbytes = 0, nb = 0;
  static StTpcLocalCoordinate locP, locP1;
  static StTpcLocalCoordinate locPD, locPD1;
  TString FouT("Membrane.");
  FouT += gSystem->BaseName(gDirectory->GetName());
  FouT.ReplaceAll(".root","");
  FouT += ".h";
  ofstream out;
  out.open(FouT.Data(), ios::out);
  TH2F *defuse = Defuser(kTRUE);
  Double_t frequency = St_starClockOnlC::instance()->Frequency();
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = t.LoadTree(jentry);
    if (ientry < 0) break;
    nb = t.fChain->GetEntry(jentry);   nbytes += nb;
//     if (t.dmuX < 1e-5 || t.dmuX > 0.02) continue;
//     if (t.dmuY < 1e-5 || t.dmuY > 0.02) continue;
//     if (t.muX < 365) continue;
    // if (Cut(ientry) < 0) continue;
    //    t.NormLX>1t.NormLY>1&&t.chisqX/t.NDFX<2000&&t.chisqY/t.NDFY<2000&&t.dmvpX<0.04&&t.dmuY<0.04
    if (t.mvpX < 365) continue; // Membrane Only
    if (t.NormLX <= 1) continue;
    if (t.NormLY <= 1) continue;
    if (t.NDFX < 1 || t.NDFY < 1) continue;
    if (t.chisqX/t.NDFX > 2000) continue; 
    if (t.chisqY/t.NDFY > 2000) continue;
    if (t.dmvpX        >  0.10) continue;
    if (t.dmuY         >  0.10) continue;
    if (t.dmuY         <  1e-4) continue;

    cout << jentry << "\tmvpX = " << t.mvpX << " +/- " << t.dmvpX << "\tmuY = " << t.muY << " +/- " << t.dmuY << endl;
    //    StTpcPadCoordinate padP(t.sector%100,t.row,t.muY,t.mvpX+T0X(t.sector%100,t.row));   cout << "padP\t" << padP << endl;
    Int_t sector = ((Int_t )t.sector)%100;
    Double_t time = t.mvpX;// + St_tpcPadGainT0BC::instance()->T0(sector,t.row,t.muY); //;T0X(sector,t.row);
    //    Double_t time = t.mvpX - St_tpcPadGainT0BC::instance()->T0(sector,t.row,t.muY); //;T0X(sector,t.row);
#if 0
    // Bias due to time
    Double_t Rrow = tran.yFromRow(t.row);
    Double_t L = 210; // cm position of diffuser
    Double_t Rraft = 197;
    Double_t dR = Rraft - Rrow;
    Double_t length = L + TMath::Sqrt(L*L + dR*dR);
#else
    Double_t length = defuse->GetBinContent(sector,t.row);
#endif
    Double_t dT = length/TMath::Ccgs()*frequency;
    // difusers installed West: sectors 2, 4, 8, 10; East: sector 4, 6, 8, 10 => 16, 18, 20, 22 (A.Lebedev 01/04/14)
    time -= dT; // time of flight 
    StTpcPadCoordinate padP(sector,t.row,t.muY,time);   cout << "dT = " << dT << "\t" << padP << "\t" << padP << endl;
    tran(padP,locP, kTRUE, kTRUE);                                
    StTpcPadCoordinate padP1(sector,t.row,t.muY+t.dmuY,time+t.dmvpX);   cout << "padP1\t" << padP1 << endl;
    tran(padP1,locP1, kTRUE, kTRUE);                                
#if 1
    //    Double_t scale = 1. - 0.3e-5*locP.position().y(); cout << "locP\t" << locP << " scale = " << scale << endl;
    // 9.    Double_t scale = 1. - 2.25e-6*locP.position().y(); cout << "locP\t" << locP << " scale = " << scale << endl;
    // 10. Double_t scale = 1. - 1e-6*locP.position().y(); cout << "locP\t" << locP << " scale = " << scale << endl;
    //   Double_t scale = 1.;/* - 1.5e-6*locP.position().y();*/ cout << "locP\t" << locP << " scale = " << scale << endl;
    //    Double_t scale = 1 + St_tpcEffectiveGeom::instance()->scale()*locP.position().y(); cout << "locP\t" << locP << " scale = " << scale << endl;
    //    StTpcDb::instance()->ScaleDriftVelocity(scale);
    //    tran(padP,locP, kTRUE, kTRUE);                 cout << "locP\t" << locP << endl;
    StTpcHitMover::moveTpcHit(locP,locPD);               cout << "locPD\t" << locPD << endl;
    StTpcHitMover::moveTpcHit(locP1,locPD1);             cout << "locPD1\t" << locPD1 << endl;
    //    StTpcDb::instance()->ScaleDriftVelocity(1./scale);
#else
    locPD = locP; // no distorsion
    locPD1 = locP1; // no distorsion
#endif
    TString name("W");
    if (sector > 12) name  = "E";
    if (t.row <= 13) name += "I";
    else             name += "O";
    name += Form("%02i%02i",(Int_t) sector,(Int_t) t.row);
    name += Set;
    Double_t dx = 1e-2*TMath::Abs(locPD.position().x()-locPD1.position().x());
    Double_t dy = 1e-2*TMath::Abs(locPD.position().y()-locPD1.position().y()); 
    Double_t dz = 1e-2*TMath::Abs(locPD.position().z()-locPD1.position().z()); 
    cout << "  {\"Tpc\",\"" << name.Data() << "\","   
	 << 0.01*locPD.position().x() << "," << 0.01*locPD.position().z() << "," << 0.01*locPD.position().y()
	 << "," << dx << "," << dz << "," << dy << ",\"Membrane from Laser\"}," << endl;  
    out << "  {\"Tpc\",\"" << name.Data() << "\"," 
	 << 0.01*locPD.position().x() << "," << 0.01*locPD.position().z() << "," << 0.01*locPD.position().y()  
	 << "," << dx << "," << dz << "," << dy << ",\"Membrane from Laser\"}," << endl;   
  }
  out.close();
}
