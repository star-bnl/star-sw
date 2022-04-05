// $Id: Laser.C,v 1.5 2007/12/10 19:54:02 fisyak Exp $
// $Log: Laser.C,v $
// Revision 1.5  2007/12/10 19:54:02  fisyak
// Add Id and Log, correct spelling error in README
//
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
//________________________________________________________________________________
void MakeTable() {
  TString fOut =  Form("tpcDriftVelocity.%8i.%06i.C",date,Time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcDriftVelocity\")) return 0;" << endl;
  out << "  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity(\"tpcDriftVelocity\",1);" << endl;
  out << "  tpcDriftVelocity_st row;// Laser Run " << run << endl;
  out << "  row.laserDriftVelocityEast	 =   " << DVAll[1][0] << "; // +/- " << dDVAll[1][0] << " cm/us All: East = " << DVAll[1][2] << " +/- " << dDVAll[1][2] << endl;
  out << "  row.laserDriftVelocityWest	 =   " << DVAll[1][0] << "; // +/- " << dDVAll[1][0] << " cm/us All: West = " << DVAll[1][1] << " +/- " << dDVAll[1][1] << endl;
  out << "  row.cathodeDriftVelocityEast	 =          0; // cm/us : from cathode emission  ;" << endl;
  out << "  row.cathodeDriftVelocityWest	 =          0; // cm/us : from cathode emission  ;" << endl;
  out << "  tableSet->AddAt(&row);// 1e3*Delta: All = " << DVAll[0][0] << " +/- " << dDVAll[0][0] << endl;
  out << "  return (TDataSet *)tableSet;//" 
      << " West = " << DVAll[0][1] << " +/- " << dDVAll[0][1]
      << " East = " << DVAll[0][2] << " +/- " << dDVAll[0][2] << endl;
  out << "};" << endl;
}
//________________________________________________________________________________
void Slopes(Int_t n1 = 1, Int_t n2 = 9999) {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  cout << "Oneped " << nn << " Files" << endl;
  TFile **Files = new TFile *[nn];
  //  TString FileNames[nn]; 
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  Int_t l = 0;
  TString cut("fFit.Prob>1e-2&&fFit.ndf>4");
  while ( (f = (TFile *) next()) ) { 
    TTree *laser = (TTree *) f->Get("laser");
    if (! laser) {
      cout << "Skip file " << f->GetName() << " with no laser Tree" << endl;
      delete f; continue;
    }
    Long64_t n = laser->Draw("fFit.slope",cut,"goff");
    if (n < 100) {
      cout << "Skip file " << f->GetName() << " with " << n << " Entries" << endl;
      delete f;
    } else {
      l++;
      if (l >=  n1 && l <= n2) {
	cout << l << " Add file " << f->GetName() << " with " << n << " Entries" << endl;
	//	FileNames[NF] = f->GetName();
	Files[NF] = f;
	NF++;
      }
      //      delete f;
    }
  }
  if (!NF) return;
  Int_t ny = (Int_t) TMath::Sqrt(NF);
  Int_t nx = NF/ny;
  if (nx*ny != NF) nx++;
  cout << "NF = " << NF << " nx " << nx << " ny " << ny << endl;
  TCanvas *c[2];
  c[0] = new TCanvas(Form("Slopes %i",n1),Form("Slopes %i",n1));  
  c[1] = new TCanvas(Form("LDV %i",n1),Form("Laser Drift Velocities %i",n1));
  c[0]->Divide(nx,ny);
  c[1]->Divide(nx,ny);
  for (Int_t i = 0; i < NF; i++) {
    memset(&DVAll[0][0], 0, 6*sizeof(Double_t));
    memset(&dDVAll[0][0], 0, 6*sizeof(Double_t));
    f = Files[i];
    //    f = TFile::Open(FileNames[i]);
    if (! f) continue;
    cout << "i = " << i << " " << f->GetName() << endl;
    f->cd();
    TTree *laser = (TTree *) f->Get("laser");
    if (! laser) {
      cout << "Did not find TTree laser in " << f->GetName() << endl;
      continue;
    }
    laser->SetMarkerStyle(20);
    laser->Draw("fEvtHdr.fRun:fEvtHdr.fDate:fEvtHdr.fTime","","goff",1);
    run = (Int_t) laser->GetV1()[0];
    date = (Int_t) laser->GetV2()[0];
    Time = (Int_t) laser->GetV3()[0];
    cout << "Run = " << run << " date = " << date << " Time = " << Time << endl;
    //    Int_t color = i+1;
    //    laser->SetMarkerColor(color);
    TString histN, plot, plotW,  Plot, Plot2, profN;
    TString plotE;
    TString CutW(cut); CutW += " && fFit.Sector < 13";
    TString CutE(cut); CutE += " && fFit.Sector > 12";
    Char_t *cutName[3] = {"All ","West","East"};
    Char_t *plName[2] = {"SS", "dV"};
    Char_t *FMT[2] = {"%s dV = (%7.2f +/- %7.2f) x 10^3","%s dV = (%10.5f +/- %10.5f) cm/mksec"};
    for (Int_t l = 0; l < 2; l++) {
      c[l]->cd(i+1)->SetLogz(1);
      histN = Form("%s%i",plName[l],run);
      if (l == 0) {
	plotW  = "1e3*fFit.slope:fFit.Sector>>";
	plotE  = plotW; plotE += "+";
	//	plotE  = "1e3*(fFit.slope+3.63518e-04):fFit.Sector>>+";
      } else {
	plotW = "1e-6*fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/(1.+fFit.slope-fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/TMath::Ccgs()):fFit.Sector>>";
	plotE = plotW; plotE += "+";
	//	plotE = "1e-6*fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/(1.+fFit.slope+3.63518e-04-fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/TMath::Ccgs()):fFit.Sector>>+";
      }
      Plot  = plotW;
      profN = histN;
      profN += "_pfx";
      Plot += profN;
      Plot += "(24,1,25)";
      Long64_t n = laser->Draw(Plot,CutW,"prof");
      Plot = plotE; Plot += profN;
      n += laser->Draw(Plot,CutE,"prof");
      if (n < 10) {
	cout << f->GetName() << " empty only " << n << " entries" << endl;
	continue;
      }
      TProfile *SSp = (TProfile *) gDirectory->Get(profN);
      if (! SSp) continue;
      if (SSp->GetEntries() <= 10) {
	cout << SSp->GetName() << " empty only " << n << " entries" << endl;
	continue;
      }
      Plot2 = plotW; 
      Plot2 += histN;
      SSp->SetMarkerColor(3);
      Double_t ymin = SSp->GetMean(2) - 3*SSp->GetRMS(2);
      Double_t ymax = SSp->GetMean(2) + 3*SSp->GetRMS(2);
      Plot2 += Form("(24,1,25,100,%f,%f)",ymin,ymax);
      laser->Draw(Plot2,CutW,"colz");
      Plot2 = plotE;
      Plot2 += histN;
      laser->Draw(Plot2,CutE,"colz");
      TH2F *SS = (TH2F *) gDirectory->Get(histN);
      if (! SS) {
	cout << "Did not find histogram " << histN << endl;
	continue;
      }
      SS->FitSlicesY();
      TString fitN(histN);
      fitN += "_1";
      TH1D *fit = (TH1D *) gDirectory->Get(fitN);
      TLegend *leg = new TLegend(0.1,0.1,0.55,0.25,"");
      if (fit) {
	fit->SetMarkerStyle(20);
	TF1 *pol0 = (TF1*) gROOT->GetFunction("pol0");
	if (pol0) {
	  for (Int_t k = 0; k < 3; k++) {
	    pol0->SetLineColor(k+1);
	    if (k == 0) fit->Fit(pol0,"er","",1,25); 
	    if (k == 1) fit->Fit(pol0,"er+","",1,13);
	    if (k == 2) fit->Fit(pol0,"er+","",13,25);
	    DVAll[l][k]  = pol0->GetParameter(0);
	    dDVAll[l][k] = pol0->GetParError(0);
	    if (!l || dDVAll[l][k] > 0 && dDVAll[l][k]< 1e-3)
	      leg->AddEntry((TF1 *) fit->GetListOfFunctions()->Last(), Form(FMT[l],cutName[k],DVAll[l][k],dDVAll[l][k]));
	  }
	}
      }
      SS->SetTitle(Form("Run: %i",run));
      SS->SetXTitle("Sector");
      SS->SetYTitle("Drift Velocity ");
      SS->Draw("colz");
      if (SSp) SSp->Draw("same");
      if (fit) fit->Draw("same");
      if (l && DVAll[l][0] > 0) {
	if (DVAll[l][0] > 5.5 &&  dDVAll[l][0] > 0 && dDVAll[l][0]< 1e-3) MakeTable();
	else {
	  cout << "File " << f->GetName() << " fails =============================" << endl;
	  leg->AddEntry(fit, "Rejected");
	}
      }
      leg->Draw();
      c[l]->Update();
    }
  }
}
#if 0
//________________________________________________________________________________
void Offsets() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **Files = new TFile *[nn];
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TTree *laser = (TTree *) f->Get("laser");
    if (! laser) continue;
    cout << "Add " << f->GetName() << endl;
    Files[NF] = f; NF++;
  }
  Int_t ny = (Int_t) TMath::Sqrt(NF);
  Int_t nx = NF/ny;
  if (nx*ny != NF) nx++;
  TCanvas *c1 = new TCanvas("Offsets","Offsets");
  c1->Divide(nx,ny);
  for (Int_t i = 0; i < NF; i++) {
    c1->cd(i+1);
    f = Files[i];
    if (! f) continue;
    TTree *laser = (TTree *) f->Get("laser");
    if (! laser) continue;
    laser->SetMarkerStyle(20);
    //    Int_t color = i+1;
    //    laser->SetMarkerColor(color);
    laser->Draw(Form("abs(fFit.offset)-6.66274:fFit.Sector>>OS%i(24,1,25)",i),"fFit.doffset > 0 && fFit.doffset <5","prof");
    TProfile *OS = (TProfile *) gDirectory->Get(Form("OS%i",i));
    TString Name(gSystem->BaseName(f->GetName()));
    Name.ReplaceAll(".root","");
    Name.ReplaceAll("laser_","Run ");
    cout << Name  << " i = " << i << endl;
    OS->SetTitle(Name);
    OS->SetXTitle("Sector");
    OS->SetYTitle("dZ(cm)");
    OS->Draw();
    c1->Update();
    //    Int_t j;
    //    cin >> j;
  }
}
#endif
//________________________________________________________________________________
void dPhi() {
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser) return;
  //  laser->Draw("fTracks.dPhi:fTracks.mSector>>dPhi(12,1,25,100,-.15,0.15)","fTracks.Flag>1","colz");
  //  laser->Draw("TMath::ATan2(fTracks.Laser.dirU.mX2, fTracks.Laser.dirU.mX2)-TMath::ATan2(fTracks.dirPU.mX2,fTracks.dirPU.mX1):fTracks.mSector>>dPhi(12,1,25,100,-.15,0.15)","fTracks.Flag>1","colz");
  laser->Draw(
	      "atan(fTracks.Laser.dirU.mX2/fTracks.Laser.dirU.mX1)-atan(fTracks.dirPU.mX2/fTracks.dirPU.mX1):fTracks.mSector>>dPhi(12,1,25,100,-.15,0.15)",
	      "fTracks.Flag>1&&abs(fTracks.dU.mX1)<1&&abs(fTracks.dU.mX2)<1","colz");
  TH2D *dPhi = (TH2D *) gDirectory->Get("dPhi");
  if (! dPhi) return;
  dPhi->FitSlicesY();
  TH1D *dPhi_1 = (TH1D *) gDirectory->Get("dPhi_1");
  dPhi_1->SetMarkerStyle(20);
  dPhi_1->Draw("same");
  for (Int_t bin = 1; bin <= 12; bin++) {
    cout << "sector: " << 2*bin << " dPhi: " << 1e3*dPhi_1->GetBinContent(bin) << " +/- " 
	 << 1e3*dPhi_1->GetBinError(bin) << "(mrad)" << endl;
  }
}  
//________________________________________________________________________________
void dTheta() {
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser) return;
  laser->Draw("atan(fTracks.Laser.dirU.mX3)-atan(fTracks.dirPU.mX3):fTracks.mSector>>dTheta(12,1,25,100,-.15,0.15)",
	      "fTracks.Flag>1&&abs(fTracks.dU.mX1)<1&&abs(fTracks.dU.mX2)<1","colz");
  TH2D *dTheta = (TH2D *) gDirectory->Get("dTheta");
  if (! dTheta) return;
  dTheta->FitSlicesY();
  TH1D *dTheta_1 = (TH1D *) gDirectory->Get("dTheta_1");
  dTheta_1->SetMarkerStyle(20);
  dTheta_1->Draw("same");
  for (Int_t bin = 1; bin <= 12; bin++) {
    cout << "sector: " << 2*bin << " dTheta: " << 1e3*dTheta_1->GetBinContent(bin) << " +/- " 
	 << 1e3*dTheta_1->GetBinError(bin) << "(mrad)" << endl;
  }
}  
//________________________________________________________________________________
void dX() {
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser) return;
  laser->Draw("fTracks.XyzPU.mX1-fTracks.Laser.XyzU.mX1:7*(3*(fTracks.Laser.Sector-2)+fTracks.Laser.Bundle-1)+fTracks.Laser.Mirror-0.5>>dX(504,0,504,100,-.2,0.2)","fTracks.Flag>1","colz");
  TH2D *dX = (TH2D *) gDirectory->Get("dX");
  if (! dX) return;
  dX->FitSlicesY();
  TH1D *dX_1 = (TH1D *) gDirectory->Get("dX_1");
  dX_1->SetMarkerStyle(20);
  dX_1->Draw("same");
  for (Int_t bin = 1; bin <= 504; bin++) {
    Int_t mirror = (bin-1)%7 + 1;
    Int_t bundle = ((bin-1)/7)%6 + 1;
    Int_t sector = ((bin-1)/(7*6)+1)*2;
    cout << "sector: " << sector << " bundle: " << bundle << " mirror: " << mirror  
	 << " dX: " << dX_1->GetBinContent(bin) << " +/- " 
	 << dX_1->GetBinError(bin) << "(cm)" << endl;
  }
}  
//________________________________________________________________________________
void dY() {
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser) return;
  laser->Draw("fTracks.XyzPU.mX2-fTracks.Laser.XyzU.mX2:7*(3*(fTracks.Laser.Sector-2)+fTracks.Laser.Bundle-1)+fTracks.Laser.Mirror-0.5>>dY(504,0,504,100,-.2,0.2)","fTracks.Flag>1","colz");
  TH2D *dY = (TH2D *) gDirectory->Get("dY");
  if (! dY) return;
  dY->FitSlicesY();
  TH1D *dY_1 = (TH1D *) gDirectory->Get("dY_1");
  dY_1->SetMarkerStyle(20);
  dY_1->Draw("same");
  for (Int_t bin = 1; bin <= 504; bin++) {
    Int_t mirror = (bin-1)%7 + 1;
    Int_t bundle = ((bin-1)/7)%6 + 1;
    Int_t sector = ((bin-1)/(7*6)+1)*2;
    cout << "sector: " << sector << " bundle: " << bundle << " mirror: " << mirror  
	 << " dY: " << dY_1->GetBinContent(bin) << " +/- " 
	 << dY_1->GetBinError(bin) << "(cm)" << endl;
  }
}  
//________________________________________________________________________________
void FillHists(Int_t n1 = 0,const Char_t *Dir = ".") {
  TFileSet *fileset = new TFileSet(Dir);
  TDataSetIter iter(fileset);
  Int_t NF = 0;
  Int_t NFiles = 0;
  const Int_t NperC = 64;
  TString cut("fFit.Prob>1e-2&&fFit.ndf>4");
  TDataSet *set = 0;
  while ((set = iter())) {
    TString Title(set->GetTitle());
    if (Title != "file") continue;
    TString file(set->GetName());
    //    cout << file << end;
    if (!file.BeginsWith("laser") || !file.EndsWith(".root")) continue;
    //    cout << "  +++++++ accepted" << endl;
    TFile *f = new TFile(file);
    TTree *laser = (TTree *) f->Get("laser");
    if (! laser) {
      cout << "Skip file " << f->GetName() << " with no laser TTree" << endl;
    }
    else {
      Long64_t n = laser->Draw("fFit.slope",cut,"goff");
      if (n < 100) {
	cout << "Skip file " << f->GetName() << " with " << n << " Entries" << endl;
      } else {
	NFiles++;
	if (n1 >= 0) {
	  if (NFiles <= NperC*n1) continue;
	  if (NFiles >  NperC*(n1+1)) break;
	}
	cout << "Add  file " << f->GetName() << " with " << n << " Entries" << endl;
	set->Mark();
	NF++;
      }
    }
    delete f;
  }
  if (NF == 0) return;
  Int_t ny = (Int_t) TMath::Sqrt(NF);
  Int_t nx = NF/ny;
  if (nx*ny != NF) nx++;
  cout << "NF = " << NF << " nx " << nx << " ny " << ny << endl;
  TCanvas *c[2] = {0,0};
  TString OutFile("LaserOut");
  if (n1 >= 0) OutFile += n1;
  OutFile += ".root";
  TFile *fOut = new TFile(OutFile,"recreate");
  iter.Reset();
  Int_t i = 0;
  //  Int_t i1 = 0;
  if (n1 < 0) n1 = 0;
  while ((set = iter())) {
    if (! set->IsMarked()) continue;
    memset(&DVAll[0][0], 0, 6*sizeof(Double_t));
    memset(&dDVAll[0][0], 0, 6*sizeof(Double_t));
    TString file(set->GetName());
    cout << "Open " << file << endl;
    TFile *f = new TFile(file);
    TTree *laser = (TTree *) f->Get("laser");
    if (! laser) {
      cout << "Cannot find laser TTRee in marked file " << file << endl;
      continue;
    }
    if (! c[0] || ! c[1]) {
      c[0] = new TCanvas(Form("Slopes %i",n1),Form("Slopes %i",n1));  
      c[1] = new TCanvas(Form("LDV %i",n1),Form("Laser Drift Velocities %i",n1));
      c[0]->Divide(nx,ny);
      c[1]->Divide(nx,ny);
    }
    laser->SetMarkerStyle(20);
    laser->Draw("fEvtHdr.fRun:fEvtHdr.fDate:fEvtHdr.fTime","","goff",1);
    run = (Int_t) laser->GetV1()[0];
    date = (Int_t) laser->GetV2()[0];
    Time = (Int_t) laser->GetV3()[0];
    cout << "Run = " << run << " date = " << date << " Time = " << Time << endl;
    //    Int_t color = i+1;
    //    laser->SetMarkerColor(color);
    TString histN, plot, plotW,  Plot, Plot2, profN;
    TString plotE;
    TString CutW(cut); CutW += " && fFit.Sector < 13";
    TString CutE(cut); CutE += " && fFit.Sector > 12";
    Char_t *cutName[3] = {"All ","West","East"};
    Char_t *plName[2] = {"SS", "dV"};
    Char_t *FMT[2] = {"%s dV = (%7.2f +/- %7.2f) x 10^3","%s dV = (%10.5f +/- %10.5f) cm/mksec"};
    for (Int_t l = 0; l < 2; l++) {
      c[l]->cd(i+1)->SetLogz(1);
      histN = Form("%s%i",plName[l],run);
      if (l == 0) {
	plotW  = "1e3*fFit.slope:fFit.Sector>>";
	plotE  = plotW; plotE += "+";
	//	plotE  = "1e3*(fFit.slope+3.63518e-04):fFit.Sector>>+";
      } else {
	plotW = "1e-6*fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/(1.+fFit.slope-fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/TMath::Ccgs()):fFit.Sector>>";
	plotE = plotW; plotE += "+";
	//	plotE = "1e-6*fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/(1.+fFit.slope+3.63518e-04-fEvtHdr.fOnlClock/fEvtHdr.fClock*fEvtHdr.fDriVel/TMath::Ccgs()):fFit.Sector>>+";
      }
      Plot  = plotW;
      profN = histN;
      profN += "_pfx";
      Plot += profN;
      Plot += "(24,1,25)";
      fOut->cd();
      Long64_t n = laser->Draw(Plot,CutW,"prof");
      Plot = plotE; Plot += profN;
      n += laser->Draw(Plot,CutE,"prof");
      if (n < 10) {
	cout << f->GetName() << " empty only " << n << " entries" << endl;
	continue;
      }
      TProfile *SSp = (TProfile *) gDirectory->Get(profN);
      if (! SSp) continue;
      if (SSp->GetEntries() <= 10) {
	cout << SSp->GetName() << " empty only " << n << " entries" << endl;
	continue;
      }
      Plot2 = plotW; 
      Plot2 += histN;
      SSp->SetMarkerColor(3);
      Double_t ymin = SSp->GetMean(2) - 3*SSp->GetRMS(2);
      Double_t ymax = SSp->GetMean(2) + 3*SSp->GetRMS(2);
      Plot2 += Form("(24,1,25,100,%f,%f)",ymin,ymax);
      laser->Draw(Plot2,CutW,"colz");
      Plot2 = plotE;
      Plot2 += histN;
      laser->Draw(Plot2,CutE,"colz");
      TH2F *SS = (TH2F *) gDirectory->Get(histN);
      if (! SS) {
	cout << "Did not find histogram " << histN << endl;
	continue;
      }
      SS->FitSlicesY();
      TString fitN(histN);
      fitN += "_1";
      TH1D *fit = (TH1D *) gDirectory->Get(fitN);
      TLegend *leg = new TLegend(0.1,0.1,0.55,0.25,"");
      if (fit) {
	fit->SetMarkerStyle(20);
	TF1 *pol0 = (TF1*) gROOT->GetFunction("pol0");
	if (pol0) {
	  for (Int_t k = 0; k < 3; k++) {
	    pol0->SetLineColor(k+1);
	    if (k == 0) fit->Fit(pol0,"er","",1,25); 
	    if (k == 1) fit->Fit(pol0,"er+","",1,13);
	    if (k == 2) fit->Fit(pol0,"er+","",13,25);
	    DVAll[l][k]  = pol0->GetParameter(0);
	    dDVAll[l][k] = pol0->GetParError(0);
	    if (!l || dDVAll[l][k] > 0 && dDVAll[l][k]< 1e-3)
	      leg->AddEntry((TF1 *) fit->GetListOfFunctions()->Last(), Form(FMT[l],cutName[k],DVAll[l][k],dDVAll[l][k]));
	  }
	}
      }
      SS->SetTitle(Form("Run: %i",run));
      SS->SetXTitle("Sector");
      SS->SetYTitle("Drift Velocity ");
      SS->Draw("colz");
      if (SSp) SSp->Draw("same");
      if (fit) fit->Draw("same");
      if (l && DVAll[l][0] > 0) {
	if (DVAll[l][0] > 5.5 &&  dDVAll[l][0] > 0 && dDVAll[l][0]< 1e-3) MakeTable();
	else {
	  cout << "File " << f->GetName() << " fails =============================" << endl;
	  leg->AddEntry(fit, "Rejected");
	}
      }
      leg->Draw();
      c[l]->Update();
    }
    i++;
//     if (n1 > 1 && i >= nx*ny) {
//       i1++; i = 0;
//     }
  }
  fOut->Write();
}
//________________________________________________________________________________
void Laser(Int_t n1 = -1){
  //  Slopes();
  FillHists(n1);
}
