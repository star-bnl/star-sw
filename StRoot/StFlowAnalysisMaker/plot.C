///////////////////////////////////////////////////////////////////////////////
//
// $Id: plot.C,v 1.70 2011/07/25 15:54:53 posk Exp $
//
// Author:       Art Poskanzer, LBNL, Aug 1999
//               FTPC added by Markus Oldenburg, MPI, Dec 2000
// Description:  Macro to plot histograms made by StFlowAnalysisMaker,
//                 StFlowLeeYangZerosMaker, and StFlowScalarProdMaker.
//               If selN = 0 plot all selections and harmonics.
//               First time type .x plot.C() to see the menu.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               Hist file is anaXX.root where XX is the number.
//               Default hist file is flow.hist.root .
//               After the first execution, just type plot(N) .
//               A negative N plots all pages starting with page N.
//               Place a symbolic link to this file in StRoot/macros/analysis .
//
///////////////////////////////////////////////////////////////////////////////

#include <math.h> 
#include "TMath.h" 
#include <iostream.h>
#include <iomanip.h>


const    Int_t nHars    = 4; // 4 
const    Int_t nSels    = 2;
const    Int_t nSubs    = 2;
Int_t    runNumber      = 0;
char     runName[6];
char     fileNumber[4]  = "x";
char     fileName[30];
TFile*   histFile;
char     tmp[10];
char     runNo[10];
TCanvas* can;

TCanvas* plot(Int_t pageNumber=0, Int_t selN=0, Int_t harN=0){
  gInterpreter->ProcessLine(".O0");

  Bool_t includeFtpc = kTRUE;
  Bool_t reCent = kFALSE;

  bool multiGraph  = kFALSE;                            // set flags
  bool singleGraph = kFALSE;
  if (selN == 0) multiGraph = kTRUE;

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  //gROOT->SetStyle("Pub");                              // set style
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();
  gStyle->SetOptStat(10);
  gStyle->SetPalette(1);

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char* baseName1[] = { // with FTPCs
    "Flow_Res_Sel",
    "Flow_Trigger",
    "Flow_VertexZ",
    "Flow_VertexXY2D",
    "Flow_EtaSymVerZ2D_Tpc",
    "Flow_EtaSym_Tpc",
    "Flow_EtaSymVerZ2D_Ftpc",
    "Flow_EtaSym_Ftpc",
    "Flow_CTBvsZDC2D",
    "Flow_Cent",
    "Flow_Mult",
    "Flow_OrigMult",
    "Flow_MultOverOrig",
    "Flow_MultEta",
    "Flow_MultPart",
    "Flow_Charge_Ftpc",
    "Flow_DcaGlobal_Tpc",
    "Flow_DcaGlobal_Ftpc",
    "Flow_Dca_Tpc",
    "Flow_Dca_Ftpc",
    "Flow_Chi2_Tpc",
    "Flow_Chi2_Ftpc",
    "Flow_FitPts_Tpc",
    "Flow_MaxPts_Tpc",
    "Flow_FitOverMax_Tpc",
    "Flow_FitPts_Ftpc",
    "Flow_MaxPts_Ftpc",
    "Flow_FitOverMax_Ftpc",
    //"Flow_EtaPtPhi3D",
    "Flow_EtaPtPhi2D.PhiEta",
    "Flow_EtaPtPhi2D.PhiPt",
    "Flow_YieldAll2D",
    "Flow_YieldAll.Eta",
    "Flow_YieldAll.Pt",
    "Flow_YieldPart2D",
    "Flow_YieldPart.Eta",
    "Flow_YieldPart.Pt",
    //"Flow_Bin_Eta",
    //"Flow_Bin_Pt",
    "Flow_MeanDedxPos2D",
    "Flow_MeanDedxNeg2D",
//     "Flow_PidPiPlusPart",
//     "Flow_PidPiMinusPart",
//     "Flow_PidProtonPart",
//     "Flow_PidAntiProtonPart",
//     "Flow_PidKplusPart",
//     "Flow_PidKminusPart",
//     "Flow_PidDeuteronPart",
//     "Flow_PidAntiDeuteronPart",
//     "Flow_PidElectronPart",
//     "Flow_PidPositronPart",
    "Flow_PidMult",
    "Flow_Phi_FarEast_Sel",                      // first multi graph hist
    "Flow_Phi_Flat_FarEast_Sel",
    "Flow_Phi_East_Sel",
    "Flow_Phi_Flat_East_Sel",
    "Flow_Phi_West_Sel",
    "Flow_Phi_Flat_West_Sel",
    "Flow_Phi_FarWest_Sel",
    "Flow_Phi_Flat_FarWest_Sel",
    "Flow_Phi_FtpcFarEast_Sel",
    "Flow_Phi_Flat_FtpcFarEast_Sel",
    "Flow_Phi_FtpcEast_Sel",
    "Flow_Phi_Flat_FtpcEast_Sel",
    "Flow_Phi_FtpcWest_Sel",
    "Flow_Phi_Flat_FtpcWest_Sel",
    "Flow_Phi_FtpcFarWest_Sel",
    "Flow_Phi_Flat_FtpcFarWest_Sel",
    "Flow_Mul_Sel",
    "Flow_Yield2D_Sel",
    "Flow_Yield.Eta_Sel",
    "Flow_Yield.Pt_Sel",
    "Flow_Psi_Subs",
    "Flow_Psi_Sel",
    "Flow_PhiLab_Sel",
    "Flow_Psi_Diff_Sel",
    "Flow_Psi_Sub_Corr_Sel",
    "Flow_Psi_Sub_Corr_Diff_Sel",
    "Flow_Phi_Corr_Sel",
    "Flow_vObs2D_Sel",
    "Flow_vObsEta_Sel",
    "Flow_vObsPt_Sel",
    "Flow_v2D_Sel",
    "Flow_vEta_Sel",
    "Flow_vPt_Sel",
    "Flow_q_Sel",
    "FlowLYZ_vEta_Sel",
    "FlowLYZ_vPt_Sel",
    "Flow_vObs2D_ScalarProd_Sel",
    "Flow_v2D_ScalarProd_Sel",
    "Flow_vEta_ScalarProd_Sel",
    "Flow_vPt_ScalarProd_Sel"
  };

  const char* baseName2[] = { // without FTPCs
    "Flow_Res_Sel",
    "Flow_Trigger",
    "Flow_VertexZ",
    "Flow_VertexXY2D",
    "Flow_EtaSymVerZ2D_Tpc",
    "Flow_EtaSym_Tpc",
    "Flow_CTBvsZDC2D",
    "Flow_Cent",
    "Flow_Mult",
    "Flow_OrigMult",
    "Flow_MultOverOrig",
    "Flow_MultEta",
    "Flow_MultPart",
    "Flow_Dca_Tpc",
    "Flow_DcaGlobal_Tpc",
    "Flow_Chi2_Tpc",
    "Flow_FitPts_Tpc",
    "Flow_MaxPts_Tpc",
    "Flow_FitOverMax_Tpc",
    //"Flow_EtaPtPhi3D",
    "Flow_EtaPtPhi2D.PhiEta",
    "Flow_EtaPtPhi2D.PhiPt",
    "Flow_YieldAll2D",
    "Flow_YieldAll.Eta",
    "Flow_YieldAll.Pt",
    "Flow_YieldPart2D",
    "Flow_YieldPart.Eta",
    "Flow_YieldPart.Pt",
    //"Flow_Bin_Eta",
    //"Flow_Bin_Pt",
//     "Flow_MeanDedxPos2D",
//     "Flow_MeanDedxNeg2D",
//     "Flow_PidPiPlusPart",
//     "Flow_PidPiMinusPart",
//     "Flow_PidProtonPart",
//     "Flow_PidAntiProtonPart",
//     "Flow_PidKplusPart",
//     "Flow_PidKminusPart",
//     "Flow_PidDeuteronPart",
//     "Flow_PidAntiDeuteronPart",
//     "Flow_PidElectronPart",
//     "Flow_PidPositronPart",
    "Flow_PidMult",
    "Flow_Phi_FarEast_Sel",                      // first multi graph hist
    "Flow_Phi_Flat_FarEast_Sel",
    "Flow_Phi_East_Sel",
    "Flow_Phi_Flat_East_Sel",
    "Flow_Phi_West_Sel",
    "Flow_Phi_Flat_West_Sel",
    "Flow_Phi_FarWest_Sel",
    "Flow_Phi_Flat_FarWest_Sel",
    "Flow_Mul_Sel",
    "Flow_Yield2D_Sel",
    "Flow_Yield.Eta_Sel",
    "Flow_Yield.Pt_Sel",
    "Flow_Psi_Subs",
    "Flow_Psi_Sel",
    "Flow_PhiLab_Sel",
    "Flow_Psi_Diff_Sel",
    "Flow_Psi_Sub_Corr_Sel",
    "Flow_Psi_Sub_Corr_Diff_Sel",
    "Flow_Phi_Corr_Sel",
    "Flow_vObs2D_Sel",
    "Flow_vObsEta_Sel",
    "Flow_vObsPt_Sel",
    "Flow_v2D_Sel",
    "Flow_vEta_Sel",
    "Flow_vPt_Sel",
    "Flow_q_Sel",
    "FlowLYZ_vEta_Sel",
    "FlowLYZ_vPt_Sel",
    "Flow_vObs2D_ScalarProd_Sel",
    "Flow_v2D_ScalarProd_Sel",
    "Flow_vEta_ScalarProd_Sel",
    "Flow_vPt_ScalarProd_Sel"
  };

  const char* baseName3[] = { // for reCent
    "Flow_Res_Sel",
    "Flow_Trigger",
    "Flow_VertexZ",
    "Flow_VertexXY2D",
    "Flow_EtaSymVerZ2D_Tpc",
    "Flow_EtaSym_Tpc",
    "Flow_EtaSymVerZ2D_Ftpc",
    "Flow_EtaSym_Ftpc",
    "Flow_CTBvsZDC2D",
    "Flow_Cent",
    "Flow_Mult",
    "Flow_OrigMult",
    "Flow_MultOverOrig",
    "Flow_MultEta",
    "Flow_MultPart",
    "Flow_Charge_Ftpc",
    "Flow_DcaGlobal_Tpc",
    "Flow_DcaGlobal_Ftpc",
    "Flow_Dca_Tpc",
    "Flow_Dca_Ftpc",
    "Flow_Chi2_Tpc",
    "Flow_Chi2_Ftpc",
    "Flow_FitPts_Tpc",
    "Flow_MaxPts_Tpc",
    "Flow_FitOverMax_Tpc",
    "Flow_FitPts_Ftpc",
    "Flow_MaxPts_Ftpc",
    "Flow_FitOverMax_Ftpc",
    //"Flow_EtaPtPhi3D",
    "Flow_EtaPtPhi2D.PhiEta",
    "Flow_EtaPtPhi2D.PhiPt",
    "Flow_YieldAll2D",
    "Flow_YieldAll.Eta",
    "Flow_YieldAll.Pt",
    "Flow_YieldPart2D",
    "Flow_YieldPart.Eta",
    "Flow_YieldPart.Pt",
    //"Flow_Bin_Eta",
    //"Flow_Bin_Pt",
    "Flow_MeanDedxPos2D",
    "Flow_MeanDedxNeg2D",
//     "Flow_PidPiPlusPart",
//     "Flow_PidPiMinusPart",
//     "Flow_PidProtonPart",
//     "Flow_PidAntiProtonPart",
//     "Flow_PidKplusPart",
//     "Flow_PidKminusPart",
//     "Flow_PidDeuteronPart",
//     "Flow_PidAntiDeuteronPart",
//     "Flow_PidElectronPart",
//     "Flow_PidPositronPart",
    "Flow_PidMult",
    "Flow_Mul_Sel",                    // first multi graph hist
    "Flow_Yield2D_Sel",
    "Flow_Yield.Eta_Sel",
    "Flow_Yield.Pt_Sel",
    "Flow_Psi_Subs",
    "Flow_Psi_Sel",
    "Flow_PhiLab_Sel",
    "Flow_Psi_Diff_Sel",
    "Flow_Psi_Sub_Corr_Sel",
    "Flow_Psi_Sub_Corr_Diff_Sel",
    "Flow_Phi_Corr_Sel",
    "Flow_QXY2D_Sel",
    "Flow_QFTPCSubXY2D_Sel",
    "Flow_QTPCSubXY2D_Sel",
    "Flow_vObs2D_Sel",
    "Flow_vObsEta_Sel",
    "Flow_vObsPt_Sel",
    "Flow_v2D_Sel",
    "Flow_vEta_Sel",
    "Flow_vPt_Sel",
    "Flow_q_Sel",
    "FlowLYZ_vEta_Sel",
    "FlowLYZ_vPt_Sel",
    "Flow_vObs2D_ScalarProd_Sel",
    "Flow_v2D_ScalarProd_Sel",
    "Flow_vEta_ScalarProd_Sel",
    "Flow_vPt_ScalarProd_Sel"
  };

  const Int_t firstMultFtpc = 38; // these must be set by hand
  const Int_t firstMultTpc  = 27;
  Int_t nSingles;
  Int_t nName;
  if (reCent) {
    nName = sizeof(baseName3) / sizeof(char*);
    nSingles = firstMultFtpc + 1;
  } else if (includeFtpc) {
    nName = sizeof(baseName1) / sizeof(char*);
    nSingles = firstMultFtpc + 1;
  } else {
    nName = sizeof(baseName2) / sizeof(char*);
    nSingles = firstMultTpc + 1;
  }
  const Int_t nNames = nName;

  // construct arrays of base and short names
  char* baseName[nNames];
  char* shortName[nNames];
  for (int n = 0; n < nNames; n++) {
    baseName[n] = new char[35];
    shortName[n] = new char[35];
    float etaMax;
    if (reCent) {
      strcpy(baseName[n], baseName3[n]);
      etaMax  =   4.5;
    } else if (includeFtpc) {
      strcpy(baseName[n], baseName1[n]);
      etaMax  =   4.5;
    } else {
      strcpy(baseName[n], baseName2[n]);
      etaMax  =   1.5;
    }
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"_Sel");
    if (cp) *cp = '\0';                                  // truncate
  }

  // input the run number
  if (runNumber == 0) {
    cout << "     run number? ";    
    fgets(runNo, sizeof(runNo), stdin);
    runNumber = atoi(runNo);
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " run name = " << runName << endl;
  }

  // input the file number (0 opens flow.hist.root)
  if (strstr(fileNumber, "x")!=0) {
    cout << "     anaXX.root file number? [0= flow.hist.root] ";
    fgets(fileNumber, sizeof(fileNumber), stdin);
    fileNumber[strlen(fileNumber)-1] = '\0';
    if (strlen(fileNumber) == 1 && strstr(fileNumber,"0")) {
      sprintf(fileName, "flow.hist.root");
    } else {
      sprintf(fileName, "ana%s.root", fileNumber);       // insert
    }
    cout << " file name = " << fileName << endl;
    histFile = new TFile(fileName);
  }
  
  // input the page number
  while (pageNumber <= 1 || pageNumber > nNames) {
    if (pageNumber < 0) {                                 // plot all
      plotAll(nNames, selN, harN, -pageNumber);
      return can;
    }
    if (pageNumber == 1) {                                // plot resolution
      can = plotResolution();
      return can;
    }
    cout << "-1: \t All" << endl;                         // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << shortName[i] << endl;
    }
    cout << "     page number? ";
    fgets(tmp, sizeof(tmp), stdin);
    pageNumber = atoi(tmp);
  }
  if (pageNumber > 0 && pageNumber <= nSingles) {         // plot singles
    singleGraph = kTRUE;
    multiGraph  = kFALSE;
  }
  pageNumber--;
  cout << pageNumber+1 << ":  graph name= " << shortName[pageNumber] << endl;

  // set constants
  float twopi   = 2. * TMath::Pi();
  float phiMax  = twopi; 
  float Ycm     =   0.0;
  TString* histProjName = NULL;

  // set row and column numbers
  char* cp = strstr(shortName[pageNumber],"Subs");
  int columns = (cp) ? nSubs * nSels : nSels;
  int rows;
  rows = (strstr(shortName[pageNumber],"Phi_") ||
	  strstr(shortName[pageNumber],"Psi_Diff") ? 2 : nHars);
  //rows = (strstr(shortName[pageNumber],"Phi_") ? 2 : nHars);
  if (strcmp(shortName[pageNumber],"Flow_Phi_Corr")==0) rows = nHars;
  if (strcmp(shortName[pageNumber],"Flow_Psi_Sub_Corr_Diff")==0) rows = 3;
  int pads = rows*columns;
  cout << "pads = " << pads << endl;

  // make the graph page
  if (multiGraph) {
    int canvasWidth = 600, canvasHeight = 780;             // portrait
  } else {
    int canvasWidth = 780, canvasHeight = 600;             // landscape
  }
  TString* canName = new TString(shortName[pageNumber]);
  if (runNumber) {
    *canName += runNumber;
  } 
  cout << canName->Data() << endl;
  can = new TCanvas(canName->Data(), canName->Data(), canvasWidth, canvasHeight);
  can->ToggleEventStatus();
  if (multiGraph) {
    TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,shortName[pageNumber]);
    title->Draw();
  }
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now.AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();

  if (multiGraph) {               // many graphs on one page
    graphPad->Divide(columns,rows);
    int firstK = 0, firstJ = 0, lastK = columns, lastJ = rows;
  } else if (singleGraph) {       // single graph on a page
    int firstK = 0, firstJ = 0, lastK = 1, lastJ = 1;
  } else {                        // one graph from a multi graph page
    int firstK = selN -1, firstJ = harN -1, lastK = selN, lastJ = harN;
  }
  TLine* lineZeroY  = new TLine(-etaMax, 0., etaMax, 0.);
  TLine* lineYcm    = new TLine(Ycm, -10., Ycm, 10.);
  TLine* lineOnePhi = new TLine(0., 1., phiMax, 1.);
  for (int j = firstJ; j < lastJ; j++) {
    float order = (float)(j+1);
    for (int k = firstK ; k < lastK; k++) {
      int padN = j*columns + k + 1;                    // pad number

      // construct histName and histProjName
      char* temp = new char[35];
      strcpy(temp,shortName[pageNumber]);
      char* cproj = strstr(temp,".");
      if (cproj) {                                     // a projection
	*cproj = '\0';                                 // remove from "." on
	if (singleGraph) {
	  cproj = strstr(temp,"2");
	  if (cproj) {                                 // a 2D projection 
	    *cproj = '\0';                             // remove from "2D" on
	    strcat(temp,"3D");
	  } else {
	    strcat(temp,"2D");
	  }
	} else {
	  strcat(temp,"2D_Sel");
	}
	TString* histName = new TString(temp);
	histProjName = new TString(baseName[pageNumber]);
	if (!singleGraph) {
	  *histProjName += k+1;
	  histProjName->Append("_Har");
	  *histProjName += j+1;
	}
      } else {                                         // not projection
	TString* histName = new TString(baseName[pageNumber]);
      }
      if (!singleGraph) {
	*histName += k+1;
	histName->Append("_Har");
	*histName += j+1;
      }
      cout << " col= " << k+1 << " row= " << order << " pad= " << padN << "\t" 
	   << histName->Data() << endl;

      // get the histogram
      bool twoD = kFALSE;
      bool threeD = kFALSE;
      if (histProjName) {
	if (strstr(temp,"3D")) {                      // 2D projection
	  twoD = kTRUE;
	  TH3* hist3D = dynamic_cast<TH3*>(histFile->Get(histName->Data()));
	  if (!hist3D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return can;
	  }
	} else {                                      // 1D projection
	  TH2* hist2D = dynamic_cast<TH2*>(histFile->Get(histName->Data()));
	  if (!hist2D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return can;
	  }
	}
      } else {
	if (strstr(shortName[pageNumber],"3D")!=0) {  // 3D
	  threeD = kTRUE;
	  TH3* hist3D = dynamic_cast<TH3*>(histFile->Get(histName->Data()));
	  if (!hist3D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return can;
	  }
	} else if (strstr(shortName[pageNumber],"2D")!=0) { // 2D
	  twoD = kTRUE;
	  TH2* hist2D = dynamic_cast<TH2*>(histFile->Get(histName->Data()));
	  if (!hist2D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return can;
	  }
	} else {                                            // 1D
	  TH1* hist = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
	  if (!hist) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return can;
	  }
	  float max = hist->GetXaxis()->GetXmax();
	}
      }
      
      // make the plots
      if (multiGraph) graphPad->cd(padN);
      if (threeD) {                                            // 3D
	gStyle->SetOptStat(10);
	hist3D->Draw("BOX");
      } else if (twoD) {                                       // 2D
	if (strstr(shortName[pageNumber],".PhiEta")!=0) {      // 3D Phi Eta proj.
	  TH2D* projZX = hist3D->Project3D("zx");
	  projZX->SetName(histProjName->Data());
	  projZX->SetYTitle("azimuthal angle (rad)");
	  projZX->SetXTitle("rapidity");
	  gStyle->SetOptStat(0);
	  if (projZX) projZX->Draw("COLZ");
	} else if (strstr(shortName[pageNumber],".PhiPt")!=0) { // 3D Phi Pt proj.
	  TH2D* projZY = hist3D->Project3D("zy");
	  projZY->SetName(histProjName->Data());
	  projZY->SetYTitle("azimuthal angle (rad");
	  projZY->SetXTitle("Pt (GeV/c)");
	  gStyle->SetOptStat(0);
	  if (projZY) projZY->Draw("COLZ");
	} else	if ((strstr(shortName[pageNumber],"QXY")!=0)) { // Q XY
	  TLine* lineZeroX = new TLine(-0.5, 0., 0.5, 0.);
	  TLine* lineZeroY = new TLine(0., -0.5, 0., 0.5);
	  gStyle->SetOptStat(100110);
	  hist2D->Draw("COLZ");
	  //hist2D->Draw("CONT");
	  lineZeroX->Draw();
	  lineZeroY->Draw();
	} else	if ((strstr(shortName[pageNumber],"TPCSubXY")!=0)) { // QSub XY
	  TLine* lineZeroX = new TLine(-1., 0., 1., 0.);
	  TLine* lineZeroY = new TLine(0., -1., 0., 1.);
	  gStyle->SetOptStat(100110);
	  hist2D->Draw("COLZ");
	  //hist2D->Draw("CONT");
	  lineZeroX->Draw();
	  lineZeroY->Draw();
	} else	if (strstr(shortName[pageNumber],"XY")!=0) {    // Vertex XY
	  TLine* lineZeroX = new TLine(-1., 0., 1., 0.);
	  TLine* lineZeroY = new TLine(0., -1., 0., 1.);
	  gStyle->SetOptStat(10);
	  hist2D->Draw("COLZ");
	  lineZeroX->Draw();
	  lineZeroY->Draw();
	} else if (strstr(shortName[pageNumber],"Dedx")!=0) {   // dE/dx
	  TVirtualPad::Pad()->SetLogz();
	  gStyle->SetOptStat(10);
	  hist2D->Draw("COLZ");
	} else if (strstr(shortName[pageNumber],"_v")!=0) {    // v
	  hist2D->SetMaximum(20.);
	  hist2D->SetMinimum(-20.);
	  gStyle->SetOptStat(0);
	  hist2D->Draw("COLZ");
	} else {                                               // other 2D
	  gStyle->SetOptStat(10);
	  hist2D->Draw("COLZ");
	}
      } else if (strstr(shortName[pageNumber],".Eta")!=0) { // 2D Eta projection
	if (singleGraph) {
	  TH1D* projX = hist2D->ProjectionX(histName->Data());
	} else {
	  //TH1D* projX = hist2D->ProjectionX(histName->Data(), -1, 9999, "E");
	  TH1D* projX = hist2D->ProjectionX(histName->Data());
	}
	projX->SetName(histProjName->Data());
	char* xTitle = hist2D->GetXaxis()->GetTitle();
	projX->SetXTitle(xTitle);
	projX->SetYTitle("Counts");
	gStyle->SetOptStat(0);
	if (projX) projX->Draw("H");
	if (!singleGraph) lineZeroY->Draw();
      } else if (strstr(shortName[pageNumber],".Pt")!=0) { // 2D Pt projection
	if (singleGraph) {
	  TH1D* projY = hist2D->ProjectionY(histName->Data()); 
	} else {
	  TH1D* projY = hist2D->ProjectionY(histName->Data(), -1, 9999, "E");
	}
	projY->SetName(histProjName->Data());
	projY->SetXTitle("Pt (GeV/c)");
	projY->SetYTitle("Counts");
	TVirtualPad::Pad()->SetLogy();
	gStyle->SetOptStat(0);
	if (projY) projY->Draw("H");
      } else if (strstr(shortName[pageNumber],".Phi")!=0) { // 2D Phi projection
	if (singleGraph) {
	  TH1D* projY = hist2D->ProjectionY(histName->Data()); 
	} else {
	  TH1D* projY = hist2D->ProjectionY(histName->Data(), -1, 9999, "E");
	}
	projY->SetName(histProjName->Data());
	projY->SetXTitle("azimuthal angle (rad");
	projY->SetYTitle("Counts");
	gStyle->SetOptStat(0);
	if (projY) projY->Draw("H");
      } else if (strstr(shortName[pageNumber],"Corr")!=0) { // azimuthal corr.
	float norm = (hist->Integral()) ? ((float)(hist->GetNbinsX()) / hist->Integral()) : 1.; 
	cout << "  Normalized by: " << norm << endl;
	hist->Scale(norm);                           // normalize height to one
	if (strstr(shortName[pageNumber],"Diff")!=0) { 
	  TF1* funcCos1 = new TF1("funcCos1",
				  "1+[0]*2/100*cos([1]*x)", 0., twopi);
	  funcCos1->SetParNames("k=1", "har");
	  funcCos1->SetParameters(0, order+1);              // initial values
	  funcCos1->SetParLimits(1, 1, 1);                  // har is fixed
	  //hist->Fit("funcCos1");
	  delete funcCos1;
	} else if (strstr(shortName[pageNumber],"Sub")!=0) { 
	  TF1* funcSubCorr = new TF1("SubCorr", SubCorr, 0., twopi/order, 2);
	  funcSubCorr->SetParNames("chi", "har");
	  funcSubCorr->SetParameters(1., order);             // initial value
	  funcSubCorr->SetParLimits(1, 1, 1);                // har is fixed
	  hist->Fit("SubCorr");
	  delete funcSubCorr;
	} else {
	  TF1* funcCos3 = new TF1("funcCos3",
	 "1+[0]*2/100*cos([3]*x)+[1]*2/100*cos(([3]+1)*x)+[2]*2/100*cos(([3]+2)*x)",
				  0., twopi/order);
	  funcCos3->SetParNames("n=har", "n=har+1", "n=har+2", "har");
	  funcCos3->SetParameters(0, 0, 0, order);          // initial values
	  funcCos3->SetParLimits(3, 1, 1);                  // har is fixed
	  //hist->Fit("funcCos3");
	  delete funcCos3;
	}
	if (strstr(shortName[pageNumber],"Phi")!=0)
	  hist->SetMinimum(0.9*(hist->GetMinimum()));
	gStyle->SetOptStat(10);
	gStyle->SetOptFit(111);
	hist->Draw("E1");
      } else if (strstr(shortName[pageNumber],"_q")!=0) {   // q distibution
	gStyle->SetOptStat(110);
	gStyle->SetOptFit(111);
	hist->Draw("E1");
	float n_qBins = (float)hist->GetNbinsX();
	double area = hist->Integral() * max / n_qBins; 
	TString* histName = new TString("Flow_Mul_Sel");
	*histName += k+1;
	histName->Append("_Har");
	*histName += j+1;
	TH1* histMult = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
	if (!histMult) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return can;
	}
	delete histName;
	float mult = histMult->GetMean();
	TF1* fit_q = new TF1("qDist", qDist, 0., max, 4);
	fit_q->SetParNames("v", "mult", "area", "g");
	float qMean = hist->GetMean();
	float vGuess = (qMean > 1.) ? sqrt(2.*(qMean - 1.) / mult) : 0.03;
	// the 0.03 is a wild guess
	vGuess *= 100.;
	cout << "vGuess = " << vGuess << endl;
	fit_q->SetParameters(vGuess, mult, area, 0.3); // initial values
	fit_q->SetParLimits(1, 1, 1);             // mult is fixed
	fit_q->SetParLimits(2, 1, 1);             // area is fixed
	hist->Fit("qDist", "Q");
	//hist->Fit("qDist");
	fit_q->Draw();
 	fit_q->FixParameter(3, 0.);               // g is fixed
	fit_q->SetLineStyle(kDotted);
	hist->Fit("qDist", "Q+");
	fit_q->Draw("same");
 	fit_q->ReleaseParameter(3);               // g is unfixed
 	fit_q->SetParameter(3, 0.5);              // initial value
 	fit_q->FixParameter(0, 0.);               // v is fixed
	fit_q->SetLineStyle(kDashed);
	hist->Fit("qDist", "Q+");
	fit_q->Draw("same");
 	fit_q->ReleaseParameter(0);               // v is unfixed
      } else if (strstr(shortName[pageNumber],"PhiLab")!=0) {  // Phi lab distibution
	hist->Draw("E1");
	float norm = (hist->Integral()) ? ((float)(hist->GetNbinsX()) / hist->Integral()) : 1.; 
	cout << "  Normalized by: " << norm << endl;
	hist->Scale(norm);                           // normalize height to one
        hist->SetMinimum(0.);
        hist->SetMaximum(1.3);
	TF1* funcCosSin = new TF1("funcCosSin",
		   "1.+[0]*2./100.*cos([2]*x)+[1]*2./100.*sin([2]*x)", 0., twopi);
	funcCosSin->SetParNames("100*cos", "100*sin", "har");
	funcCosSin->SetParameters(0, 0, order); // initial values
	funcCosSin->SetParLimits(2, 1, 1); // har is fixed
	hist->Fit("funcCosSin");
	delete funcCosSin;
	gStyle->SetOptFit(111);
      } else if (strstr(shortName[pageNumber],"Phi")!=0) {  // other Phi distibutions
       	//hist->SetMinimum(0.9*(hist->GetMinimum()));
       	hist->SetMinimum(0.);
	gStyle->SetOptStat(10);
	hist->Draw(); 
      } else if (strstr(shortName[pageNumber],"Psi_Diff")!=0) { // Psi_Diff distibutions
	hist->Draw("E1");
      } else if (strstr(shortName[pageNumber],"Psi")!=0) {    // Psi distibutions
 	float norm = (hist->Integral()) ? ((float)(hist->GetNbinsX()) / hist->Integral()) : 1.; 
	cout << "  Normalized by: " << norm << endl;
	hist->Scale(norm);                           // normalize height to one
      	hist->SetMinimum(0.);
	TF1* funcCosSin = new TF1("funcCosSin",
				"1.+[0]*2./100.*cos([2]*x)+[1]*2./100.*sin([2]*x)", 0., twopi/order);
	funcCosSin->SetParNames("100*cos", "100*sin", "har");
	funcCosSin->SetParameters(0, 0, order); // initial values
	funcCosSin->SetParLimits(2, 1, 1); // har is fixed
	hist->Fit("funcCosSin");
	delete funcCosSin;
	gStyle->SetOptFit(111);
	hist->Draw("E1");
      } else if (strstr(shortName[pageNumber],"Eta")!=0) {    // Eta distibutions
	gStyle->SetOptStat(100110);
 	if (strstr(shortName[pageNumber],"_v")!=0 ) {
	  hist->SetMaximum(10.);
	  hist->SetMinimum(-10.);
	  hist->Draw();
	  lineYcm->Draw();
	} else {
	  hist->Draw();
	}
	lineZeroY->Draw();
      } else if (strstr(shortName[pageNumber],"Pt")!=0) {     // Pt distibutions
 	if (strstr(shortName[pageNumber],"_v")!=0 ) {
	  hist->SetMaximum(25.);
	  hist->SetMinimum(-5.);
	}
	gStyle->SetOptStat(100110);
	hist->Draw();
	if (strstr(shortName[pageNumber],"v")!=0) {
	  TLine* lineZeroPt  = new TLine(0., 0., max, 0.);
	  lineZeroPt->Draw();
	}
      } else if (strstr(shortName[pageNumber],"Bin")!=0) {    // Bin hists
	if (strstr(shortName[pageNumber],"Pt")!=0) {
	  TLine* lineDiagonal = new TLine(0., 0., max, max);
	} else {
	  TLine* lineDiagonal = new TLine(-max, -max, max, max);
	}
	gStyle->SetOptStat(0);
	hist->SetMarkerStyle(21);
	hist->SetMarkerColor(2);
	hist->Draw();
	lineDiagonal->Draw();
      } else if (strstr(shortName[pageNumber],"PidMult")!=0) {  // PID Mult
	TVirtualPad::Pad()->SetLogy();
	gStyle->SetOptStat(0);
	hist->Draw();
      } else {                                              // all other 1D
	gStyle->SetOptStat(100110);
	hist->Draw(); 
      }
      delete [] temp;
      delete histName;
      if (histProjName) delete histProjName;
    }
  }
  delete [] shortName;

  return can;
}

// macro for the resolution plot
TCanvas* plotResolution() {
  char* resName[] = {
    "Flow_Cos_Sel",
    "Flow_Res_Sel",
    "Flow_v_Sel"
//     "Flow_v_ScalarProd_Sel",
//     "Flow_Cumul_v_Order2_Sel",
//     "Flow_Cumul_v_Order4_Sel"
  };
  int columns = nSels;
  int rows = sizeof(resName) / sizeof(char*);
  int pads = rows*columns;

  // make the graph page
  can = new TCanvas(resName[1], resName[1], 600, 780);
  can->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now.AsString());
  date->Draw();
  TLine* lineZeroHar = new TLine(0.5, 0., nHars+0.5, 0.);
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(columns,rows);

  // make the plots
  float v;
  float err;
  for (int j = 0; j < rows; j++) {
    int resNumber = j;
    cout << "resolution name= " << resName[resNumber] << endl;
    for (int k = 0; k < columns; k++) {
      int padN = j*columns + k +1;
      TString* histName = new TString(resName[resNumber]);
      *histName += k+1;
      cout << "row= " << j << " col= " << k << " pad= " << padN << "\t" 
	   << histName->Data() << endl;
      TH1* hist = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return can;
      }
      graphPad->cd(padN);
      gStyle->SetOptStat(0);
      if (strstr(resName[resNumber],"_v")!=0) {
	hist->SetMaximum(10.);
	hist->SetMinimum(-5.);
      } else {
	hist->SetMaximum(1.1);
	hist->SetMinimum(0.);
      }
      for (int n=1; n < nHars+1; n++) {
	v   = hist->GetBinContent(n);                       // output v values
	err = hist->GetBinError(n);
	cout << " " << n << ": " << setprecision(3) << v << " +/- " << err << endl;
	if (TMath::IsNaN(v)) {
	  hist->SetBinContent(n, 0.);
	  hist->SetBinError(n, 0.);
	}
      }
      hist->Draw();
      lineZeroHar->Draw();
      delete histName;
    }
  }

  return can;
} 

void plotAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    can = plot(i, selN, harN);
    can->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) can->Print(".pdf");
    else if (strstr(tmp,"q")!=0) return;
    else if (strstr(tmp," ")!=0) continue;
  }
  cout << "  plotAll Done" << endl;
}

//-----------------------------------------------------------------------

static Double_t qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area, g

  double sig2 = 0.5 * (1. + par[3]);
  double expo = (par[1]*par[0]*par[0]/10000. + q[0]*q[0]) / (2*sig2);
  Double_t dNdq = par[2] * (q[0]*exp(-expo)/sig2) * 
    TMath::BesselI0(q[0]*par[0]/100.*sqrt(par[1])/sig2);

  return dNdq;
}

//-----------------------------------------------------------------------

static Double_t SubCorr(double* x, double* par) {
  // Calculates the n(Psi_a - Psi_b) distribution by fitting chi
  // From J.-Y. Ollitrault, Nucl. Phys. A590, 561c (1995), Eq. 6. with correc.

  double chi2 = par[0] * par[0] / 2;     // divide by two for SV chi
  double z = chi2 * cos(par[1]*x[0]);
  double TwoOverPi = 2./TMath::Pi();

  Double_t dNdPsi = exp(-chi2)/TwoOverPi * (TwoOverPi*(1.+chi2) 
                    + z*(TMath::BesselI0(z) + TMath::StruveL0(z))
                    + chi2*(TMath::BesselI1(z) + TMath::StruveL1(z)));

  return dNdPsi;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plot.C,v $
// Revision 1.70  2011/07/25 15:54:53  posk
// Added correction for non-flatness of event plane.
//
// Revision 1.69  2011/03/10 18:56:39  posk
// Added histogram for laboratory azimuthal distribution of particles.
//
// Revision 1.68  2010/09/30 19:28:23  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.67  2010/03/05 17:04:38  posk
// ROOT 5.22 compatable.
// Moved doFlowEvents.C here from StRoot/macros/analysis/
//
// Revision 1.66  2009/11/24 19:29:19  posk
// Added reCenter to remove acceptance correlations as an option instead of phiWgt.
//
// Revision 1.65  2006/02/22 19:35:18  posk
// Added graphs for the StFlowLeeYangZerosMaker
//
// Revision 1.64  2005/08/26 19:00:25  posk
// plot style back to bold
//
// Revision 1.63  2005/08/05 20:13:43  posk
// Improved first guess for qDist fit.
//
// Revision 1.62  2004/12/09 23:47:11  posk
// Minor changes in code formatting.
// Added hist for TPC primary dca to AnalysisMaker.
//
// Revision 1.61  2004/12/07 23:10:23  posk
// Only odd and even phiWgt hists. If the old phiWgt file contains more than
// two harmonics, only the first two are read. Now writes only the first two.
//
// Revision 1.60  2004/12/02 16:10:55  posk
// Added  gInterpreter->ProcessLine(".O0");
//
// Revision 1.59  2004/11/19 16:54:40  posk
// Replaced gPad with (TVirtualPad::Pad()). Reverted to TMath::Struve functions.
//
// Revision 1.58  2004/11/11 18:25:43  posk
// Minor updates.
//
// Revision 1.57  2004/03/11 18:00:05  posk
// Added Random Subs analysis method.
//
// Revision 1.56  2004/03/01 22:43:43  posk
// Changed some "->" to ".".
//
// Revision 1.55  2003/08/26 21:10:13  posk
// Calculates v8 if nHars=8.
//
// Revision 1.54  2003/07/30 22:03:26  oldi
// Fit("pol4") taken out (which was in accidentally).
//
// Revision 1.53  2003/07/07 21:58:21  posk
// Made units of momentum GeV/c instead of GeV.
//
// Revision 1.52  2003/06/27 21:25:44  posk
// v4 and v6 are with repect to the 2nd harmonic event plane.
//
// Revision 1.51  2003/05/06 21:33:07  posk
// Removed some histograms.
//
// Revision 1.50  2003/05/02 21:11:13  posk
// Reduced the number of harmonics from 3 to 2.
//
// Revision 1.49  2003/03/18 17:58:36  posk
// Kirill Fillimonov's improved fit to the angle between subevent planes.
//
// Revision 1.48  2003/03/17 20:46:55  posk
// Improved fit to q dist.
//
// Revision 1.47  2003/03/11 23:04:30  posk
// Includes scalar product hists.
//
// Revision 1.46  2003/02/25 19:25:32  posk
// Improved plotting.
//
// Revision 1.45  2003/02/05 18:52:50  posk
// Added Bool_t includeFtpc
//
// Revision 1.44  2003/01/16 16:02:30  posk
// Some plotting changes.
//
// Revision 1.43  2003/01/10 16:40:53  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.42  2002/11/26 22:11:54  posk
// First use of doxygen.
//
// Revision 1.41  2002/06/11 21:54:16  posk
// Kirill's further correction to minBias.C for bins with one count.
//
// Revision 1.40  2002/05/21 18:42:18  posk
// Kirill's correction to minBias.C for bins with one count.
//
// Revision 1.39  2002/02/13 22:31:50  posk
// Pt Weight now also weights Phi Weight. Added Eta Weught, default=FALSE.
//
// Revision 1.38  2002/01/14 23:42:57  posk
// Renamed ScalerProd histograms. Moved print commands to FlowMaker::Finish().
//
// Revision 1.37  2001/12/18 19:27:37  posk
// "proton" and "antiproton" replaced by "pr+" and "pr-".
//
// Revision 1.36  2001/12/11 22:04:13  posk
// Four sets of phiWgt histograms.
// StFlowMaker StFlowEvent::PhiWeight() changes.
// Cumulant histogram names changed.
//
// Revision 1.35  2001/11/13 22:47:35  posk
// Documentation updated. Fit to q function moved to macro.
//
// Revision 1.34  2001/11/09 21:15:04  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.33  2001/05/22 20:11:20  posk
// Changed dEdx graphs.
//
// Revision 1.32  2000/12/12 15:01:11  posk
// Put log comments at end of file.
//
// Revision 1.31  2000/12/08 17:04:09  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.27  2000/09/26 20:54:12  posk
// Updated documentation.
//
// Revision 1.26  2000/09/15 22:52:56  posk
// Added Pt weighting for event plane calculation.
//
// Revision 1.25  2000/08/31 18:50:32  posk
// Added plotCen.C to plot from a series of files with different centralities.
//
// Revision 1.24  2000/08/12 20:20:15  posk
// More centrality bins.
//
// Revision 1.23  2000/08/01 21:51:20  posk
// Added doubly integrated v.
//
// Revision 1.22  2000/07/12 17:49:39  posk
// Changed EtaSym plots.
//
// Revision 1.21  2000/06/30 14:51:20  posk
// Using MessageMgr. Added graph for Eta Symmetry vs. Vertex Z.
//
// Revision 1.20  2000/05/26 21:25:23  posk
// Use TProfile2D class and profile projection methods.
// Correction needed for >2 subevents.
//
// Revision 1.18  2000/04/13 22:34:16  posk
// Resolution correction is now made.
//
// Revision 1.17  2000/04/10 18:49:10  posk
// Asks for the histogram file number.
//
// Revision 1.16  2000/03/28 23:25:37  posk
// Allow multiple instances.
//
// Revision 1.15  2000/03/21 00:24:45  posk
// Added GetCVS and changed some plot names.
//
// Revision 1.12  2000/02/18 23:44:54  posk
// Added PID and centrality.
//
// Revision 1.11  2000/02/04 16:26:43  posk
// Added correct calculation of event plane resolution for large flow.
//
// Revision 1.2  1999/10/05 16:54:14  posk
// Added getPhiWeight method for making the event plane isotropic.
//
///////////////////////////////////////////////////////////////////////////////
