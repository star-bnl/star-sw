///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotMultiGraphFits.C,v 1.2 2003/01/24 23:10:25 posk Exp $
//
// Author:       Art Poskanzer and Alexander Wetzler, April 2002
// Description:
// Plot histograms for all 6 centralities averaged to 3
// and also minimum bias and also v as a function of centrality.
// Reads from pion and proton files produced by vProj.C.
// Divides canvas 2 x 2 or 1 x 2.
// Writes data to data.txt.
// Writes all TGraphsErrors objects to part_eBeam.root files.
// Does Blast Wave fits to v(pt).
//
///////////////////////////////////////////////////////////////////////////////

FILE* f    = fopen("data.txt","w");
FILE* fpar = fopen("par.txt", "w");
Double_t PI = TMath::Pi();
const Int_t nPars = 4;
Bool_t batch;

void plotMultiGraphFits(Int_t eBeam = 158) {

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();

  //Bool_t fixAll = kTRUE;
  Bool_t fixAll = kFALSE;

  batch = kTRUE;
  //batch = kFALSE;

  Bool_t crossSection = kTRUE;
  //Bool_t crossSection = kFALSE;

  // Blast Wave fit min. bias initial values
  if (eBeam == 158) {
    Double_t T       = 0.15;
    Double_t rho0Pi1 = 2.4;
    Double_t rho0Pi2 = 1.6;
    Double_t rho0Pr  = 0.65;
    Double_t rhoaPi1 = 0.01;
    Double_t rhoaPi2 = 0.06;
    Double_t rhoaPr1 = 0.05;
    Double_t rhoaPr2 = 0.05;
    Double_t s1      = 0.0;
    Double_t s2      = 0.05;
  } else {
    Double_t T       = 0.15;
    Double_t rho0Pi1 = 2.5;
    Double_t rho0Pi2 = 0.5;
    Double_t rho0Pr  = 0.6;
    Double_t rhoaPi1 = 0.02;
    Double_t rhoaPi2 = 0.04;
    Double_t rhoaPr1 = 0.04;
    Double_t rhoaPr2 = 0.06;
    Double_t s1      = 0.0;
    Double_t s2      = 0.05;
}

  // For min. bias fit results
  Double_t rho0MbPi1;
  Double_t rho0MbPi2;
  Double_t rho0MbPr1;
  Double_t rho0MbPr2;
  Double_t rhoaMbPi1;
  Double_t rhoaMbPi2;
  Double_t rhoaMbPr1;
  Double_t rhoaMbPr2;
  Double_t s1MbPi;
  Double_t s2MbPi;
  Double_t s2MbPr;

  func_v->SetParNames("T", "rho0", "rhoa", "s");
  fprintf(fpar, "Blast Wave fits: \n");
  TDatime now;
  fprintf(fpar, "%s \n\n", now->AsString());

  Bool_t TFix    = kTRUE;
  if (fixAll) {
    Bool_t rho0Fix = kTRUE;
    Bool_t rhoaFix = kTRUE;
    Bool_t sFix    = kTRUE;
  } else {
    Bool_t rho0Fix = kFALSE;
    Bool_t rhoaFix = kFALSE;
    Bool_t sFix    = kFALSE;
  }
  if (TFix) {func_v->FixParameter(0, T);}
  else {func_v->SetParLimits(0, 0.05, 0.2);}

  // The directory must exist
  if (crossSection)
    Char_t* outdir = "PconsPlots/";
  else
    Char_t* outdir = "PconsYieldPlots/";

  //Char_t pstype[255] = "ps";
  Char_t pstype[255] = "eps";
  //Char_t pstype[255] = "gif";

  const Int_t nFiles = 2;
  TFile*  file[nFiles];
  if (crossSection) {
    file[0] = new TFile("pionPcons.root", "READ");
    file[1] = new TFile("protonPcons.root", "READ");
  } else {
    file[0] = new TFile("pionPconsYield.root", "READ");
    file[1] = new TFile("protonPconsYield.root", "READ");
  }
  const Int_t nHar = 2;
  const Int_t nCen = 10;   // min. bias + six centralities + 3 averaged
  TH1F*    hist;
  Int_t    nYbins;
  Int_t    nYbinsMB;
  Int_t    nPtbins;
  Float_t  max;
  Float_t  min;
  Float_t  flip;
  Char_t*  centrality[3] = {"central", "mid-central", "peripheral"};
  Float_t  noPercent = 100.;
  Float_t  yCM;
  Char_t   title[255];
  Char_t   energyTitle[255];
  Char_t   outfile[255];
  Float_t  twoCol = 1.5;
  Double_t param[nPars];
  TOrdCollection *graphList = new TOrdCollection(26);


  if (eBeam == 158) {
    yCM = 2.92;
    Float_t E0[6] = {0.189, 0.315, 0.489, 0.646, 0.754, 0.855};
  } else if (eBeam == 40) {
    yCM =  2.24;
    Float_t E0[6]  = {0.119, 0.243, 0.410, 0.587, 0.720, 0.867};
  } else {
    cout << " Not valid beam energy" << endl;
    return;
  }

  gROOT->SetStyle("Bold");
  //gROOT->ForceStyle();
  gStyle->SetOptStat(kFALSE);
  gStyle->SetOptTitle(kFALSE);
  gStyle->SetLineWidth((gStyle->GetLineWidth())/twoCol);
  gStyle->SetHistLineColor(kBlack);
  gStyle->SetHistLineWidth(gStyle->GetLineWidth());
  gStyle->SetHistLineStyle(kDashed);
  gStyle->SetFrameLineWidth((gStyle->GetFrameLineWidth())/twoCol);
  Float_t markerSize  = 1.7/twoCol;
  Float_t textSize    = 0.06;
  Float_t commentSize = 0.05;
  Float_t energySize  = 0.07;

  char selText[2];
  char harText[2];
  char cenText[2];
  char polText[2];

  // Read input files and get Y, Pt, and V histograms
  TH1F *Y[nCen][nHar][nFiles];
  TH1F *Pt[nCen][nHar][nFiles];
  TH1F *V[nHar][nFiles];
  Float_t yMax;
  Float_t yMin;

  for (int fileN = 0; fileN < nFiles; fileN++) {
    if (fileN==0) {
      Bool_t pion = kTRUE;
      Char_t* part = "pion";
    } else {
      Bool_t pion = kFALSE;
      Char_t* part = "proton";
    }
    if (eBeam == 158) {
      if (pion) {
	yMax     = 4.9;
	yMin     = 2.85;
      } else {
	yMax     = 4.75;
	yMin     = 2.85;
      }
    } else {
      if (pion) {
	yMax     = 4.4;
	yMin     = 2.24;
      } else {
	yMax     = 4.2;
	yMin     = 2.24;
      }
    }
    for (Int_t j = 0; j < nHar; j++) {
      sprintf(harText, "%d", j + 1);
      for (Int_t i = 0; i < nCen; i++) {
	if (i==0 || i==7 || i==8 || i==9){ // min. bias + 6 cent. combined to 3
	  sprintf(cenText, "%d", i);
	  TString *histName = new TString("Flow_vY_Sel2_Har");
	  histName->Append(*harText);
	  histName->Append("_Cen");
	  histName->Append(*cenText);
	  Y[i][j][fileN] = (TH1F*)file[fileN]->Get(histName->Data());
	  Y[i][j][fileN]->Scale(1./noPercent);
	  if (eBeam == 158) {
	    if (!pion && i!=0) {
	      Y[i][j][fileN]->Rebin(4);
	      Y[i][j][fileN]->Scale(0.25);
	    } else if (!pion && j==1) {
	      Y[i][j][fileN]->Rebin(2);  
	      Y[i][j][fileN]->Scale(0.5);
	    } else {
	      Y[i][j][fileN]->Rebin(2);
	      Y[i][j][fileN]->Scale(0.5);
	    }
	  } else {
	    if (!pion && i!=0) {
	      Y[i][j][fileN]->Rebin(2);
	      Y[i][j][fileN]->Scale(0.5);
	    } else if (!pion && j==1) {
	      Y[i][j][fileN]->Rebin(2);  
	      Y[i][j][fileN]->Scale(0.5);
	    } else {
	      Y[i][j][fileN]->Rebin(2);
	      Y[i][j][fileN]->Scale(0.5);
	    }
	  }
	  delete histName;
	  TString *histName = new TString("Flow_vPt_Sel2_Har");
	  histName->Append(*harText);
	  histName->Append("_Cen");
	  histName->Append(*cenText);
	  Pt[i][j][fileN] = (TH1F*)file[fileN]->Get(histName->Data());
	  Pt[i][j][fileN]->Scale(1./noPercent);
	  if (!pion) {
// 	    Pt[i][j][fileN]->Rebin(8);
// 	    Pt[i][j][fileN]->Scale(0.125);
	    if (eBeam==158) {
	      Pt[i][j][fileN]->Rebin(4);
	      Pt[i][j][fileN]->Scale(0.25);
	    } else {
	      Pt[i][j][fileN]->Rebin(8);
	      Pt[i][j][fileN]->Scale(0.125);
	    }
	  } else if (eBeam != 158) {
	    Pt[i][j][fileN]->Rebin(2);
	    Pt[i][j][fileN]->Scale(0.5);
	  }
	  delete histName;
	}
      }
      TString *histName = new TString("Flow_v_Sel2_Har");
      histName->Append(*harText);
      V[j][fileN] = (TH1F*)file[fileN]->Get(histName->Data());
      V[j][fileN]->Scale(1./noPercent);
      delete histName;
    }
    nYbins   = Y[7][0][fileN]->GetNbinsX();
    nYbinsMB = Y[0][0][fileN]->GetNbinsX();
    nPtbins = Pt[7][0][fileN]->GetNbinsX();
    
    // Make graphs  
    // flowY[cen][har][reflected=1][fileN]
    TGraphErrors *flowY[nCen][nHar][2][nFiles];
    TGraphErrors *flowPt[nCen][nHar][2][nFiles];
    TGraphErrors *flowV[nHar][nFiles];
    for (Int_t j = 0; j < nHar; j++) {
      for (Int_t i = 0; i < nCen; i++) {
	if (i==0 || i==7 || i==8 || i==9){
	  for (Int_t k = 0; k < 2; k++) {
	    flowY[i][j][k][fileN] = new TGraphErrors();
	    flowPt[i][j][k][fileN] = new TGraphErrors();
	    sprintf(harText, "%d", j + 1);
	    sprintf(cenText, "%d", i);
	    sprintf(polText, "%d", k);
	    TString *histName = new TString("Flow_vY_Har");
	    histName->Append(*harText);
	    histName->Append("_Cen");
	    histName->Append(*cenText);
	    histName->Append("_Pol");
	    histName->Append(*polText);
	    flowY[i][j][k][fileN]->SetName(histName->Data());
	    delete histName;
	    if(!k) {
	      TString *histName = new TString("Flow_vPt_Har");
	      histName->Append(*harText);
	      histName->Append("_Cen");
	      histName->Append(*cenText);
	      flowPt[i][j][k][fileN]->SetName(histName->Data());
	      delete histName;
	    }
	  }
	}
      }
      flowV[j][fileN] = new TGraphErrors();
      TString *histName = new TString("Flow_v_Har");
      histName->Append(*harText);
      flowV[j][fileN]->SetName(histName->Data());
    }
    
    Float_t offsetY;
    Float_t offsetPt;
    for (Int_t i = 0; i < nCen; i++) {
      if (i==0 || i==7 || i==8 || i==9) {
	fprintf(f, "\n Centrality: %d \n", i);
	for (Int_t j = 0; j < nHar; j++) {
	  // Fill graphs with Y projection
	  if (!pion && i==0) {nYbins = nYbinsMB;}
	  flip = (j) ? 1. : -1.;
	  offsetY  = 0.03;
	  offsetPt = 0.01;
	  if (i==0) {
	    if (!j) {
	      offsetY  = -offsetY;
	      offsetPt = -offsetPt;
	    }
	  } else if (i==8) {
	    offsetY  = 0.;
	    offsetPt = 0.;
	  } else if (i==9) {
	    offsetY  = -offsetY;
	    offsetPt = -offsetPt;
	  }
	  for (Int_t m = 0; m < nYbins; m++) {
	    Int_t n = m + 1;
	    if (Y[i][j][fileN]->GetBinCenter(n) > yMin && 
		Y[i][j][fileN]->GetBinCenter(n) < yMax && 
		Y[i][j][fileN]->GetBinContent(n) != 0 &&
		Y[i][j][fileN]->GetBinError(n) < 20) {
	      flowY[i][j][0][fileN]->SetPoint(m, 
			   (Y[i][j][fileN]->GetBinCenter(n)) -yCM +offsetY,
			    flip * Y[i][j][fileN]->GetBinContent(n));
	      flowY[i][j][0][fileN]->SetPointError(m, 0., 
			    Y[i][j][fileN]->GetBinError(n));
	      flowY[i][j][1][fileN]->SetPoint(m, 
			yCM -offsetY - Y[i][j][fileN]->GetBinCenter(n),
				     Y[i][j][fileN]->GetBinContent(n));
	      flowY[i][j][1][fileN]->SetPointError(m, 0., 
				     Y[i][j][fileN]->GetBinError(n));
	    } else {
	      flowY[i][j][0][fileN]->SetPoint(m,-1000,0);
	      flowY[i][j][1][fileN]->SetPoint(m, 1000,0);
	    }
	  }
	  fprintf(f, "%s v%d versus rapidity\n", part, j+1);
	  PrintGraph(flowY[i][j][0][fileN]);
	  graphList->AddLast(flowY[i][j][0][fileN]);
	  graphList->AddLast(flowY[i][j][1][fileN]);
	  // Fill graphs with Pt projection  - - -  for protons
	  if (!pion) {
	    for (Int_t m = 0; m < nPtbins; m++) {
	      Int_t n = m + 1;
	      if (Pt[i][j][fileN]->GetBinError(n) < 40){
		flowPt[i][j][0][fileN]->SetPoint(m, 
			     Pt[i][j][fileN]->GetBinCenter(n) +offsetPt,
			     flip * Pt[i][j][fileN]->GetBinContent(n));
		flowPt[i][j][0][fileN]->SetPointError(m, 0., 
			      Pt[i][j][fileN]->GetBinError(n));
	      } else {
 		flowPt[i][j][0][fileN]->SetPoint(m,-1,0);
	      }
	    }
	  } else {
	    // Fill graphs with Pt projection  - - -  for pions
	    for (Int_t m = 0; m < 8; m++) {
	      Int_t n = m + 1;
	      if (Pt[i][j][fileN]->GetBinError(n) < 3){
		flowPt[i][j][0][fileN]->SetPoint(m, 
                               Pt[i][j][fileN]->GetBinCenter(n) +offsetPt,
			       flip * Pt[i][j][fileN]->GetBinContent(n));
		flowPt[i][j][0][fileN]->SetPointError(m, 0., 
					  Pt[i][j][fileN]->GetBinError(n));
	      } else {
		flowPt[i][j][0][fileN]->SetPoint(m,-1,0);
	      }
	    }	    
	    Pt[i][j][fileN]->Rebin();
	    Pt[i][j][fileN]->Scale(0.5);
	    for (Int_t m = 8; m < 16; m++) {
	      Int_t n = m/2 + 1;
	      if (!(m%2) && Pt[i][j][fileN]->GetBinError(n) < 12){
		flowPt[i][j][0][fileN]->SetPoint(m, 
                           Pt[i][j][fileN]->GetBinCenter(n) +offsetPt,
			   flip * Pt[i][j][fileN]->GetBinContent(n));
		flowPt[i][j][0][fileN]->SetPointError(m, 0., 
				      Pt[i][j][fileN]->GetBinError(n));
	      } else {
		flowPt[i][j][0][fileN]->SetPoint(m,-1,0);
	      }
	    }	    
	    Pt[i][j][fileN]->Rebin();
	    Pt[i][j][fileN]->Scale(0.5);
	    for (Int_t m = 16; m < 24; m++) {
	      Int_t n = m/4 + 1;
	      if (!(m%4) && Pt[i][j][fileN]->GetBinError(n) < 24){
		flowPt[i][j][0][fileN]->SetPoint(m, 
                              Pt[i][j][fileN]->GetBinCenter(n) +offsetPt,
			      flip * Pt[i][j][fileN]->GetBinContent(n));
		flowPt[i][j][0][fileN]->SetPointError(m, 0.,
				     Pt[i][j][fileN]->GetBinError(n));
	      } else {
		flowPt[i][j][0][fileN]->SetPoint(m,-1,0);
	      }
	    }
	    Pt[i][j][fileN]->Rebin();
	    Pt[i][j][fileN]->Scale(0.5);
	    for (Int_t m = 24; m < 40; m++) {
	      Int_t n = m/8 + 1;
	      if (!(m%8) && Pt[i][j][fileN]->GetBinError(n) < 48
		  && !(i==9 && j==1 && fileN==0 && n==5)) { // outlier
		flowPt[i][j][0][fileN]->SetPoint(m, 
                                   Pt[i][j][fileN]->GetBinCenter(n) +offsetPt,
				   flip * Pt[i][j][fileN]->GetBinContent(n));
		flowPt[i][j][0][fileN]->SetPointError(m, 0., 
				  Pt[i][j][fileN]->GetBinError(n));
	      } else {
		flowPt[i][j][0][fileN]->SetPoint(m,-1,0);
	      }
	    }
	  }    
	  fprintf(f, "%s v%d versus pt\n", part, j+1);
	  PrintGraph(flowPt[i][j][0][fileN]);
	  graphList->AddLast(flowPt[i][j][0][fileN]);
	}
      }
    }
    // Fill graphs with v doubly projected
    for (Int_t j = 0; j < nHar; j++) {
      flip = (j) ? 1. : -1.;
      for (Int_t m = 0; m < 6; m++) {
	Int_t n = m + 1;
	flowV[j][fileN]->SetPoint(m, E0[m] - 0.005 + 0.01*j,
				  flip * V[j][fileN]->GetBinContent(n));
	flowV[j][fileN]->SetPointError(m, 0., V[j][fileN]->GetBinError(n));
      }
      if (eBeam != 158 && j == 1) { // don't plot one point
	flowV[j][fileN]->SetPoint(0, -1., 0.);
      }
      fprintf(f, "%s v%d doubly integrated\n", part, j+1);
      PrintGraph(flowV[j][fileN]);
      graphList->AddLast(flowV[j][fileN]);
    }
  }
  
  // Save graphs to file
  Char_t rootFileName[255];
  sprintf(rootFileName, "%d.root", eBeam);
  TFile *graphFile = new TFile(rootFileName,"RECREATE");
  graphFile->cd();
  graphList->Write();
  graphFile->Close();
  delete graphList;
  delete graphFile;

  // Save data to file
  fclose(f);
  
  // Create Canvas
  TCanvas *canvas = new TCanvas("Flow", "Flow", 100, 100, 840, 600);
  canvas->Divide(2,2,0.,0.);
  TLegend *legend = new TLegend(0.25,0.7,0.52,0.9);
  legend->SetFillColor(kWhite);
  TLatex l; 
  l.SetNDC();
  l.SetTextColor(kBlue); 
  l.SetTextSize(textSize); 
      
  // Define fitting functions
  Float_t yMaxPi;
  Float_t yMinPi;
  Float_t yReflMaxPi;
  Float_t yReflMinPi;
  Float_t yMaxPr;
  Float_t yMinPr;
  Float_t yReflMaxPr;
  Float_t yReflMinPr;
  Float_t yAllMaxPr;
  Float_t yAllMaxPi;
  Float_t yAllReflMin;
  if (eBeam == 158) {
    yMaxPi      = 2.05;
    yMinPi      = -0.1;
    yReflMaxPi  = 0.1;
    yReflMinPi  = -2.05;
    yMaxPr      = 1.75;
    yMinPr      = 0.;
    yReflMaxPr  = 0.;
    yReflMinPr  = -1.75;
    yAllMaxPr   = 1.55;
    yAllMaxPi   = 2.1;
    yAllReflMin = -1.55;
  } else {
    yMaxPi      = 4.0  - yCM;
    yMinPi      = 0.;
    yReflMaxPi  = 0.;
    yReflMinPi  = 0.5  - yCM;
    yMaxPr      = 3.95 - yCM;
    yMinPr      = 0.;
    yReflMaxPr  = 0.;
    yReflMinPr  = 0.5  - yCM;
    yAllMaxPr   = 4.0  - yCM;
    yAllMaxPi   = 4.0  - yCM;
    yAllReflMin = 0.5  - yCM;
  }
    
  TF1* ptL1 = new TF1("ptL1", "[0]*(x)", 0.0, 2.0);
  TF1* ptL2 = new TF1("ptL2", "[0]*(x) + [1]*pow(x,2)", 0.0, 2.0);

  TF1* f1Pi = new TF1("f1Pi", "[0] + [1]*(x)", yMinPi, yMaxPi);
  TF1* f2Pi = new TF1("f2Pi", "[0] + [1]*(x) + [2]*pow(x,3)", 
		      yMinPi, yMaxPi);
  TF1* f3Pi = new TF1("f3Pi",
		      "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", yMinPi, yMaxPi);
  TF1* s1Pi = new TF1("s1Pi", "[0] + [1]*pow(x,2)", yMinPi, yMaxPi);
  TF1* s2Pi = new TF1("s2Pi", "[0] + [1]*pow(x,2) + [2]*pow(x,4)", 
		      yMinPi, yMaxPi);
  
  TF1* r1Pi = new TF1("r1Pi", "[0] + [1]*pow(x,2)", yReflMinPi, yReflMaxPi);
  TF1* r2Pi = new TF1("r2Pi", "[0] + [1]*pow(x,2) + [2]*pow(x,4)",
		      yReflMinPi, yReflMaxPi);
  TF1* r3Pi = new TF1("r3Pi",
		      "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", 
		      yReflMinPi, yReflMaxPi);
  
  TF1* f3AllPi = new TF1("f3AllPi",
			 "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", yMinPi, yAllMaxPi);
  TF1* s2AllPi = new TF1("s2AllPi", "[0] + [1]*pow(x,2) + [2]*pow(x,4)", 
			 yMinPi, yAllMaxPi);
  TF1* r2AllPi = new TF1("r2AllPi", "[0] + [1]*pow(x,2) + [2]*pow(x,4)",
			 yAllReflMin, yReflMaxPi);
  TF1* r3AllPi = new TF1("r3AllPi",
			 "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", 
			 yAllReflMin, yReflMaxPi);
  TF1* f1Pr = new TF1("f1Pr", "[0] + [1]*(x)", yMinPr, yMaxPr);
  TF1* f2Pr = new TF1("f2Pr", "[0] + [1]*(x) + [2]*pow(x,3)", 
		      yMinPr, yMaxPr);
  TF1* f3Pr = new TF1("f3Pr",
		      "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", yMinPr, yMaxPr);
  
  TF1* s1Pr = new TF1("s1Pr", "[0] + [1]*pow(x,2)", yMinPr, yMaxPr);
  TF1* s2Pr = new TF1("s2Pr", "[0] + [1]*pow(x,2) + [2]*pow(x,4)", 
		      yMinPr, yMaxPr);
  
  TF1* r1Pr = new TF1("r1Pr", "[0] + [1]*pow(x,2)", yReflMinPr, yReflMaxPr);
  TF1* r2Pr = new TF1("r2Pr", "[0] + [1]*pow(x,2) + [2]*pow(x,4)",
		      yReflMinPr, yReflMaxPr);
  TF1* r3Pr = new TF1("r3Pr",
		      "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", 
		      yReflMinPr, yReflMaxPr);
  
  TF1* f3AllPr = new TF1("f3AllPr",
			 "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", yMinPr, yAllMaxPr);
  TF1* s2AllPr = new TF1("s2AllPr", "[0] + [1]*pow(x,2) + [2]*pow(x,4)", 
			 yMinPr, yAllMaxPr);
  TF1* r2AllPr = new TF1("r2AllPr", "[0] + [1]*pow(x,2) + [2]*pow(x,4)",
			 yAllReflMin, yReflMaxPr);
  TF1* r3AllPr = new TF1("r3AllPr",
			 "[0]*(x) + [1]*pow(x,3) + [2]*pow(x,5)", 
			 yAllReflMin, yReflMaxPr);
  // centrality polynomials
  TF1* c1 = new TF1("c1", "pol2", 0.18, 0.87);
  TF1* c2 = new TF1("c2", "pol3", 0.18, 0.87);
  TF1* c3 = new TF1("c3", "pol4", 0.18, 0.87);
  TF1* d1 = new TF1("d1", "pol2", 0.31, 0.87);
  TF1* d2 = new TF1("d2", "pol3", 0.23, 0.88);
  TF1* d3 = new TF1("d3", "pol4", 0.31, 0.87);
  TF1* e2 = new TF1("e2", "pol3", 0.10, 0.88);
    
  // Draw Graphs

  // DrawClones of min. bias pt graphs
  TGraphErrors* flowPtPi1;
  TGraphErrors* flowPtPr1;
  TGraphErrors* flowPtPi2;
  TGraphErrors* flowPtPr2;

  for (int fileN = 0; fileN < nFiles; fileN++) {
    if (fileN==0) {
      Char_t* part = "pion";
      Bool_t pion = kTRUE;
    } else {
      Char_t* part = "proton";
      Bool_t pion = kFALSE;
    }
    // Minimum Bias vs. y --------------------------------------------
    canvas->cd(1+fileN);
    flowY[0][0][0][fileN]->SetTitle(strcat(title, part));
    flowY[0][0][0][fileN]->SetMarkerStyle(kFullCircle);
    flowY[0][0][1][fileN]->SetMarkerStyle(kOpenCircle);
    flowY[0][1][0][fileN]->SetMarkerStyle(kFullSquare);
    flowY[0][1][1][fileN]->SetMarkerStyle(kOpenSquare);
    flowY[0][0][0][fileN]->SetMarkerColor(kRed);
    flowY[0][0][1][fileN]->SetMarkerColor(kRed);
    flowY[0][1][0][fileN]->SetMarkerColor(kGreen);
    flowY[0][1][1][fileN]->SetMarkerColor(kGreen);
    flowY[0][0][0][fileN]->SetLineColor(kRed);
    flowY[0][0][1][fileN]->SetLineColor(kRed);
    flowY[0][1][0][fileN]->SetLineColor(kGreen);
    flowY[0][1][1][fileN]->SetLineColor(kGreen);
    flowY[0][0][0][fileN]->SetMarkerSize(markerSize);
    flowY[0][0][1][fileN]->SetMarkerSize(markerSize);
    flowY[0][1][0][fileN]->SetMarkerSize(markerSize);
    flowY[0][1][1][fileN]->SetMarkerSize(markerSize);
    
    sprintf(title, "%s Minimum Bias y", part);
    if (eBeam == 158) {
      hist = new TH1F(title, title, 10, -2.1, 2.1);
      if (pion) {
	max = 4.;
	min = -4.;
      } else {
	max = 4.;
	min = -3.;
      }
    } else {
      hist = new TH1F(title, title, 10, -2.1, 2.1);
      if (pion) {
	max = 6.;
	min = -6.;
      } else {
	max = 20.;
	min = -20.;
      }
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    cout << endl << "##### " << part << " v1(y) min. bias" << endl << endl;
    if (pion) {
      f3Pi->SetLineColor(kRed);
      r3Pi->SetLineColor(kRed);
      flowY[0][0][0][fileN]->Fit("f3Pi", "R");
      flowY[0][0][1][fileN]->Fit("r3Pi", "R");
    } else {
      f3Pr->SetLineColor(kRed);
      r3Pr->SetLineColor(kRed);
      flowY[0][0][0][fileN]->Fit("f3Pr", "R");
      flowY[0][0][1][fileN]->Fit("r3Pr", "R");
    }
    flowY[0][0][0][fileN]->Draw("P");
    flowY[0][0][1][fileN]->Draw("P");
    cout << endl << "##### " << part << " v2(y) min. bias" << endl << endl;
    if (eBeam == 158) {
      if (pion) {
	flowY[0][1][0][fileN]->Fit("s2Pi", "R");
	flowY[0][1][1][fileN]->Fit("r2Pi", "R");
      } else {
	flowY[0][1][0][fileN]->Fit("s1Pr", "R");
	flowY[0][1][1][fileN]->Fit("r1Pr", "R");
      }
    } else {
      if (pion) {
	flowY[0][1][0][fileN]->Fit("s2Pi", "R");
	flowY[0][1][1][fileN]->Fit("r2Pi", "R");
      } else {
	flowY[0][1][0][fileN]->Fit("s2Pr", "R");
	flowY[0][1][1][fileN]->Fit("r2Pr", "R");
      }
    }
    flowY[0][1][0][fileN]->Draw("P");
    flowY[0][1][1][fileN]->Draw("P");
    
    TLine* lineYcm = new TLine(0., min/noPercent, 0., max/noPercent);
    lineYcm->SetLineStyle(kDashed);
    lineYcm->Draw();
    
    l.SetTextColor(kBlue); 
    l.SetTextAngle(90); 
    ordTitle = new TString(" flow");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.03,"rapidity" );
    
    l.SetTextSize(commentSize); 
    l.DrawLatex(0.2,0.2,"p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d A#bulletGeV", eBeam);
      l.SetTextSize(energySize); 
      l.DrawLatex(0.65,0.82,energyTitle);
    }
    l.SetTextSize(textSize); 

    l.SetTextColor(kRed); 
    if (eBeam == 158) {
      if (pion) {
	l.DrawLatex(0.7,0.25,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.82,"v_{2}"); 
      } else {
	l.DrawLatex(0.8,0.53,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.62,0.75,"v_{2}"); 
      }
    } else {
      if (pion) {
	l.DrawLatex(0.7,0.28,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.77,"v_{2}"); 
      } else {
	l.DrawLatex(0.73,0.72,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.75,0.47,"v_{2}"); 
      }
    }
    
    // Minimum Bias versus pt -----------------------------------------
    canvas->cd(3+fileN);
    flowPt[0][0][0][fileN]->SetTitle(strcat(title, part));
    flowPt[0][0][0][fileN]->SetMarkerStyle(kFullCircle);
    flowPt[0][1][0][fileN]->SetMarkerStyle(kFullSquare);
    flowPt[0][0][0][fileN]->SetMarkerColor(kRed);
    flowPt[0][1][0][fileN]->SetMarkerColor(kGreen);
    flowPt[0][0][0][fileN]->SetLineColor(kRed);
    flowPt[0][1][0][fileN]->SetLineColor(kGreen);
    flowPt[0][0][0][fileN]->SetLineWidth(gStyle->GetLineWidth());
    flowPt[0][1][0][fileN]->SetLineWidth(gStyle->GetLineWidth());
    flowPt[0][0][0][fileN]->SetMarkerSize(markerSize);
    flowPt[0][1][0][fileN]->SetMarkerSize(markerSize);
    
    sprintf(title, "%s Minimum Bias pt", part);
    hist = new TH1F(title, title, 10, 0., 2.);
    if (eBeam == 158) {
      if (pion) {
	max = 10.;
	min = -3.;
      } else {
	max = 10.;
	min = -3.;
      }
    } else {
      if (pion) {
	max = 15.;
	min = -5.;
      } else {
	max = 15.;
	min = -3.;
      }
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    
    if (pion) {
      cout << endl << "##### " << part << " v1(pt) min. bias" << endl << endl;
      func_v->SetLineColor(kRed);
      func_bw->FixParameter(1, 0.14);   // mass
      func_bw->FixParameter(7, 1.);     // order
      func_v->SetParameters(T, rho0Pi1, rhoaPi1, s1);
      if (rho0Fix) {func_v->FixParameter(1, rho0Pi1);}
      if (rhoaFix) {func_v->FixParameter(2, rhoaPi1);}
      if (sFix)    {func_v->FixParameter(3, s1);}
      flowPt[0][0][0][fileN]->Fit("func_v", "R");
      flowPtPi1 = (TGraphErrors*)flowPt[0][0][0][fileN]->DrawClone("P");
      fprintf(fpar, "%s v1 min bias\n", part);
      PrintFits();
      cout << endl << "##### " << part << " v2(pt) min. bias" << endl << endl;
      func_v->SetLineColor(kGreen);
      func_bw->FixParameter(7, 2.);     // order
      func_v->SetParameters(T, rho0Pi2, rhoaPi2, s2);
      if (rho0Fix) {func_v->FixParameter(1, rho0Pi2);}
      if (rhoaFix) {func_v->FixParameter(2, rhoaPi2);}
      if (sFix)    {func_v->FixParameter(3, s2);}
      flowPt[0][1][0][fileN]->Fit("func_v", "R");
      flowPtPi2 = (TGraphErrors*)flowPt[0][1][0][fileN]->DrawClone("P");
      fprintf(fpar, "%s v2 min bias\n", part);
      PrintFits();
    } else {
      cout << endl << "##### " << part << " v1(pt) min. bias" << endl << endl;
      func_v->SetLineColor(kRed);
      func_bw->FixParameter(1, 0.94);   // mass
      func_bw->FixParameter(7, 1.);     // order
      func_v->SetParameters(T, rho0Pr, rhoaPr1, 0.);
      if (rho0Fix) {func_v->FixParameter(1, rho0Pr);}
      if (rhoaFix) {func_v->FixParameter(2, rhoaPr1);}
      func_v->FixParameter(3, 0.);      // s1=0
      fprintf(fpar, "%s v1 min bias\n", part);
      if (eBeam == 158) {
	ptL1->SetLineColor(kRed);
	flowPt[0][0][0][fileN]->Fit("ptL1", "R");
	flowPtPr1 = (TGraphErrors*)flowPt[0][0][0][fileN]->DrawClone("P");
      } else {
	flowPt[0][0][0][fileN]->Fit("func_v", "R");
	flowPtPr1 = (TGraphErrors*)flowPt[0][0][0][fileN]->DrawClone("P");
	PrintFits();
      }
      cout << endl << "##### " << part << " v2(pt) min. bias" << endl << endl;
      func_v->SetLineColor(kGreen);
      func_bw->FixParameter(7, 2.);     // order
      func_v->SetParameters(T, rho0Pr, rhoaPr2, 0.);
      if (rho0Fix) {func_v->FixParameter(1, rho0Pr);}
      if (rhoaFix) {func_v->FixParameter(2, rhoaPr2);}
      func_v->FixParameter(3, 0.);      // s2=0
      flowPt[0][1][0][fileN]->Fit("func_v", "R");
      flowPtPr2 = (TGraphErrors*)flowPt[0][1][0][fileN]->DrawClone("P");
      fprintf(fpar, "%s v2 min bias\n", part);
      PrintFits();
      func_v->ReleaseParameter(3);
    }
    
    l.SetTextColor(kBlue); 
    l.SetTextAngle(90); 
    ordTitle = new TString(" flow");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.03,"p_{t} (GeV/c)" ); 
    
    if (eBeam == 158) {
      l.SetTextSize(commentSize); 
      l.DrawLatex(0.2,0.8,"0. < y < 2.1");
      l.SetTextSize(textSize); 
      l.SetTextColor(kRed); 
      if (pion) {
	l.DrawLatex(0.7,0.37,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.7,"v_{2}");
      } else {
	l.DrawLatex(0.8,0.4,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.75,0.85,"v_{2}");
      }
    } else {
      l.SetTextSize(commentSize); 
      l.DrawLatex(0.2,0.8,"0. < y < 1.8");
      l.SetTextSize(textSize); 
      l.SetTextColor(kRed); 
      if (pion) {
	l.DrawLatex(0.75,0.45,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.75,0.73,"v_{2}");
      } else {
	l.DrawLatex(0.6,0.62,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.6,0.4,"v_{2}");
      }
    }
  }

  sprintf(outfile, "%s%d_mb.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  
  // Create new canvas
  delete canvas;
  TCanvas *canvas = new TCanvas("Flow", "Flow", 100, 100, 840, 600);
  canvas->Divide(2,2,0.,0.);

  for (int fileN = 0; fileN < nFiles; fileN++) {
    if (fileN==0) {
      Char_t* part = "pion";
      Bool_t pion = kTRUE;
    } else {
      Char_t* part = "proton";
      Bool_t pion = kFALSE;
    }

    // Set Marker Types
    for (Int_t m = 0; m < 3; m++) {
      int i = m + 7;
      for (Int_t j = 0; j < 2; j++) {
	for (Int_t k = 0; k < 2; k++) {
	  flowY[i][j][k][fileN]->SetMarkerColor(m+2);
	  flowY[i][j][k][fileN]->SetLineColor(m+2);
	  flowY[i][j][k][fileN]->SetMarkerSize(markerSize);
	  flowY[i][j][k][fileN]->SetMarkerStyle(20+(m%3)+4*(k%2));
	  flowPt[i][j][k][fileN]->SetMarkerColor(m+2);
	  flowPt[i][j][k][fileN]->SetLineColor(m+2);
	  flowPt[i][j][k][fileN]->SetLineWidth(gStyle->GetLineWidth());
	  flowPt[i][j][k][fileN]->SetMarkerSize(markerSize);
	  flowPt[i][j][k][fileN]->SetMarkerStyle(20+(m%3)+4*(k%2));
	}
      }
    }
    
    // Harmonic 1 vs. y three Centralities ---------------------------
    canvas->cd(1+fileN);
    sprintf(title, "%s v1 y", part);
    flowY[7][0][0][fileN]->SetTitle(strcat(title, part));

    if (eBeam == 158) {
      hist = new TH1F(title, title, 10, 0.8-yCM, 5.-yCM);
      if (pion) {
	max = 7.;
	min = -7.;
      } else {
	max = 4.;
	min = -4.;
      }
    } else {
      hist = new TH1F(title, title, 10, 0.-yCM, 4.5-yCM);
      if (pion) {
	max = 15.;
	min = -15.;
      } else {
	max = 25.;
	min = -25.;
      }
    }  
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    for (Int_t i = 7; i <= 9; i++) {
      cout << endl << "##### " << part << " v1(y) cent " << i-6 << endl << endl;
      if (pion) {
	f3Pi->SetLineColor(i-5);
	r3Pi->SetLineColor(i-5);
	flowY[i][0][0][fileN]->Fit("f3Pi", "R");
	flowY[i][0][0][fileN]->Draw("P");
	flowY[i][0][1][fileN]->Fit("r3Pi", "R");
	flowY[i][0][1][fileN]->Draw("P");
      } else {
	f3AllPr->SetLineColor(i-5);
	r3AllPr->SetLineColor(i-5);
	flowY[i][0][0][fileN]->Fit("f3AllPr", "R");
	flowY[i][0][0][fileN]->Draw("P");
	flowY[i][0][1][fileN]->Fit("r3AllPr", "R");
	flowY[i][0][1][fileN]->Draw("P");
      }
    }
        
    TLine* lineYcm = new TLine(0., min/noPercent, 0., max/noPercent);
    lineYcm->SetLineStyle(kDashed);
    lineYcm->Draw();
    
    l.SetTextColor(kBlue); 
    l.SetTextAngle(90); 
    ordTitle = new TString(" v_{1}");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0);
    l.DrawLatex(0.7,0.03,"rapidity" ); 

    l.SetTextSize(commentSize); 
    l.DrawLatex(0.3,0.8,"p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d A#bulletGeV", eBeam);
      l.SetTextSize(energySize); 
      l.DrawLatex(0.55,0.82,energyTitle);
    }
    l.SetTextSize(textSize); 
        
    // Harmonic 1 versus pt three Centralities -------------------
    canvas->cd(3+fileN);
    sprintf(title, "%s v1 pt", part);
    flowPt[7][0][0][fileN]->SetTitle(strcat(title, part));
    hist = new TH1F(title, title, 10, 0., 2.);
    
    if (eBeam == 158) {
      if (pion) {
	max = 4.;
	min = -5.;
      } else {
	max = 5.;
	min = -6.;
      }
    } else {
      if (pion) {
	max = 15.;
	min = -7.;
      } else {
	max = 15.;
	min = -2.;
      }
    }    
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    func_bw->FixParameter(7, 1.);              // order
    rho0MbPi1 = flowPtPi1->GetFunction("func_v")->GetParameter(1);
    rhoaMbPi1 = flowPtPi1->GetFunction("func_v")->GetParameter(2);
    s1MbPi    = flowPtPi1->GetFunction("func_v")->GetParameter(3);
    if (eBeam == 158) {
      rho0MbPr1 = rho0Pr;
      rhoaMbPr1 = rhoaPr1;
    } else { 
      rho0MbPr1 = flowPtPr1->GetFunction("func_v")->GetParameter(1);
      rhoaMbPr1 = flowPtPr1->GetFunction("func_v")->GetParameter(2);
    }
    TGraphErrors* flowPtPi1Cen[3];
    TGraphErrors* flowPtPr1Cen[3]; 
    for (Int_t i = 7; i <= 9; i++) {
      func_v->SetLineColor(i-5);
      if (pion) {
	cout << endl << "##### " << part << " v1(pt) cent " << i-6 << endl << endl;
	fprintf(fpar, "%s v1 cent %.0f \n", part, i-6);
	if ((i==9 && eBeam==158) || (i==7 && eBeam==40)) {
	  flowPtPi1Cen[i-7] = 
	    (TGraphErrors*)flowPt[i][0][0][fileN]->DrawClone("PC");
	} else {
	  func_bw->FixParameter(1, 0.14);        // mass
	  func_v->SetParameters(T, rho0MbPi1, rhoaMbPi1, s1MbPi);
	  if (rho0Fix) {func_v->FixParameter(1, rho0MbPi1);}
	  if (rhoaFix) {func_v->FixParameter(2, rhoaMbPi1);}
	  if (sFix)    {func_v->FixParameter(3, s1MbPi);}
	  if (eBeam != 158) {func_v->FixParameter(3, 0.);} // s1= 0
	  flowPt[i][0][0][fileN]->Fit("func_v", "R");
	  flowPtPi1Cen[i-7] = 
	    (TGraphErrors*)flowPt[i][0][0][fileN]->DrawClone("P");
	  PrintFits();
	}
      } else {
	cout << endl << "##### " << part << " v1(pt) cent " << i-6 << endl << endl;
	func_bw->FixParameter(1, 0.94);        // mass
	func_v->SetParameters(T, rho0MbPr1, rhoaMbPr1, 0.);
	if (eBeam == 158) {
	  if (rho0Fix) {func_v->FixParameter(1, rho0MbPr1);} // fix to min. bias
	} else {func_v->FixParameter(1, 0.);}  // rho0= 0
	if (rhoaFix) {func_v->FixParameter(2, rhoaMbPr1);}
	func_v->FixParameter(3, 0.);           // s1= 0
	fprintf(fpar, "%s v1 cent %.0f \n", part, i-6);
	flowPt[i][0][0][fileN]->Fit("func_v", "R");
	flowPtPr1Cen[i-7] = 
	  (TGraphErrors*)flowPt[i][0][0][fileN]->DrawClone("P");
	PrintFits();
	func_v->ReleaseParameter(1);
	func_v->ReleaseParameter(3);
      }
    }
    
    Int_t padN = gPad->GetNumber();
    if (padN == 4) {
      for (Int_t i = 7; i <= 9; i++){
	legend->AddEntry(flowPt[i][0][0][fileN], centrality[i-7], "P");
      }
      legend->Draw();
    }
    
    l.SetTextAngle(90); 
    ordTitle = new TString(" v_{1}");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.03,"p_{t} (GeV/c)" ); 
    
    l.SetTextSize(commentSize); 
    if (eBeam == 158) {
      l.DrawLatex(0.65,0.2,"0. < y < 2.1");
    } else {
      l.DrawLatex(0.65,0.18,"0. < y < 1.8");
    }
    l.SetTextSize(textSize); 
  }
  sprintf(outfile, "%s%d_v1_all.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;
  
  // Create new canvas
  delete lineYcm;
  delete canvas;
  TCanvas *canvas = new TCanvas("Flow", "Flow", 100, 100, 840, 600);
  canvas->Divide(2,2,0.,0.);

  for (int fileN = 0; fileN < nFiles; fileN++) {
    if (fileN==0) {
      Char_t* part = "pion";
      Bool_t pion = kTRUE;
    } else {
      Char_t* part = "proton";
      Bool_t pion = kFALSE;
    }
    
    // Harmonic 2 vs. y three Centralities ---------------------------------
    canvas->cd(1+fileN);
    sprintf(title, "%s v2 y", part);
    flowY[7][1][0][fileN]->SetTitle(strcat(title, part));

    if (eBeam == 158) {  
      hist = new TH1F(title, title, 10, 0.8-yCM, 5.-yCM);
      if (pion) {
	max = 5.;
	min = 0.;
      } else {
	max = 8.;
	min = -3.;
      }
    } else {
      hist = new TH1F(title, title, 10, 0.-yCM, 4.5-yCM);
      if (pion) {
	max = 10.;
	min = -5.;
      } else {
	max = 15.;
	min = -15.;
      }
    }    
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    for (Int_t i = 7; i <= 9; i++) {
      cout << endl << "##### " << part << " v1(y) cent " << i-6 << endl << endl;
      if (pion) {
	s2Pi->SetLineColor(i-5);
	r2Pi->SetLineColor(i-5);
	flowY[i][1][0][fileN]->Fit("s2Pi", "R");
	flowY[i][1][0][fileN]->Draw("P");
	flowY[i][1][1][fileN]->Fit("r2Pi", "R");
	flowY[i][1][1][fileN]->Draw("P");
      } else {
	s2AllPr->SetLineColor(i-5);
	r2AllPr->SetLineColor(i-5);
	flowY[i][1][0][fileN]->Fit("s2AllPr", "R");
	flowY[i][1][0][fileN]->Draw("P");
	flowY[i][1][1][fileN]->Fit("r2AllPr", "R");
	flowY[i][1][1][fileN]->Draw("P");
      }
    }
        
    TLine* lineYcm = new TLine(0., min/noPercent, 0., max/noPercent);
    lineYcm->SetLineStyle(kDashed);
    lineYcm->Draw();
    
    l.SetTextAngle(90); 
    ordTitle = new TString(" v_{2}");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.03,"rapidity" ); 
        
    l.SetTextSize(commentSize); 
    l.DrawLatex(0.6,0.2,"p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d A#bulletGeV", eBeam);
      l.SetTextSize(energySize); 
      l.DrawLatex(0.65,0.82,energyTitle);
    }
    l.SetTextSize(textSize); 
    
    // Harmonic 2 versus pt three Centralities -------------------------------
    canvas->cd(3+fileN);
    sprintf(title, "%s v2 pt", part);
    flowPt[7][1][0][fileN]->SetTitle(strcat(title, part));
    hist = new TH1F(title, title, 10, 0., 2.);
    if (eBeam == 158) {
      if (pion) {
	max = 25.;
	min = -2.;
      } else {
	max = 25.;
	min = -5.;
      }
    } else {
      if (pion) {
	max = 30.;
	min = -5.;
      } else {
	max = 15.;
	min = -7.;
      }
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    func_bw->FixParameter(7, 2.);         // order
    rho0MbPi2 = flowPtPi2->GetFunction("func_v")->GetParameter(1);
    rhoaMbPi2 = flowPtPi2->GetFunction("func_v")->GetParameter(2);
    s2MbPi    = flowPtPi2->GetFunction("func_v")->GetParameter(3);
    rho0MbPr2 = flowPtPr2->GetFunction("func_v")->GetParameter(1);
    rhoaMbPr2 = flowPtPr2->GetFunction("func_v")->GetParameter(2);
    s2MbPr    = flowPtPr2->GetFunction("func_v")->GetParameter(3);
    TGraphErrors* flowPtPi2Cen[3]; 
    TGraphErrors* flowPtPr2Cen[3]; 
    for (Int_t i = 7; i <= 9; i++) {
      func_v->SetLineColor(i-5);
      if (pion) {
	cout << endl << "##### " << part << " v2(pt) cent " << i-6 << endl << endl;
	func_bw->FixParameter(1, 0.14);   // mass
	func_v->SetParameters(T, rho0MbPi2, rhoaMbPi2, s2MbPi);
	if (rho0Fix) {func_v->FixParameter(1, rho0MbPi2);} 
	if (rhoaFix) {func_v->FixParameter(2, rhoaMbPi2);} 
	if (eBeam == 158) {if (sFix) {func_v->FixParameter(3, s2MbPi);}
	} else {func_v->FixParameter(3, 0.);}        // s2= 0
	flowPt[i][1][0][fileN]->Fit("func_v", "R");
	flowPtPi2Cen[i-7] = 
	  (TGraphErrors*)flowPt[i][1][0][fileN]->DrawClone("P");
	fprintf(fpar, "%s v2 cent %.0f \n", part, i-6);
	PrintFits();
      } else {
	cout << endl << "##### " << part << " v2(pt) cent " << i-6 << endl << endl;
	func_bw->FixParameter(1, 0.94);   // mass
	func_v->SetParameters(T, rho0MbPr2, rhoaMbPr2, 0.);
	if (rho0Fix) {func_v->FixParameter(1, rho0MbPr2);}
	if (rhoaFix) {func_v->FixParameter(2, rhoaMbPr2);}
	func_v->FixParameter(3, 0.);      // s2= 0
	flowPt[i][1][0][fileN]->Fit("func_v", "R");
	flowPtPr2Cen[i-7] = 
	  (TGraphErrors*)flowPt[i][1][0][fileN]->DrawClone("P");
	fprintf(fpar, "%s v2 cent %.0f \n", part, i-6);
	PrintFits();
	func_v->ReleaseParameter(3);
      }
    }
    
    Int_t padN = gPad->GetNumber();
    if (padN == 4) {
      legend->Clear();
      legend->SetX1NDC(0.25);
      legend->SetY1NDC(0.6);    
      legend->SetX2NDC(0.52);
      legend->SetY2NDC(0.8);
      for (Int_t i = 7; i <= 9; i++){
	legend->AddEntry(flowPt[i][1][0][fileN], centrality[i-7], "P");
      }
      legend->Draw();
    }

    l.SetTextAngle(90); 
    ordTitle = new TString(" v_{2}");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.03,"p_{t} (GeV/c)" ); 
    
    l.SetTextSize(commentSize); 
    if (eBeam == 158) {
      l.DrawLatex(0.22,0.82,"0. < y < 2.1");
    } else {
      l.DrawLatex(0.22,0.82,"0. < y < 1.8");
    }
    l.SetTextSize(textSize); 
  }

  sprintf(outfile, "%s%d_v2_all.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;
  
  // Create new canvas
  delete lineYcm;
  delete canvas;
  TCanvas *canvas = new TCanvas("Flow", "Flow", 100, 100, 600, 780);
  canvas->Divide(1,2,0.,0.);

  markerSize *= twoCol;
  Float_t margin = 0.15;  
  gStyle->SetFrameLineWidth((gStyle->GetFrameLineWidth()*twoCol));
  gStyle->SetLineWidth((gStyle->GetLineWidth())*twoCol);

  for (int fileN = 0; fileN < nFiles; fileN++) {
    if (fileN==0) {
      Char_t* part = "pion";
      Bool_t pion = kTRUE;
    } else {
      Char_t* part = "proton";
      Bool_t pion = kFALSE;
    }
    
    // v doubly intergated  -------------------------------
    canvas->cd(1+fileN);
    gPad->SetTopMargin(margin);
    gPad->SetBottomMargin(margin);
    gPad->SetLeftMargin(margin);
    gPad->SetRightMargin(margin);
    if (pion) gPad->SetBottomMargin(0.);
    else gPad->SetTopMargin(0.);
    if (eBeam==158) gPad->SetRightMargin(0.);
    else gPad->SetLeftMargin(0.);
    flowV[0][fileN]->SetMarkerStyle(kFullCircle);
    flowV[1][fileN]->SetMarkerStyle(kFullSquare);
    flowV[0][fileN]->SetMarkerColor(kRed);
    flowV[1][fileN]->SetMarkerColor(kGreen);
    flowV[1][fileN]->SetLineColor(kGreen);
    flowV[0][fileN]->SetLineColor(kRed);
    flowV[0][fileN]->SetMarkerSize(markerSize);
    flowV[1][fileN]->SetMarkerSize(markerSize);
    
    sprintf(title, "%s v ", part);
    flowV[0][fileN]->SetTitle(strcat(title, part));
    hist = new TH1F(title, title, 6, 0., 1.);
    max =  8.;
    min = -6.;
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    cout << endl << "##### " << part << " v1 " << endl << endl;
    cout << endl << "##### " << part << " v2 " << endl << endl;
    if (eBeam == 158) {
      c3->SetLineColor(kRed);
      flowV[0][fileN]->Fit("c3", "R");
      flowV[0][fileN]->Draw("P");
      flowV[1][fileN]->Fit("c2", "R");
      flowV[1][fileN]->Draw("P");
    } else {
      e2->SetLineColor(kRed);
      flowV[0][fileN]->Fit("e2", "R");
      flowV[0][fileN]->Draw("P");
      flowV[1][fileN]->Fit("d2", "R");
      flowV[1][fileN]->Draw("P");
    }
    
    l.SetTextColor(kBlue);
    l.SetTextAngle(90); 
    ordTitle = new TString(" flow");
    ordTitle->Prepend(part);
    if (eBeam==158) l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    if (!pion) {
      if (eBeam==158) l.DrawLatex(0.8,0.02,"E^{0}/E^{0}_{beam}" ); 
      else l.DrawLatex(0.7,0.02,"E^{0}/E^{0}_{beam}" );
    }
    l.SetTextSize(commentSize); 
    if (eBeam == 158) {
      if (pion) {
	l.DrawLatex(0.2,0.75,"0. < y < 2.1");
	l.DrawLatex(0.2,0.70,"0 < p_{t} < 2 GeV/c");
      } else {
	l.DrawLatex(0.2,0.9,"0. < y < 2.1");
	l.DrawLatex(0.2,0.85,"0 < p_{t} < 2 GeV/c");
      }
    } else {
      if (pion) {
	l.DrawLatex(0.05,0.75,"0. < y < 1.8");
	l.DrawLatex(0.05,0.70,"0 < p_{t} < 2 GeV/c");
      } else {
	l.DrawLatex(0.05,0.90,"0. < y < 1.8");
	l.DrawLatex(0.05,0.85,"0 < p_{t} < 2 GeV/c");
      }
    }
    if (fileN) {
      sprintf(energyTitle, "%d A#bulletGeV", eBeam);
      l.SetTextSize(energySize); 
      if (eBeam==158) l.DrawLatex(0.75,0.87,energyTitle);
      else l.DrawLatex(0.62,0.87,energyTitle);
    }
    l.SetTextSize(textSize); 
        
    l.SetTextColor(kRed); 
    if (eBeam == 158) {
      if (pion) {
	l.DrawLatex(0.7,0.18,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.63,"v_{2}"); 
      } else {
	l.DrawLatex(0.73,0.42,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.73,0.77,"v_{2}"); 
      }
    } else {
      if (pion) {
	l.DrawLatex(0.6,0.27,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.6,0.63,"v_{2}"); 
      } else {
	l.DrawLatex(0.67,0.73,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.65,0.54,"v_{2}"); 
      }
    }
  }

  sprintf(outfile, "%s%d_v_cen.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  //if (!Pause()) return;

  return;

}

bool Pause() {
  // Wait for input.
  char temp[10];

  cout << "next?, quit? q" << endl;
  if (!batch) fgets(temp, sizeof(temp), stdin);  // remove to run in batch
  if (strstr(temp, "q")) {
    fclose(fpar);   // Save fit parameters to file
    return kFALSE;
  }

  return kTRUE;
}

void PrintGraph(TGraphErrors* graph) {
  // Outputs TGraphErrors objects to ROOT file.

  Int_t nBins = graph->GetN();
  Double_t *x,*y,*ey;

  x  = graph->GetX();
  y  = graph->GetY();
  ey = graph->GetEY();

  fprintf(f, "abcissa\n");

  fprintf(f, "{");
  if (x[0] > 0)
    fprintf(f, "%f", x[0]);
  for (Int_t i = 1; i < nBins; i++){
    if (x[i] > 0)
      fprintf(f, ", %f", x[i]);    
  }
  fprintf(f, "}\n");
  
  fprintf(f, "flow\n");
  fprintf(f, "{");
  if (x[0] > 0)
    fprintf(f, "%f", y[0]);
  for (Int_t i = 1; i < nBins; i++){
    if (x[i] > 0)
      fprintf(f, ", %f", y[i]);
  }
  fprintf(f, "}\n");
  
  fprintf(f, "error\n");
  fprintf(f, "{");
  if (x[0] > 0)
    fprintf(f, "%f", ey[0]);
  for (Int_t i = 1; i < nBins; i++){
    if (x[i] > 0)
      fprintf(f, ", %f", ey[i]);
  }
  fprintf(f, "}\n\n");

}


//========================================================

void PrintFits() {
  // Outputs fit parameters to txt file.

  Double_t  par[nPars];
  func_v->GetParameters(par);
  Double_t* err   = func_v->GetParErrors();
  Double_t  chiSq = func_v->GetChisquare();
  Int_t     NDF   = func_v->GetNDF();
  char* status = (char*)gMinuit->fCstatu.Data();

  TDatime now;
  fprintf(fpar, "%s \n", now->AsString());
  fprintf(fpar, "Params: T= %f, rho0= %f, rhoa= %f, s= %f \n", 
	  par[0], par[1], par[2], par[3]);
  fprintf(fpar, "Errors: T= %f, rho0= %f, rhoa= %f, s= %f \n", 
	  err[0], err[1], err[2], err[3]);
  fprintf(fpar, "ChiSq/df= %f for %.0f df with status= %s\n", 
	  chiSq/(double)NDF, NDF, status);
  fprintf(fpar, "\n");

  fclose(fpar);                  // Save parameters to file
  fpar = fopen("par.txt", "a");  // reopen

  return;
}


Double_t integrand(Double_t *xx, Double_t *ppar) {
  // Calculates Blast Wave integrand.

  Double_t phi  = xx[0];
  Double_t pt   = ppar[0];
  Double_t mass = ppar[1];
  Double_t T    = ppar[2];
  Double_t rho0 = ppar[3];
  Double_t rhoa = ppar[4];
  Double_t s    = ppar[6];
  Double_t ord  = ppar[7];
  
  Double_t rho    = rho0 + rhoa*cos(ord*phi);
  Double_t mt     = sqrt(pt*pt + mass*mass);
  
  Double_t alphat = pt/T*TMath::sinh(rho); 
  Double_t betat  = mt/T*TMath::cosh(rho); 
  Double_t K1     = TMath::BesselK1(betat);
  Double_t I0     = TMath::BesselI0(alphat);
  Double_t I;
  Double_t bw;

  if (ppar[5] > 0.) {  // numerator
    Double_t I1 = TMath::BesselI1(alphat);
    if (ord < 1.5) {
      I = I1;
    } else if (ord > 1.5) {
      Double_t I2 = I0 - 2./alphat*I1;
      I = I2;
    }
    bw = I * K1 * cos(ord*phi) * (1 + 2.*s*cos(ord*phi));
  } else {             // denominator
    bw = I0 * K1 * (1 + 2.*s*cos(ord*phi));
  }

  return bw;
}                                                                              


TF1* func_bw = new TF1("func_bw", integrand, 0., 2.*PI, 8);


Double_t vi(Double_t* x, Double_t* pari) {
  // Calculates v for Blast Wave fitting.

  Double_t pt = *x;

//    param[0] = T;
//    param[1] = rho0;
//    param[2] = rhoa;
//    param[3] = s;

  func_bw->SetParameter(0, pt);
  func_bw->SetParameter(2, pari[0]);
  func_bw->SetParameter(3, pari[1]);
  func_bw->SetParameter(4, pari[2]);
  func_bw->SetParameter(6, pari[3]);

  func_bw->SetParameter(5, 1.);
  Double_t numerator = func_bw->Integral(-PI,PI);

  func_bw->SetParameter(5, -1.);
  Double_t denominator = func_bw->Integral(-PI,PI);

  if (denominator == 0.) {
    cout << "Error: denominator is 0" << endl;
    return;
  }

  Double_t vi = numerator/denominator;
  
  return vi;
}


TF1* func_v = new TF1("func_v", vi, 0.01, 2., nPars);


///////////////////////////////////////////////////////////////////////////
//
// $Log: plotMultiGraphFits.C,v $
// Revision 1.2  2003/01/24 23:10:25  posk
// For version 37 of the paper.
//
// Revision 1.1  2002/11/15 22:38:17  posk
// updates.
//
// Revision 1.1  2002/05/14 20:59:30  posk
// For paper version 09.
//
//
///////////////////////////////////////////////////////////////////////////
