///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotMultiGraphs.C,v 1.1 2002/05/14 20:59:30 posk Exp $
//
// Author:       Art Poskanzer and Alexander Wetzler, April 2002
// Description:
// Plot histograms for all 6 centralities averaged to 3
// and also minimum bias and also v as a function of centrality.
// Reads from pion and proton files produced by vProj.C.
// Divides canvas 2 x 2 or 1 x 2.
// Writes data to data.txt.
// Writes all TGraphsErrors objects to part_eBeam.root files.
//
///////////////////////////////////////////////////////////////////////////////

FILE* f = fopen("data.txt","w");

void plotMultiGraphs() {

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();

  // The directory must exist
  Char_t* outdir = "PconsPlots/";
  int   eBeam = 158; //select full beam energy
  //int   eBeam = 40;  //select 40Gev beam energy

  //Char_t pstype[255] = "ps";
  Char_t pstype[255] = "eps";
  //Char_t pstype[255] = "gif";

  const Int_t nFiles = 2;
  TFile*  file[nFiles];
  file[0] = new TFile("pionPcons.root", "READ");
  file[1] = new TFile("protonPcons.root", "READ");
  const Int_t nHar = 2;
  const Int_t nCen = 10;   // min. bias + six centralities + 3 averaged
  Int_t   nYbins;
  Int_t   nYbinsMB;
  Int_t   nPtbins;
  Float_t max;
  Float_t min;
  Float_t flip;
  Char_t* centrality[3] = {"central", "mid-central", "peripheral"};
  Float_t noPercent = 100.;
  Float_t yCM;
  Char_t  title[255];
  Char_t  energyTitle[255];
  Char_t  outfile[255];
  Float_t E0Beam = 36.8; // ?
  Float_t twoCol = 1.5;
  TOrdCollection *graphList = new TOrdCollection(26);

  if (eBeam == 158) {
    yCM = 2.92;
    Float_t E0[6] = {6.97, 11.61, 17.99, 23.76, 27.74, 31.45};
  } else if (eBeam == 40) {
    yCM =  2.24;
    Float_t E0[6] = {6.97, 11.61, 17.99, 23.76, 27.74, 31.45}; // not correct
  } else {
    cout << " Not valid beam energy" << endl;
    return;
  }

  gROOT->SetStyle("Bold");
  gROOT->ForceStyle();
  gStyle->SetOptStat(kFALSE);
  gStyle->SetOptTitle(kFALSE);
  gStyle->SetLineWidth((gStyle->GetLineWidth())/twoCol);
  gStyle->SetFuncWidth((gStyle->GetFuncWidth())/twoCol);
  gStyle->SetHistLineColor(kBlack);
  gStyle->SetHistLineWidth(gStyle->GetLineWidth());
  gStyle->SetHistLineStyle(kDashed);
  gStyle->SetFrameLineWidth((gStyle->GetFrameLineWidth())/twoCol);
  Float_t markerSize = 1.7/twoCol;
  Float_t textSize = 0.06;
  Float_t commentSize = 0.05;
  Float_t energySize = 0.07;

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
	    Pt[i][j][fileN]->Rebin(8);
	    Pt[i][j][fileN]->Scale(0.125);
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
    
    for (Int_t i = 0; i < nCen; i++) {
      if (i==0 || i==7 || i==8 || i==9){
	fprintf(f, "\n Centrality: %d \n", i);
	for (Int_t j = 0; j < nHar; j++) {
	  // Fill graphs with Y projection
	  if (!pion && i==0) nYbins = nYbinsMB;
	  flip = (j) ? 1. : -1.;
	  for (Int_t m = 0; m < nYbins; m++) {
	    Int_t n = m + 1;
	    if (Y[i][j][fileN]->GetBinCenter(n) > yMin && 
		Y[i][j][fileN]->GetBinCenter(n) < yMax && 
		Y[i][j][fileN]->GetBinContent(n) != 0 &&
		Y[i][j][fileN]->GetBinError(n) < 20) {
	      //flowY[i][j][0][fileN]->SetPoint(m, Y[i][j][fileN]->GetBinCenter(n),
	      flowY[i][j][0][fileN]->SetPoint(m, (Y[i][j][fileN]->GetBinCenter(n))-yCM,
				flip * Y[i][j][fileN]->GetBinContent(n));
	      flowY[i][j][0][fileN]->SetPointError(m, 0., 
						   Y[i][j][fileN]->GetBinError(n));
	      flowY[i][j][1][fileN]->SetPoint(m, 
			       //2 * yCM - Y[i][j][fileN]->GetBinCenter(n),
				     yCM - Y[i][j][fileN]->GetBinCenter(n),
				     Y[i][j][fileN]->GetBinContent(n));
	    flowY[i][j][1][fileN]->SetPointError(m, 0., 
						 Y[i][j][fileN]->GetBinError(n));
	    } else {
	      flowY[i][j][0][fileN]->SetPoint(m,-1000,0);
	      flowY[i][j][1][fileN]->SetPoint(m,1000,0);
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
		flowPt[i][j][0][fileN]->SetPoint(m, Pt[i][j][fileN]->GetBinCenter(n),
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
		flowPt[i][j][0][fileN]->SetPoint(m, Pt[i][j][fileN]->GetBinCenter(n),
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
		flowPt[i][j][0][fileN]->SetPoint(m, Pt[i][j][fileN]->GetBinCenter(n),
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
		flowPt[i][j][0][fileN]->SetPoint(m, Pt[i][j][fileN]->GetBinCenter(n),
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
	      if (!(m%8) && Pt[i][j][fileN]->GetBinError(n) < 48){
		flowPt[i][j][0][fileN]->SetPoint(m, Pt[i][j][fileN]->GetBinCenter(n),
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
	//flowV[j]->SetPoint(m, m+1, flip * V[j][fileN]->GetBinContent(n));
	flowV[j][fileN]->SetPoint(m, (E0[m]/E0Beam) - 0.005 + 0.01*j,
				  flip * V[j][fileN]->GetBinContent(n));
	flowV[j][fileN]->SetPointError(m, 0., V[j][fileN]->GetBinError(n));
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
  TLegend *legend = new TLegend();
  legend->SetBorderSize(6);
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
    yMaxPi      = 4.9  - yCM;
    yMinPi      = 2.85 - yCM;
    yReflMaxPi  = 3.0  - yCM;
    yReflMinPi  = 1.1  - yCM;
    yMaxPr      = 4.77 - yCM;
    yMinPr      = 0.;
    yReflMaxPr  = 0.;
    yReflMinPr  = 1.1  - yCM;
    yAllMaxPr   = 4.42 - yCM;
    yAllMaxPi   = 5.0  - yCM;
    yAllReflMin = 1.4  - yCM;
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
    
  // pt polynomials with zero intercepts
  TF1* p1 = new TF1("p1", "[0]*x", 0., 1.85);
  TF1* p2 = new TF1("p2", "[0]*x + [1]*x*x", 0., 1.85);
  TF1* p3 = new TF1("p3", "[0]*x + [1]*x*x + [2]*x*x*x", 0., 1.85);
  TF1* p4 = new TF1("p4", "[0]*x + [1]*x*x + [2]*x*x*x + [3]*x*x*x*x", 0., 1.85);
  
  // pt polynomials with constant offsets
  TF1* o1 = new TF1("o1", "pol2", 0.05, 1.85);
  TF1* o2 = new TF1("o2", "pol3", 0.05, 1.85);
  TF1* o3 = new TF1("o3", "pol4", 0.05, 1.85);
  TF1* o4 = new TF1("o4", "pol5", 0.05, 1.85);
  
  // pt polynomials near zero pt, which go to zero
  TF1* n2 = new TF1("n2", "[0]*x + [1]*x*x", 0., 0.08);
  
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
  //   TF1* c1 = new TF1("c1", "pol2", 1., 6.);
  //   TF1* c2 = new TF1("c2", "pol3", 1., 6.);
  //   TF1* c3 = new TF1("c3", "pol4", 1., 6.);
  TF1* c1 = new TF1("c1", "pol2", 0.18, 0.86);
  TF1* c2 = new TF1("c2", "pol3", 0.18, 0.86);
  TF1* c3 = new TF1("c3", "pol4", 0.18, 0.86);
    
  // Draw Graphs
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
    
    sprintf(title, "Minimum Bias y");
    if(eBeam == 158) {
      TH1F *hist = new TH1F(title, title, 10, 1.-yCM, 5.-yCM);
      if (pion) {
	max = 4.;
	min = -4.;
      } else {
	max = 4.;
	min = -4.;
      }
    } else {
      TH1F *hist = new TH1F(title, title, 10, 0.-yCM, 4.5-yCM);
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
    if(eBeam == 158) {
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
    
    //TLine* lineYcm = new TLine(yCM, min/noPercent, yCM, max/noPercent);
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
    l.DrawLatex(0.7,0.05,"rapidity" );
    
    l.SetTextSize(commentSize); 
    l.DrawLatex(0.2,0.2,"p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d GeV/A", eBeam);
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
	l.DrawLatex(0.8,0.72,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.6,0.7,"v_{2}"); 
      }
    } else {
      if (pion) {
	l.DrawLatex(0.7,0.30,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.75,"v_{2}"); 
      } else {
	l.DrawLatex(0.7,0.7,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.75,0.45,"v_{2}"); 
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
    flowPt[0][0][0][fileN]->SetMarkerSize(markerSize);
    flowPt[0][1][0][fileN]->SetMarkerSize(markerSize);
    
    sprintf(title, "Minimum Bias pt");
    hist = new TH1F(title, title, 10, 0., 2.);
    if(eBeam == 158) {
      if (pion) {
	max = 10.;
	min = -3.;
      } else {
	max = 10.;
	min = -5.;
      }
    } else {
      if (pion) {
	max = 20.;
	min = -5.;
      } else {
	max = 15.;
	min = -5.;
      }
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    
    if (pion) {
      //     flowPt[0][0][0]->Fit("o2", "R");
      //     flowPt[0][0][0]->Fit("n2", "R+");
      flowPt[0][0][0][fileN]->Draw("PC");
    } else {
      //     flowPt[0][0][0]->Fit("p3", "R");
      flowPt[0][0][0][fileN]->Draw("PC");
    }
    //   flowPt[0][1][0]->Fit("p3", "R");
    flowPt[0][1][0][fileN]->Draw("PC");
    
    l.SetTextColor(kBlue); 
    l.SetTextAngle(90); 
    ordTitle = new TString(" flow");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.05,"p_{t} (GeV/c)" ); 
    
    if(eBeam == 158) {
      l.SetTextSize(commentSize); 
      l.DrawLatex(0.2,0.8,"3 < y < 5");
      l.SetTextSize(textSize); 
      l.SetTextColor(kRed); 
      if (pion) {
	l.DrawLatex(0.7,0.35,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.8,"v_{2}");
      } else {
	l.DrawLatex(0.7,0.45,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.8,"v_{2}");
      }
    } else {
      l.SetTextSize(commentSize); 
      l.DrawLatex(0.2,0.8,"2.24 < y < 4");
      l.SetTextSize(textSize); 
      l.SetTextColor(kRed); 
      if (pion) {
	l.DrawLatex(0.7,0.25,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.7,"v_{2}");
      } else {
	l.DrawLatex(0.7,0.5,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.7,"v_{2}");
      }
    }
  }

  sprintf(outfile, "%s%d_mb.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  // Create new canvas
  delete hist;
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
	  flowPt[i][j][k][fileN]->SetMarkerSize(markerSize);
	  flowPt[i][j][k][fileN]->SetMarkerStyle(20+(m%3)+4*(k%2));
	}
      }
    }
    
    // Harmonic 1 vs. y all Centralities dummy
    canvas->cd(1+fileN);
    sprintf(title, "v1 y");
    flowY[0][0][0][fileN]->SetTitle(strcat(title, part));
    TH1F *hist = new TH1F(title, title, 10, 1., 5.);
    if (pion) {
      max = 2.;
      min = -7.;
    } else {
      max = 8.;
      min = -8.;
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    flowY[0][0][0][fileN]->Draw("P");
    
    legend->Clear();
    legend->SetX1NDC(0.2);
    legend->SetY1NDC(0.17);    
    legend->SetX2NDC(0.41);
    legend->SetY2NDC(0.37);
    Char_t EntryName[255];
    for (Int_t i = 7; i <= 9; i++){
      legend->AddEntry(flowY[i][0][0][fileN], centrality[i-7], "P");
    }
    legend->SetHeader("Centralities");
    legend->Draw();
    
    sprintf(outfile, "%s%d_v1_all.%s", outdir, eBeam, pstype);
    canvas->Print(outfile, pstype);
    delete hist;
    
    // Harmonic 1 vs. y three Centralities redrawn ---------------------------
    canvas->cd(1+fileN);
    sprintf(title, "v1 y");
    flowY[7][0][0][fileN]->SetTitle(strcat(title, part));
    
    if (eBeam == 158) {
      TH1F *hist = new TH1F(title, title, 10, 1.-yCM, 5.-yCM);
      if (pion) {
	max = 7.;
	min = -7.;
      } else {
	max = 5.;
	min = -5.;
      }
    } else {
      TH1F *hist = new TH1F(title, title, 10, 0.-yCM, 4.5-yCM);
      if (pion) {
	max = 15.;
	min = -15.;
      } else {
	max = 15.;
	min = -15.;
      }
    }  
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    flowY[7][0][0][fileN]->Draw("P");
    for (Int_t i = 7; i <= 9; i++) {
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
        
    //TLine* lineYcm = new TLine(yCM, min/noPercent, yCM, max/noPercent);
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
    l.DrawLatex(0.7,0.05,"rapidity" ); 

    l.SetTextSize(commentSize); 
    l.DrawLatex(0.3,0.8,"p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d GeV/A", eBeam);
      l.SetTextSize(energySize); 
      l.DrawLatex(0.65,0.82,energyTitle);
    }
    l.SetTextSize(textSize); 
        
    // Harmonic 1 versus pt three Centralities ------------------------------
    canvas->cd(3+fileN);
    sprintf(title, "v1 pt");
    flowPt[7][0][0][fileN]->SetTitle(strcat(title, part));
    hist = new TH1F(title, title, 10, 0., 2.);
    
    if(eBeam == 158) {
      if (pion) {
	max = 5.;
	min = -5.;
      } else {
	max = 10.;
	min = -5.;
      }
    } else {
      if (pion) {
	max = 10.;
	min = -10.;
      } else {
	max = 20.;
	min = -5.;
      }
    }    
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    flowPt[7][0][0][fileN]->Draw("P");
    for (Int_t i = 7; i <= 9; i++) {
      if (pion) {
	//flowPt[i][0][0]->Draw("PC");
	//       flowPt[i][0][0]->Fit("o3", "R");
	//       flowPt[i][0][0]->Fit("n2", "R+");
	flowPt[i][0][0][fileN]->Draw("PC");
      } else {
	//       flowPt[i][0][0]->Fit("p3", "R");
	flowPt[i][0][0][fileN]->Draw("PC");
      }
    }
    
    if (fileN) {
      legend->Clear();
      legend->SetX1NDC(0.2);
      legend->SetY1NDC(0.65);    
      legend->SetX2NDC(0.47);
      legend->SetY2NDC(0.85);
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
    l.DrawLatex(0.7,0.05,"p_{t} (GeV/c)" ); 
    
    l.SetTextSize(commentSize); 
    if (eBeam == 158) {
      l.DrawLatex(0.7,0.2,"3 < y < 5");
    } else {
      l.DrawLatex(0.7,0.2,"2.24 < y < 4");
    }
    l.SetTextSize(textSize); 
  }
  sprintf(outfile, "%s%d_v1_all.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;
  
  // Create new canvas
  delete lineYcm;
  delete hist;
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
    sprintf(title, "v2 y");
    flowY[7][1][0][fileN]->SetTitle(strcat(title, part));
    
    if (eBeam == 158) {  
      TH1F *hist = new TH1F(title, title, 10, 1.-yCM, 5.-yCM);
      if (pion) {
	max = 5.;
	min = -1.;
      } else {
	max = 10.;
	min = -10.;
      }
    } else {
      TH1F *hist = new TH1F(title, title, 10, 0.-yCM, 4.5-yCM);
      if (pion) {
	max = 6.;
	min = -6.;
      } else {
	max = 10.;
	min = -10.;
      }
    }    
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    flowY[7][1][0][fileN]->Draw("P");
    if (pion) {
      for (Int_t i = 7; i <= 9; i++) {
	s2Pi->SetLineColor(i-5);
	r2Pi->SetLineColor(i-5);
	flowY[i][1][0][fileN]->Fit("s2Pi", "R");
	flowY[i][1][0][fileN]->Draw("P");
	flowY[i][1][1][fileN]->Fit("r2Pi", "R");
	flowY[i][1][1][fileN]->Draw("P");
      }
    } else {
      for (Int_t i = 7; i <= 9; i++) {
	s2AllPr->SetLineColor(i-5);
	r2AllPr->SetLineColor(i-5);
	flowY[i][1][0][fileN]->Fit("s2AllPr", "R");
	flowY[i][1][0][fileN]->Draw("P");
	flowY[i][1][1][fileN]->Fit("r2AllPr", "R");
	flowY[i][1][1][fileN]->Draw("P");
      }
    }
        
    //TLine* lineYcm = new TLine(yCM, min/noPercent, yCM, max/noPercent);
    TLine* lineYcm = new TLine(0., min/noPercent, 0., max/noPercent);
    lineYcm->SetLineStyle(kDashed);
    lineYcm->Draw();
    
    l.SetTextAngle(90); 
    ordTitle = new TString(" v_{2}");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.05,"rapidity" ); 
        
    l.SetTextSize(commentSize); 
    l.DrawLatex(0.7,0.2,"p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d GeV/A", eBeam);
      l.SetTextSize(energySize); 
      l.DrawLatex(0.65,0.82,energyTitle);
    }
    l.SetTextSize(textSize); 
    
    // Harmonic 2 versus pt three Centralities -------------------------------
    canvas->cd(3+fileN);
    sprintf(title, "v2 pt");
    flowPt[7][1][0][fileN]->SetTitle(strcat(title, part));
    hist = new TH1F(title, title, 10, 0., 2.);
    if (eBeam == 158) {
      if (pion) {
	max = 25.;
	min = -2.;
      } else {
	max = 25.;
	min = -10.;
      }
    } else {
      if (pion) {
	max = 25.;
	min = -5.;
      } else {
	max = 20.;
	min = -10.;
      }
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    flowPt[7][1][0][fileN]->Draw("P");
    for (Int_t i = 7; i <= 9; i++) {
      //     flowPt[i][1][0]->Fit("p2", "R");
      flowPt[i][1][0][fileN]->Draw("PC");
    }
    
    if (fileN) {
      legend->Clear();
      legend->SetX1NDC(0.2);
      legend->SetY1NDC(0.6);    
      legend->SetX2NDC(0.47);
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
    l.DrawLatex(0.7,0.05,"p_{t} (GeV/c)" ); 
    
    l.SetTextSize(commentSize); 
    if (eBeam == 158) {
      l.DrawLatex(0.22,0.82,"3 < y < 5");
    } else {
      l.DrawLatex(0.22,0.82,"2.24 < y < 4");
    }
    l.SetTextSize(textSize); 
  }

  sprintf(outfile, "%s%d_v2_all.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;
  
  // Create new canvas
  delete lineYcm;
  delete hist;
  delete canvas;
  TCanvas *canvas = new TCanvas("Flow", "Flow", 100, 100, 600, 780);
  canvas->Divide(1,2,0.,0.);

  markerSize *= twoCol;  
  gStyle->SetFrameLineWidth((gStyle->GetFrameLineWidth()*twoCol));
  gStyle->SetLineWidth((gStyle->GetLineWidth())*twoCol);
  gStyle->SetFuncWidth((gStyle->GetFuncWidth())*twoCol);
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
    flowV[0][fileN]->SetMarkerStyle(kFullCircle);
    flowV[1][fileN]->SetMarkerStyle(kFullSquare);
    flowV[0][fileN]->SetMarkerColor(kRed);
    flowV[1][fileN]->SetMarkerColor(kGreen);
    flowV[1][fileN]->SetLineColor(kGreen);
    flowV[0][fileN]->SetLineColor(kRed);
    flowV[0][fileN]->SetMarkerSize(markerSize);
    flowV[1][fileN]->SetMarkerSize(markerSize);
    
    sprintf(title, "v ");
    flowV[0][fileN]->SetTitle(strcat(title, part));
    //hist = new TH1F(title, title, 6, 0.5, 6.5);
    hist = new TH1F(title, title, 6, 0., 1.);
    if(eBeam == 158) {
      if (pion) {
	max =  5.;
	min = -4.;
      } else {
	max =  5.;
	min = -5.;
      }
    } else {
      if (pion) {
	max =  6.;
	min = -6.;
      } else {
	max =  6.;
	min = -6.;
      }
    }
    hist->SetMaximum(max/noPercent);
    hist->SetMinimum(min/noPercent);
    hist->Draw();
    //flowV[0]->Draw("PC");
    //flowV[1]->Draw("PC");
    c3->SetLineColor(kRed);
    flowV[0][fileN]->Fit("c3", "R");
    flowV[0][fileN]->Draw("P");
    flowV[1][fileN]->Fit("c2", "R");
    flowV[1][fileN]->Draw("P");
    
    l.SetTextColor(kBlue);
    l.SetTextAngle(90); 
    ordTitle = new TString(" flow");
    ordTitle->Prepend(part);
    l.DrawLatex(0.05,0.6,ordTitle->Data());
    delete ordTitle;
    l.SetTextAngle(0); 
    l.DrawLatex(0.7,0.04,"E^{0}/E^{0}_{beam}" ); 
    
    l.SetTextSize(commentSize); 
    if (eBeam == 158) {
      l.DrawLatex(0.17,0.83,"3 < y < 5");
    } else {
      l.DrawLatex(0.17,0.83,"2.24 < y < 4");
    }
    l.DrawLatex(0.17,0.78,"0 < p_{t} < 2 GeV/c");
    if (fileN) {
      sprintf(energyTitle, "%d GeV/A", eBeam);
      l.SetTextSize(energySize); 
      l.DrawLatex(0.65,0.82,energyTitle);
    }
    l.SetTextSize(textSize); 
        
    l.SetTextColor(kRed); 
    if (eBeam == 158) {
      if (pion) {
	l.DrawLatex(0.7,0.2,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.74,"v_{2}"); 
      } else {
	l.DrawLatex(0.75,0.57,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.75,0.78,"v_{2}"); 
      }
    } else {
      if (pion) {
	l.DrawLatex(0.7,0.27,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.7,0.76,"v_{2}"); 
      } else {
	l.DrawLatex(0.75,0.78,"v_{1}"); 
	l.SetTextColor(kGreen); 
	l.DrawLatex(0.75,0.57,"v_{2}"); 
      }
    }
  }

  sprintf(outfile, "%s%d_v_cen.%s", outdir, eBeam, pstype);
  canvas->Print(outfile, pstype);
  //if (!Pause()) return;

  return;

}

bool Pause() {
  char temp[10];
  cout << "next?, quit? q" << endl;
  fgets(temp, sizeof(temp), stdin);
  if (strstr(temp, "q")) return kFALSE;

  return kTRUE;
}

void PrintGraph(TGraphErrors* graph) {
  Int_t nBins = graph->GetN();
  Double_t *x,*y,*ey;

  x = graph->GetX();
  y = graph->GetY();
  ey = graph->GetEY();

  fprintf(f, "abcissa\n");

  fprintf(f, "{");
  if (x[0] > 0)
    fprintf(f, "%f",x[0]);
  for (Int_t i = 1; i < nBins; i++){
    if (x[i] > 0)
      fprintf(f, ", %f",x[i]);    
  }
  fprintf(f, "}\n");
  
  fprintf(f, "flow\n");
  fprintf(f, "{");
  if (x[0] > 0)
    fprintf(f, "%f",y[0]);
  for (Int_t i = 1; i < nBins; i++){
    if (x[i] > 0)
      fprintf(f, ", %f",y[i]);
  }
  fprintf(f, "}\n");
  
  fprintf(f, "error\n");
  fprintf(f, "{");
  if (x[0] > 0)
    fprintf(f, "%f",ey[0]);
  for (Int_t i = 1; i < nBins; i++){
    if (x[i] > 0)
      fprintf(f, ", %f",ey[i]);
  }
  fprintf(f, "}\n\n");

}

///////////////////////////////////////////////////////////////////////////
//
// $Log: plotMultiGraphs.C,v $
// Revision 1.1  2002/05/14 20:59:30  posk
// For paper version 09.
//
//
///////////////////////////////////////////////////////////////////////////
