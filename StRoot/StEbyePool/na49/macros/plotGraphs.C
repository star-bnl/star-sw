// Plot histograms for all 6 centralities averaged to 3
// and also minimum bias and also v as a function of centrality.
// Reads from file produced by vProj.C.
// Alexander Wetzler and Art Poskanzer, April 2001

void plotGraphs(Char_t* part = "pion") {
  Bool_t pion = kFALSE;
  if (strcmp(part, "pion")==0) pion = kTRUE;

  // The directories must exist
  Bool_t crossSection = kTRUE;

  //Bool_t crossSection = kFALSE;  // yield weighting
//   if (crossSection) {
//     Char_t* fileExt = ".root";
//     Char_t* plotExt = "Plots";
//   } else {
//     Char_t* fileExt = "Yield.root";
//     Char_t* plotExt = "YieldPlots";
//   }

  if (crossSection) {
    Char_t* fileExt = "Pcons.root";
    Char_t* plotExt = "PconsPlots";
  } else {
    Char_t* fileExt = "PconsYield.root";
    Char_t* plotExt = "PconsYieldPlots";
  }

  Char_t pstype[255] = "ps";
  //Char_t pstype[255] = "eps";
  //Char_t pstype[255] = "gif";

  gROOT->SetStyle("Bold");
  gStyle->SetOptStat(kFALSE);

  const Int_t nHar = 2;
  const Int_t nCen = 10;   // min. bias + six centralities + 3 averaged
  Int_t   nYbins;
  Int_t   nYbinsMB;
  Int_t   nPtbins;
  Float_t max;
  Float_t min;
  Float_t flip;
  Float_t yCM = 2.92;
  Float_t markerSize = 1.5;

  Char_t temp[30];
  strcpy(temp, part);
  Char_t infile[255] = strcat(temp, fileExt);
  cout << "in file = " << infile << endl;

  strcpy(temp, part);
  strcat(temp, plotExt);
  Char_t outdir[255] = strcat(temp, "/");
  cout << "out dir = " << outdir << endl;
  Char_t outfile[255];

  Float_t yMax;
  Float_t yMin;
  Float_t yRefMax;
  Float_t yRefMin;
  if (pion) {
    yMax    = 4.9;
    yMin    = 2.85;
    yRefMax = 3.0;
    yRefMin = 1.1;
  } else {
    yMax    = 4.75;
    yMin    = 2.85;
    yRefMax = 3.0;
    yRefMin = 1.1;
  }
  Float_t yAllMax = 4.4;
  Float_t yAllRefMin = 1.4;

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

  // pt polynomial which goes to zero
  TF1* n1 = new TF1("n1", "[0]*x + [1]*x*x", 0., 0.08);

  // y for first (f), second (s), and reflected (r)
  TF1* s1 = new TF1("s1", "[0] + [1]*pow(x-2.92,2)", yMin, yMax);
  TF1* r1 = new TF1("r1", "[0] + [1]*pow(x-2.92,2)", yRefMin, yRefMax);
  TF1* f1 = new TF1("f1", "[0] + [1]*(x-2.92)", yMin, yMax);
  TF1* s2 = new TF1("s2", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)", 
    yMin, yMax);
  TF1* r2 = new TF1("r2", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)",
    yRefMin, yRefMax);
  TF1* s2All = new TF1("s2All", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)", 
    yMin, yAllMax);
  TF1* r2All = new TF1("r2All", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)",
    yAllRefMin, yRefMax);
  TF1* f2 = new TF1("f2", "[0] + [1]*(x-2.92) + [2]*pow(x-2.92,3)", yMin, yMax);
  TF1* f3 = new TF1("f3",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yMin, yMax);
  TF1* r3 = new TF1("r3",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yRefMin, yRefMax);
  TF1* f3All = new TF1("f3All",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yMin, yAllMax);
  TF1* r3All = new TF1("r3All",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yAllRefMin, yRefMax);

  // centrality polynomials
  TF1* c1 = new TF1("c1", "pol2", 1., 6.);
  TF1* c2 = new TF1("c2", "pol3", 1., 6.);
  TF1* c3 = new TF1("c3", "pol4", 1., 6.);

  char selText[2];
  char harText[2];
  char cenText[2];

  // Read input files
  TH1F *Y[nCen][nHar];
  TFile *file = new TFile(infile, "READ");
  for (Int_t j = 0; j < nHar; j++) {
    sprintf(harText, "%d", j + 1);
    for (Int_t i = 0; i < nCen; i++) {
      if (i==0 || i==7 || i==8 || i==9){ // min. bias + 6 cent. combined to 3
	sprintf(cenText, "%d", i);
	TString *histName = new TString("Flow_vY_Sel2_Har");
	histName->Append(*harText);
	histName->Append("_Cen");
	histName->Append(*cenText);
	Y[i][j] = (TH1F*)file->Get(histName->Data());
	if (!pion && i!=0) {
	  Y[i][j]->Rebin(4);  
	  Y[i][j]->Scale(0.25);
	} else if (!pion && j==1) {
	  Y[i][j]->Rebin(4);  
	  Y[i][j]->Scale(0.25);
	} else {
	  Y[i][j]->Rebin(2);
	  Y[i][j]->Scale(0.5);
	}
	delete histName;
      }
    }
  }
  nYbins   = Y[7][0]->GetNbinsX();
  nYbinsMB = Y[0][0]->GetNbinsX();
  
  TH1F *Pt[nCen][nHar];
  for (Int_t j = 0; j < nHar; j++) {
    sprintf(harText, "%d", j + 1);
    for (Int_t i = 0; i < nCen; i++) {
      if (i==0 || i==7 || i==8 || i==9){
	sprintf(cenText, "%d", i);
	TString *histName = new TString("Flow_vPt_Sel2_Har");
	histName->Append(*harText);
	histName->Append("_Cen");
	histName->Append(*cenText);
	Pt[i][j] = (TH1F*)file->Get(histName->Data());
	if (!pion) {
	  Pt[i][j]->Rebin(8);
	  Pt[i][j]->Scale(0.125);
	} else {
	  //Pt[i][j]->Rebin(2);
	  //Pt[i][j]->Scale(0.5);
	}
	delete histName;
      }
    }
  }
  nPtbins = Pt[7][0]->GetNbinsX();
    
  TH1F *V[nHar];
  for (Int_t j = 0; j < nHar; j++) {
    sprintf(harText, "%d", j + 1);
    TString *histName = new TString("Flow_v_Sel2_Har");
    histName->Append(*harText);
    V[j] = (TH1F*)file->Get(histName->Data());
    delete histName;
  }

  // Make graphs for Y projection  
  // flowY[cen][har][reflected]
  TGraphErrors *flowY[nCen][nHar][2];
  for (Int_t i = 0; i < nCen; i++) {
    for (Int_t j = 0; j < nHar; j++) {
      for (Int_t k = 0; k < 2; k++) {
	flowY[i][j][k] = new TGraphErrors();
      }
    }
  }
  
  // Make graphs for pt projection
  TGraphErrors *flowPt[nCen][nHar][2];
  for (Int_t i = 0; i < nCen; i++) {
    for (Int_t j = 0; j < nHar; j++) {
      for (Int_t k = 0; k < 2; k++) {
	flowPt[i][j][k] = new TGraphErrors();
      }
    }
  }
  
  // Make graphs for v doubly projected
  TGraphErrors *flowV[nHar];
  for (Int_t j = 0; j < nHar; j++) {
    flowV[j] = new TGraphErrors();
  }
  
  // Fill graphs with Y projection
  for (Int_t i = 0; i < nCen; i++) {
    if (!pion && i==0) nYbins = nYbinsMB;
    for (Int_t j = 0; j < nHar; j++) {
      if (j) { flip = 1.; }
      else { flip = -1.; }
      for (Int_t k = 0; k < nYbins; k++) {
      if (i==0 || i==7 || i==8 || i==9){
	  if (Y[i][j]->GetBinCenter(k+1) > yMin && 
	      //Y[i][j]->GetBinContent(k+1) != 0 &&
	      Y[i][j]->GetBinError(k+1) < 2) {
	    flowY[i][j][0]->SetPoint(k, Y[i][j]->GetBinCenter(k+1),
				       flip * Y[i][j]->GetBinContent(k+1));
	    flowY[i][j][0]->SetPointError(k, 0., Y[i][j]->GetBinError(k+1));
	    flowY[i][j][1]->SetPoint(k, 2 * yCM -Y[i][j]->GetBinCenter(k+1),
				     Y[i][j]->GetBinContent(k+1));
	    flowY[i][j][1]->SetPointError(k, 0., Y[i][j]->GetBinError(k+1));
	  }
	}
//       flowY[i][j][0]->Rebin(2);
//       flowY[i][j][0]->Scale(0.5);
//       flowY[i][j][1]->Rebin(2);
//       flowY[i][j][1]->Scale(0.5);
      }
    }
  }
  
  // Fill graphs with Pt projection  - - -  for protons
  if (!pion) {
    for (Int_t i = 0; i < nCen; i++) {
      for (Int_t j = 0; j < nHar; j++) {
	if (j) { flip = 1.; }
	else { flip = -1.; }
	for (Int_t k = 0; k < nPtbins; k++) {
	  if (i==0 || i==7 || i==8 || i==9){
	    if (Pt[i][j]->GetBinError(k+1) < 4){
	      flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter(k+1),
					flip * Pt[i][j]->GetBinContent(k+1));
	      flowPt[i][j][0]->SetPointError(k, 0., Pt[i][j]->GetBinError(k+1));
	    } else {
	      flowPt[i][j][0]->SetPoint(k,-1,0);
	    }
	  } else {
	    flowPt[i][j][0]->SetPoint(k,-1,0);
	  }
	}
      }
    }
  } else {
    // Fill graphs with Pt projection  - - -  for pions
    for (Int_t i = 0; i < nCen; i++) {
      for (Int_t j = 0; j < nHar; j++) {
	if (j) { flip = 1.; }
	else { flip = -1.; }	
	if (i==0 || i==7 || i==8 || i==9){
	  for (Int_t k = 0; k < 4; k++) {
	    if (Pt[i][j]->GetBinError(k+1) < 3){
	      flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter(k+1),
					flip * Pt[i][j]->GetBinContent(k+1));
	      flowPt[i][j][0]->SetPointError(k, 0., Pt[i][j]->GetBinError(k+1));
	    } else {
	      flowPt[i][j][0]->SetPoint(k,-1,0);
	    }
	  }	    
	  Pt[i][j]->Rebin();
	  Pt[i][j]->Scale(0.5);
	  for (Int_t k = 4; k < 8; k++) {
	    if (Pt[i][j]->GetBinError(k+1) < 3){
	      flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter(k+1),
					flip * Pt[i][j]->GetBinContent(k+1));
	      flowPt[i][j][0]->SetPointError(k, 0., Pt[i][j]->GetBinError(k+1));
	    } else {
	      flowPt[i][j][0]->SetPoint(k,-1,0);
	    }
	  }	    
	  Pt[i][j]->Rebin();
	  Pt[i][j]->Scale(0.5);
	  for (Int_t k = 8; k < 12; k++) {
	    if (!(k%2)){
	      if (Pt[i][j]->GetBinError((k/2)+1) < 3) {
		flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter((k/2)+1),
			              flip * Pt[i][j]->GetBinContent((k/2)+1));
		flowPt[i][j][0]->SetPointError(k, 0.,
					       Pt[i][j]->GetBinError((k/2)+1));
	      } else {
		flowPt[i][j][0]->SetPoint(k,-1,0);
	    }
	    } else {
	      flowPt[i][j][0]->SetPoint(k,-1,0);
	    }
	  }
	  Pt[i][j]->Rebin();
	  Pt[i][j]->Scale(0.5);
	  for (Int_t k = 12; k < 20; k++) {
	    if (!(k%4)){
	      if (Pt[i][j]->GetBinError((k/4)+1) < 3) {
		flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter((k/4)+1),
				      flip * Pt[i][j]->GetBinContent((k/4)+1));
		flowPt[i][j][0]->SetPointError(k, 0., 
					       Pt[i][j]->GetBinError((k/4)+1));
	      } else {
		flowPt[i][j][0]->SetPoint(k,-1,0);
	      }
	    } else {
	      flowPt[i][j][0]->SetPoint(k,-1,0);
	    }  
	  }
	}    
      }
    }
  }

  // Fill graphs with v doubly projected
  for (Int_t j = 0; j < nHar; j++) {
    if (j) { flip = 1.; }
    else { flip = -1.; }	
    for (Int_t k = 0; k < 6; k++) {
      flowV[j]->SetPoint(k, k+1, flip * V[j]->GetBinContent(k+1));
      flowV[j]->SetPointError(k, 0., V[j]->GetBinError(k+1));
    }
  }
  
  // Create Canvas
  TCanvas *canvas = new TCanvas("Flow", "Flow", 100, 100, 840, 600);
  canvas->cd();
  TLegend *legend = new TLegend();

  // Draw Graphs

  // Minimum Bias vs. y -------------------- ------------------------
  Char_t title[255] = "Minimum Bias ";
  flowY[0][0][0]->SetTitle(strcat(title, part));
  flowY[0][0][0]->SetMarkerStyle(20);
  flowY[0][0][1]->SetMarkerStyle(24);
  flowY[0][1][0]->SetMarkerStyle(21);
  flowY[0][1][1]->SetMarkerStyle(25);
  flowY[0][0][0]->SetMarkerColor(kRed);
  flowY[0][0][1]->SetMarkerColor(kRed);
  flowY[0][1][0]->SetMarkerColor(kGreen);
  flowY[0][1][1]->SetMarkerColor(kGreen);
  flowY[0][0][0]->SetMarkerSize(markerSize);
  flowY[0][0][1]->SetMarkerSize(markerSize);
  flowY[0][1][0]->SetMarkerSize(markerSize);
  flowY[0][1][1]->SetMarkerSize(markerSize);

  canvas->Clear();
  TH1F *hist = new TH1F(title, title, 10, 1, 5);
  if (pion) {
    max = 4.;
    min = -4.;
  } else {
    max = 4.;
    min = -4.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[0][0][0]->Fit("f3", "R");
  flowY[0][0][0]->Draw("P");
  flowY[0][0][1]->Fit("r3", "R");
  flowY[0][0][1]->Draw("P");
  if (pion) {
    flowY[0][1][0]->Fit("s2", "R");
    flowY[0][1][1]->Fit("r2", "R");
  } else {
    flowY[0][1][0]->Fit("s1", "R");
    flowY[0][1][1]->Fit("r1", "R");
  }
  flowY[0][1][0]->Draw("P");
  flowY[0][1][1]->Draw("P");

  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();
  
  TLatex l; 
  l.SetNDC();
  l.SetTextColor(kBlue); 
  l.SetTextAlign(12); 
  l.SetTextSize(0.06); 
  l.SetIndiceSize(0.5);  
  
  l.SetTextAngle(90); 
  l.DrawLatex(0.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"rapidity" );

  l.SetTextSize(0.04); 
  l.DrawLatex(0.2,0.2,"p_{t} < 2 GeV/c");
  l.SetTextSize(0.06); 

  l.SetTextColor(kRed); 
  if (pion) {
    l.DrawLatex(0.7,0.25,"v_{1}"); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.7,0.82,"v_{2}"); 
  } else {
    l.DrawLatex(0.8,0.8,"v_{1}"); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.6,0.7,"v_{2}"); 
  }

  sprintf(outfile, "%smb_y.%s", outdir, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  // Minimum Bias versus pt -------------------- ---------------------
  sprintf(title, "Minimum Bias ");
  flowPt[0][0][0]->SetTitle(strcat(title, part));
  flowPt[0][0][0]->SetMarkerStyle(20);
  flowPt[0][1][0]->SetMarkerStyle(21);
  flowPt[0][0][0]->SetMarkerColor(kRed);
  flowPt[0][1][0]->SetMarkerColor(kGreen);
  flowPt[0][0][0]->SetMarkerSize(markerSize);
  flowPt[0][1][0]->SetMarkerSize(markerSize);

  delete hist;
  canvas->Clear();
  hist = new TH1F(title, title, 10, 0, 2);
  if (pion) {
    max = 10.;
    min = -3.;
  } else {
    max = 15.;
    min = -5.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();

  //gStyle->SetFuncColor(kRed); // does not work
  if (pion) {
    //flowPt[0][0][0]->Draw("PC");
    flowPt[0][0][0]->Fit("o2", "R");
    flowPt[0][0][0]->Fit("n1", "R+");
    flowPt[0][0][0]->Draw("P");
  } else {
    flowPt[0][0][0]->Fit("p3", "R");
    flowPt[0][0][0]->Draw("P");
  }
  //gStyle->SetFuncColor(kGreen);
  flowPt[0][1][0]->Fit("p2", "R");
  flowPt[0][1][0]->Draw("P");

  l.SetTextColor(kBlue); 
  l.SetTextAngle(90); 
  l.DrawLatex(.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"p_{t} (GeV/c)" ); 

  l.SetTextSize(0.04); 
  l.DrawLatex(0.2,0.8,"3 < y < 5");
  l.SetTextSize(0.06); 

  l.SetTextColor(kRed); 
  if (pion) {
    l.DrawLatex(0.7,0.45,"v_{1}"); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.7,0.75,"v_{2}");
  } else {
    l.DrawLatex(0.7,0.4,"v_{1}"); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.7,0.65,"v_{2}");
  } 
  
  sprintf(outfile, "%smb_pt.%s", outdir, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  // Set Marker Types
  for (Int_t m = 0; m < 3; m++) {
    int i = m + 7;
    for (Int_t j = 0; j < 2; j++) {
      for (Int_t k = 0; k < 2; k++) {
	flowY[i][j][k]->SetMarkerColor(m+2);
	flowY[i][j][k]->SetMarkerSize(markerSize);
	flowY[i][j][k]->SetMarkerStyle(20+(m%3)+4*(k%2));
	flowPt[i][j][k]->SetMarkerColor(m+2);
	flowPt[i][j][k]->SetMarkerSize(markerSize);
	flowPt[i][j][k]->SetMarkerStyle(20+(m%3)+4*(k%2));
      }
    }
  }

  // Harmonic 1 vs. y all Centralities dummy
  sprintf(title, "v1 ");
  flowY[0][0][0]->SetTitle(strcat(title, part));
  delete hist;
  canvas->Clear();
  TH1F *hist = new TH1F(title, title, 10, 1, 5);
  if (pion) {
    max = 2.;
    min = -7.;
  } else {
    max = 8.;
    min = -8.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[0][0][0]->Draw("P");
  for (Int_t i = 0; i < 3; i++) {
    for (Int_t k = 0; k < 2; k++) {
      flowY[2*i][0][k]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.17);    
  legend->SetX2NDC(0.39);
  legend->SetY2NDC(0.37);
  Char_t EntryName[255];
  for (Int_t i = 0; i < 3; i++){
    sprintf(EntryName, "%d + %d", 2*i+1, 2*i+2);
    legend->AddEntry(flowY[i][0][0], EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();

  sprintf(outfile, "%sv1_all_y.%s", outdir, pstype);
  canvas->Print(outfile, pstype);

  // Harmonic 1 vs. y three Centralities redrawn ---------------------------
  sprintf(title, "v1 ");
  flowY[7][0][0]->SetTitle(strcat(title, part));
  delete hist;
  canvas->Clear();
  TH1F *hist = new TH1F(title, title, 10, 1, 5);
  if (pion) {
    max = 7.;
    min = -7.;
  } else {
    max = 10.;
    min = -10.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[7][0][0]->Draw("P");
  for (Int_t i = 7; i <= 9; i++) {
    if (!pion) {
      flowY[i][0][0]->Fit("f3All", "R");
      flowY[i][0][0]->Draw("P");
      flowY[i][0][1]->Fit("r3All", "R");
      flowY[i][0][1]->Draw("P");
    } else {
      flowY[i][0][0]->Fit("f3", "R");
      flowY[i][0][0]->Draw("P");
      flowY[i][0][1]->Fit("r3", "R");
      flowY[i][0][1]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.17);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.37);
  Char_t EntryName[255];
  for (Int_t i = 7; i <= 9; i++){
    sprintf(EntryName, "%d + %d", (i-6)*2-1, (i-6)*2);
    legend->AddEntry(flowY[i][0][0], EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();

  delete lineYcm;
  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();

  l.SetTextColor(kBlue); 
  l.SetTextAngle(90); 
  l.DrawLatex(.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0);
  l.DrawLatex(0.7,0.05,"rapidity" ); 

  l.SetTextSize(0.04); 
  l.DrawLatex(0.3,0.8,"p_{t} < 2 GeV/c");
  l.SetTextSize(0.06); 

  sprintf(outfile, "%sv1_all_y.%s", outdir, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  // Harmonic 2 vs. y three Centralities ---------------------------------
  sprintf(title, "v2 ");
  flowY[7][1][0]->SetTitle(strcat(title, part));
  delete hist;
  canvas->Clear();
  TH1F *hist = new TH1F(title, title, 10, 1, 5);
  if (pion) {
    max = 5.;
    min = -1.;
  } else {
    max = 10.;
    min = -10.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[7][1][0]->Draw("P");
  if (pion) {
    for (Int_t i = 7; i <= 9; i++) {
      flowY[i][1][0]->Fit("s2", "R");
      flowY[i][1][0]->Draw("P");
    }
    for (Int_t i = 7; i <= 9; i++) {
      flowY[i][1][1]->Fit("r2", "R");
      flowY[i][1][1]->Draw("P");
    }
  } else {
    for (Int_t i = 7; i <= 9; i++) {
      flowY[i][1][0]->Fit("s2All", "R");
      flowY[i][1][0]->Draw("P");
    }
    for (Int_t i = 7; i <= 9; i++) {
      flowY[i][1][1]->Fit("r2All", "R");
      flowY[i][1][1]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.17);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.37);
  Char_t EntryName[255];
  for (Int_t i = 7; i <= 9; i++){
    sprintf(EntryName, "%d + %d", (i-6)*2-1, (i-6)*2);
    legend->AddEntry(flowY[i][1][0], EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();
  
  delete lineYcm;
  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();

  l.SetTextAngle(90); 
  l.DrawLatex(.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"rapidity" ); 

  l.SetTextSize(0.04); 
  l.DrawLatex(0.7,0.2,"p_{t} < 2 GeV/c");
  l.SetTextSize(0.06); 

  sprintf(outfile, "%sv2_all_y.%s", outdir, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  // Harmonic 1 versus pt three Centralities ------------------------------
  sprintf(title, "v1 ");
  flowPt[7][0][0]->SetTitle(strcat(title, part));
  delete hist;
  canvas->Clear();
  hist = new TH1F(title, title, 10, 0, 2);
  if (pion) {
    max = 10.;
    min = -4.;
  } else {
    max = 15.;
    min = -5.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowPt[7][0][0]->Draw("P");
  for (Int_t i = 7; i <= 9; i++) {
    if (pion) {
      //flowPt[i][0][0]->Draw("PC");
      flowPt[i][0][0]->Fit("o3", "R");
      flowPt[i][0][0]->Fit("n1", "R+");
      flowPt[i][0][0]->Draw("P");
    } else {
      flowPt[i][0][0]->Fit("p3", "R");
      flowPt[i][0][0]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.65);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.85);
  Char_t EntryName[255];
  for (Int_t i = 7; i <= 9; i++){
    sprintf(EntryName, "%d + %d", (i-6)*2-1, (i-6)*2);
    legend->AddEntry(flowPt[i][0][0], EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();
  
  l.SetTextAngle(90); 
  l.DrawLatex(.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"p_{t} (GeV/c)" ); 
  
  l.SetTextSize(0.04); 
  l.DrawLatex(0.7,0.2,"3 < y < 5");
  l.SetTextSize(0.06); 

  sprintf(outfile, "%sv1_all_pt.%s", outdir, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;
  
  // Harmonic 2 versus pt three Centralities -------------------------------
  sprintf(title, "v2 ");
  flowPt[7][1][0]->SetTitle(strcat(title, part));
  delete hist;
  canvas->Clear();
  hist = new TH1F(title, title, 10, 0, 2);
  if (pion) {
    max = 20.;
    min = -2.;
  } else {
    max = 20.;
    min = -10.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowPt[7][1][0]->Draw("P");
  for (Int_t i = 7; i <= 9; i++) {
    flowPt[i][1][0]->Fit("p2", "R");
    flowPt[i][1][0]->Draw("P");
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.6);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.8);
  Char_t EntryName[255];
  for (Int_t i = 7; i <= 9; i++){
    sprintf(EntryName, "%d + %d", (i-6)*2-1, (i-6)*2);
    legend->AddEntry(flowPt[i][1][0], EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();
  
  l.SetTextAngle(90); 
  l.DrawLatex(.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"p_{t} (GeV/c)" ); 

  l.SetTextSize(0.04); 
  l.DrawLatex(0.22,0.85,"3 < y < 5");
  l.SetTextSize(0.06); 

  sprintf(outfile, "%sv2_all_pt.%s", outdir, pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;
 
  // v doubly intergated  -------------------------------
  flowV[0]->SetMarkerStyle(20);
  flowV[1]->SetMarkerStyle(21);
  flowV[0]->SetMarkerColor(kRed);
  flowV[1]->SetMarkerColor(kGreen);
  flowV[0]->SetMarkerSize(markerSize);
  flowV[1]->SetMarkerSize(markerSize);

  sprintf(title, "v ");
  flowV[0]->SetTitle(strcat(title, part));
  delete hist;
  canvas->Clear();
  hist = new TH1F(title, title, 6, 0.5, 6.5);
  if (pion) {
    max =  5.;
    min = -4.;
  } else {
    max =  7.;
    min = -5.;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  //flowV[0]->Draw("PC");
  //flowV[1]->Draw("PC");
  flowV[0]->Fit("c3", "R");
  flowV[0]->Draw("P");
  flowV[1]->Fit("c3", "R");
  flowV[1]->Draw("P");
  
  l.SetTextColor(kBlue);
  l.SetTextAngle(90); 
  l.DrawLatex(.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"Centrality" ); 

  l.SetTextSize(0.04); 
  l.DrawLatex(0.22,0.85,"3 < y < 5");
  l.DrawLatex(0.22,0.8,"0 < p_{t} < 2 GeV/c");
  l.SetTextSize(0.06); 

  l.SetTextColor(kRed); 
  if (pion) {
    l.DrawLatex(0.7,0.2,"v_{1}"); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.7,0.74,"v_{2}"); 
  } else {
    l.DrawLatex(0.75,0.52,"v_{1}"); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.75,0.69,"v_{2}"); 
  }

  sprintf(outfile, "%sv_cen.%s", outdir, pstype);
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
