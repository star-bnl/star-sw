// Plots same histograms from different files produced by vProj.C.
// Art Poskanzer, Dec. 2001

void plotFiles(Char_t* part = "pion") {
  Bool_t pion = kFALSE;
  if (strcmp(part, "pion")==0) pion = kTRUE;

  Bool_t crossSection = kTRUE;
  //Bool_t crossSection = kFALSE;     // yield weighting

  // The output directory must exist
  Char_t* plotExt = "ZeroCrossPlots/";

  const Int_t nFiles = 2;            // number of input files
  Char_t* fileExt[nFiles];
  if (crossSection) {
    fileExt[0] = "Pcons.root";
    fileExt[1] = ".root";
  } else {
    fileExt[0] = "PconsYield.root";
    fileExt[1] = "Yield.root";
  }

  //Char_t pstype[255] = "ps";
  //Char_t pstype[255] = "eps";
  Char_t pstype[255] = "gif";

  gROOT->SetStyle("Bold");
  gStyle->SetOptStat(kFALSE);
  gStyle->SetOptTitle(kFALSE);

  const Int_t har = 1;     // v1
  Int_t   nYbins;
  Int_t   nPtbins;
  Float_t max;
  Float_t min;
  Float_t flip;
  Float_t yCM = 2.92;
  Float_t markerSize = 1.5;

//   Char_t* infile[nFiles];
//   //Char_t temp[30];
//   //for (int i = 0; i < nFiles; i++) {
//   Char_t tmp1[30];
//   strcpy(tmp1, part);
//   infile[0] = strcat(tmp1, fileExt[0]);
//   cout << "in file 1 = " << infile[0] << endl;
//   Char_t tmp2[30];
//   strcpy(tmp2, part);
//   infile[1] = strcat(tmp2, fileExt[1]);
//   cout << "in file 2 = " << infile[1] << endl;
//   //}

//   Char_t tmp[30];
//   strcpy(tmp, part);
//   strcat(tmp, plotExt);
//   Char_t outdir[255] = strcat(tmp, "/");
//   cout << "out dir = " << outdir << endl;

  TString* inFileName[nFiles];
  for (int i = 0; i < nFiles; i++) {
    inFileName[i] = new TString(fileExt[i]);
    inFileName[i]->Prepend(part);
    cout << "in file " << i << " = " << inFileName[i]->Data() << endl;
  }
    TString* outDirName = new TString(plotExt);
    outDirName->Prepend(part);
    cout << "out dir = " << outDirName->Data() << endl;

  Float_t yMax;
  Float_t yMin;
  Float_t yReflMax;
  Float_t yReflMin;
  if (pion) {
    yMax    = 4.9;
    yMin    = 2.85;
    yReflMax = 3.0;
    yReflMin = 1.1;
  } else {
    yMax    = 4.75;
    yMin    = 2.85;
    yReflMax = 3.0;
    yReflMin = 1.1;
  }
  Float_t yAllMax = 4.4;
  Float_t yAllReflMin = 1.4;

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

  // y for first (f), second (s), and reflected (r)
  TF1* f1 = new TF1("f1", "[0] + [1]*(x-2.92)", yMin, yMax);
  TF1* f2 = new TF1("f2", "[0] + [1]*(x-2.92) + [2]*pow(x-2.92,3)", yMin, yMax);
  TF1* f3 = new TF1("f3",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yMin, yMax);

  TF1* s1 = new TF1("s1", "[0] + [1]*pow(x-2.92,2)", yMin, yMax);
  TF1* s2 = new TF1("s2", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)", 
    yMin, yMax);

  TF1* r1 = new TF1("r1", "[0] + [1]*pow(x-2.92,2)", yReflMin, yReflMax);
  TF1* r2 = new TF1("r2", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)",
    yReflMin, yReflMax);
  TF1* r3 = new TF1("r3",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yReflMin, yReflMax);

  // with constant offset
  TF1* fc3 = new TF1("fc3",
    "[0] + [1]*(x-2.92) + [2]*pow(x-2.92,3) + [3]*pow(x-2.92,5)", yMin, yMax);
  TF1* rc3 = new TF1("rc3",
    "[0] + [1]*(x-2.92) + [2]*pow(x-2.92,3) + [3]*pow(x-2.92,5)", yReflMin, yReflMax);

  // all points
  TF1* f3All = new TF1("f3All",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yMin, yAllMax);
  TF1* s2All = new TF1("s2All", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)", 
    yMin, yAllMax);
  TF1* r2All = new TF1("r2All", "[0] + [1]*pow(x-2.92,2) + [2]*pow(x-2.92,4)",
    yAllReflMin, yReflMax);
  TF1* r3All = new TF1("r3All",
    "[0]*(x-2.92) + [1]*pow(x-2.92,3) + [2]*pow(x-2.92,5)", yAllReflMin, yReflMax);

  //char selText[2];
  char harText[2];
  sprintf(harText, "%d", har);

  // Read input files and get Y, and Pt histograms
  TFile* file[nFiles];
  TH1F* Y[nFiles];
  TH1F* Pt[nFiles];
  TString* histYName = new TString("Flow_vY_Sel2_Har");
  histYName->Append(*harText);
  histYName->Append("_Cen0");
  TString* histPtName = new TString("Flow_vPt_Sel2_Har");
  histPtName->Append(*harText);
  histPtName->Append("_Cen0");
  for (int i = 0; i < nFiles; i++) {
    file[i] = new TFile(inFileName[i]->Data(), "READ");
    if (!file[i]) {
      cout << "no file " << inFileName[i]->Data() << endl;
      return;
    }
    delete inFileName[i];
    Y[i] = (TH1F*)file[i]->Get(histYName->Data());
    Y[i]->Rebin(2);
    Y[i]->Scale(0.5);
    Pt[i] = (TH1F*)file[i]->Get(histPtName->Data());
    if (!pion) {
      Pt[i]->Rebin(8);
      Pt[i]->Scale(0.125);
    } else {
      //Pt[i]->Rebin(2);
      //Pt[i]->Scale(0.5);
    }
  }  
  delete histYName;
  delete histPtName;
  nYbins  = Y[0]->GetNbinsX();
  nPtbins = Pt[0]->GetNbinsX();
  
  // Make graphs  
  // flowY[file][reflected=1]
  TGraphErrors* flowY[nFiles][2];
  TGraphErrors* flowPt[nFiles][2];
  for (Int_t i = 0; i < nFiles; i++) {
    for (Int_t k = 0; k < 2; k++) {
      flowY[i][k] = new TGraphErrors();
      flowPt[i][k] = new TGraphErrors();
    } 
  
  // Fill graphs with Y projection
  flip = (har-1) ? 1. : -1.;
  for (Int_t m = 0; m < nYbins; m++) {
    Int_t n = m + 1;
    if (Y[i]->GetBinCenter(n) > yMin && 
	Y[i]->GetBinError(n) < 2) {
      flowY[i][0]->SetPoint(m, Y[i]->GetBinCenter(n),
			       flip * Y[i]->GetBinContent(n));
      flowY[i][0]->SetPointError(m, 0., Y[i]->GetBinError(n));
      flowY[i][1]->SetPoint(m, 2 * yCM -Y[i]->GetBinCenter(n),
			       Y[i]->GetBinContent(n));
      flowY[i][1]->SetPointError(m, 0., Y[i]->GetBinError(n));
    }
  }
  // Fill graphs with Pt projection  - - -  for protons
  if (!pion) {
    for (Int_t m = 0; m < nPtbins; m++) {
      Int_t n = m + 1;
      if (Pt[i]->GetBinError(n) < 4){
	flowPt[i][0]->SetPoint(m, Pt[i]->GetBinCenter(n),
				  flip * Pt[i]->GetBinContent(n));
	flowPt[i][0]->SetPointError(m, 0., Pt[i]->GetBinError(n));
      } else {
	flowPt[i][0]->SetPoint(m,-1,0);
      }
    }
  } else {
    // Fill graphs with Pt projection  - - -  for pions
    for (Int_t m = 0; m < 8; m++) {
      Int_t n = m + 1;
      if (Pt[i]->GetBinError(n) < 3){
	flowPt[i][0]->SetPoint(m, Pt[i]->GetBinCenter(n),
				  flip * Pt[i]->GetBinContent(n));
	flowPt[i][0]->SetPointError(m, 0., Pt[i]->GetBinError(n));
      } else {
	flowPt[i][0]->SetPoint(m,-1,0);
      }
    }	    
    Pt[i]->Rebin();
    Pt[i]->Scale(0.5);
    for (Int_t m = 8; m < 16; m++) {
      Int_t n = m/2 + 1;
      if (!(m%2) && Pt[i]->GetBinError(n) < 3){
	flowPt[i][0]->SetPoint(m, Pt[i]->GetBinCenter(n),
				  flip * Pt[i]->GetBinContent(n));
	flowPt[i][0]->SetPointError(m, 0., Pt[i]->GetBinError(n));
      } else {
	flowPt[i][0]->SetPoint(m,-1,0);
      }
    }	    
    Pt[i]->Rebin();
    Pt[i]->Scale(0.5);
    for (Int_t m = 16; m < 24; m++) {
      Int_t n = m/4 + 1;
      if (!(m%4) && Pt[i]->GetBinError(n) < 3){
	flowPt[i][0]->SetPoint(m, Pt[i]->GetBinCenter(n),
				  flip * Pt[i]->GetBinContent(n));
	flowPt[i][0]->SetPointError(m, 0.,
				       Pt[i]->GetBinError(n));
      } else {
	flowPt[i][0]->SetPoint(m,-1,0);
      }
    }
    Pt[i]->Rebin();
    Pt[i]->Scale(0.5);
    for (Int_t m = 24; m < 40; m++) {
      Int_t n = m/8 + 1;
      if (!(m%8) && Pt[i]->GetBinError(n) < 3){
	flowPt[i][0]->SetPoint(m, Pt[i]->GetBinCenter(n),
				  flip * Pt[i]->GetBinContent(n));
	flowPt[i][0]->SetPointError(m, 0., 
				       Pt[i]->GetBinError(n));
      } else {
	flowPt[i][0]->SetPoint(m,-1,0);
      }
    }
  }    
  } 
  
  // Create Canvas
  TCanvas* canvas = new TCanvas("ZeroCross", "ZeroCross", 100, 100, 840, 600);
  canvas->cd();
  //TLegend* legend = new TLegend();

  // Draw Graphs

  // Minimum Bias vs. y -------------------- ------------------------
  Char_t title[255] = "Minimum Bias ";
  flowY[0][0]->SetTitle(strcat(title, part));
  flowY[0][0]->SetMarkerStyle(kFullCircle);
  flowY[0][1]->SetMarkerStyle(kOpenCircle);
  flowY[1][0]->SetMarkerStyle(kFullSquare);
  flowY[1][1]->SetMarkerStyle(kOpenSquare);
  flowY[0][0]->SetMarkerColor(kRed);
  flowY[0][1]->SetMarkerColor(kRed);
  flowY[1][0]->SetMarkerColor(kGreen);
  flowY[1][1]->SetMarkerColor(kGreen);
  flowY[0][0]->SetMarkerSize(markerSize);
  flowY[0][1]->SetMarkerSize(markerSize);
  flowY[1][0]->SetMarkerSize(markerSize);
  flowY[1][1]->SetMarkerSize(markerSize);

  //canvas->Clear();
  TH1F* hist = new TH1F(title, title, 10, 1, 5);
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
  flowY[0][0]->Fit("f3", "R");
  flowY[0][0]->Draw("P");
  flowY[0][1]->Fit("r3", "R");
  flowY[0][1]->Draw("P");
  if (pion) {
    flowY[1][0]->Fit("fc3", "R");
    flowY[1][1]->Fit("rc3", "R");
  } else {
    flowY[1][0]->Fit("fc3", "R");
    flowY[1][1]->Fit("rc3", "R");
  }
  flowY[1][0]->Draw("P");
  flowY[1][1]->Draw("P");

  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();
  
  TLatex l; 
  l.SetNDC();
  l.SetTextColor(kBlue); 
  l.SetTextSize(0.06); 
    l.SetTextAngle(90); 
  l.DrawLatex(0.07,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.05,"rapidity" );
  l.SetTextSize(0.04); 
  l.DrawLatex(0.2,0.2,"p_{t} < 2 GeV/c");
  l.SetTextSize(0.06); 

  l.SetTextColor(kRed); 
  if (pion) {
    l.DrawLatex(0.57,0.25,"with corr."); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.57,0.6,"no corr."); 
  } else {
    l.DrawLatex(0.55,0.45,"with corr."); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.6,0.7,"no corr."); 
  }

  Char_t outfile[255];
  sprintf(outfile, "%smb_y.%s", outDirName->Data(), pstype);
  canvas->Print(outfile, pstype);
  if (!Pause()) return;

  // Minimum Bias versus pt -------------------- ---------------------
  sprintf(title, "Minimum Bias ");
  flowPt[0][0]->SetTitle(strcat(title, part));
  flowPt[0][0]->SetMarkerStyle(kFullCircle);
  flowPt[1][0]->SetMarkerStyle(kFullSquare);
  flowPt[0][0]->SetMarkerColor(kRed);
  flowPt[1][0]->SetMarkerColor(kGreen);
  flowPt[0][0]->SetMarkerSize(markerSize);
  flowPt[1][0]->SetMarkerSize(markerSize);

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

  if (pion) {
    //flowPt[0][0]->Draw("PC");
    flowPt[0][0]->Fit("o2", "R");
    flowPt[0][0]->Fit("n2", "R+");
    flowPt[0][0]->Draw("P");
    flowPt[1][0]->Fit("o2", "R");
    flowPt[1][0]->Fit("n2", "R+");
    flowPt[1][0]->Draw("P");
  } else {
    flowPt[0][0]->Fit("p2", "R");
    //flowPt[0][0]->Fit("n2", "R+");
    flowPt[0][0]->Draw("P");
  }

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
    l.DrawLatex(0.6,0.23,"with corr."); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.6,0.57,"no corr.");
  } else {
    l.DrawLatex(0.7,0.4,"with corr."); 
    l.SetTextColor(kGreen); 
    l.DrawLatex(0.7,0.65,"no corr.");
  } 
  
  sprintf(outfile, "%smb_pt.%s", outDirName->Data(), pstype);
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
