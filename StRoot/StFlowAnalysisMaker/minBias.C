///////////////////////////////////////////////////////////////////////////////
//
// $Id: minBias.C,v 1.3 2000/09/29 22:53:17 posk Exp $
//
// Author:       Sergei Voloshin and Art Poskanzer, Sep. 2000
// Description:  Macro to add centrality-selected histograms together with
//               weighting to make a minimum bias result.
//               Uses a set of histograms with different centrality
//               starting with anaXX.root given by first run number XX.
//               First Run Number appended to "ana" is entered in the box.
//               Default selN = 1 and harN = 2.
//               First time type .x minBias.C() to see the menu.
//               After the first execution, just type minBias(N) .
//               A negative N plots all pages starting with page N.
//               Place a symbolic link to this file in StRoot/macros/analysis .
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: minBias.C,v $
// Revision 1.3  2000/09/29 22:53:17  posk
// More histograms.
//
// Revision 1.2  2000/09/26 20:54:11  posk
// Updated documentation.
//
// Revision 1.1  2000/09/26 00:19:43  posk
// New macro to add centrality-selected histograms with proper weights, to make
// minimum bias histogram.
//
//
//
///////////////////////////////////////////////////////////////////////////////

Int_t runNumber      = 0;
const Int_t nCens    = 8;
TFile* histFile[nCens];

TCanvas* minBias(Int_t pageNumber=0, Int_t selN=1, Int_t harN=2) {

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
  
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();
  
  char  runName[6];
  char  fileName[30];
  int   canvasWidth = 780, canvasHeight = 600; 
  
  // names of histograms made by StFlowAnalysisMaker
  const char* baseName[] = { "Flow_Phi_Flat",
			     "Flow_Psi",
			     "Flow_q",
			     "Flow_Psi_Sub_Corr",
			     "Flow_Psi_Sub_Corr_Diff",
			     "Flow_vEta",
			     "Flow_vPt"};
  const int nNames = sizeof(baseName) / sizeof(char*);
  
  // input the first run number
  if (runNumber == 0) {
    cout << "     first run number? ";
    cin >> runNumber;
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " first run name = " << runName << endl;
    
    // open the files
    for (int n = 0; n < nCens; n++) {
      sprintf(fileName, "ana%2d.root", runNumber + n);
      cout << " file name = " << fileName << endl;
      histFile[n] = new TFile(fileName);
    }
  }
  
  // input the page number
  while (pageNumber <= 0 || pageNumber > nNames) {
    if (pageNumber < 0) {                                // plot all
      minBiasAll(nNames, selN, harN, -pageNumber);
      return;
    }
    cout << "-1: \t All" << endl;                        // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << baseName[i] << endl;
    }
    cout << "     page number? ";
    cin >> pageNumber;
  }
  pageNumber--;
  
  // construct histName
  char sel[2];
  sprintf(sel,"%d",selN);
  char har[2];
  sprintf(har,"%d",harN);
  TString* histName = new TString(baseName[pageNumber]);
  histName->Append("_Sel");
  histName->Append(*sel);
  histName->Append("_Har");
  histName->Append(*har);
  cout << " input hist name= " << histName->Data() << endl;
  
  // make the graph page
  TCanvas* c = new TCanvas(baseName[pageNumber], baseName[pageNumber],
			   canvasWidth, canvasHeight);
  c->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  
  // get the histograms
  TH1* hist[nCens];
  for (int cen = 0; cen < nCens; cen++) {
    hist[cen] = (TH1*)histFile[cen].Get(histName->Data());
    if (!hist[cen]) {
      cout << "### Can't find histogram " << histName->Data() << endl;
      return;
    }
  }

  // book the minBias histogram
  TString* meanHistName = new TString(histName->Data());
  meanHistName->Append("_MinBias");
  cout << " output hist name= " << meanHistName->Data() << endl;
  int nBins    = hist[0]->GetNbinsX();
  TAxis* xAxis = hist[0]->GetXaxis();
  float xMin   = xAxis->GetXmin();
  float xMax   = xAxis->GetXmax();
  char* xTitle = xAxis->GetTitle();
  char* yTitle = hist[0]->GetYaxis()->GetTitle();
  TH1F* meanHist = new TH1F(meanHistName->Data(), meanHistName->Data(), 
			    nBins, xMin, xMax);
  meanHist->SetXTitle(xTitle);
  meanHist->SetYTitle(yTitle);
  
  // loop over the bins
  float content, error, errorSq;
  float meanContent, meanError, weight;
  for (int bin = 0; bin < nBins; bin++) {
    meanContent = 0.;
    meanError   = 0.;
    weight      = 0.;
    for (int cen = 0; cen < nCens; cen++) {
      content = hist[cen]->GetBinContent(bin);
      error   = hist[cen]->GetBinError(bin);
      errorSq = error * error;
      if (errorSq > 0.) {
	meanContent += content / errorSq;
	weight      += 1. / errorSq;
      }
    }
    if (weight > 0.) {
      meanContent /= weight;
      meanError = sqrt(1. / weight);
      meanHist->SetBinContent(bin, meanContent);
      meanHist->SetBinError(bin, meanError);
    }
  }

  // make the plot
  gStyle->SetOptStat(100100);
  meanHist->Draw();
  gPad->Update();

  delete histName;
  delete meanHistName;
  
  return c;
}

void minBiasAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  char temp[3];
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = minBias(i, selN, harN);
    c->Update();
    cout << "save? y/[n]" << endl;
    fgets(temp, sizeof(temp), stdin);
    if (strstr(temp,"y")!=0) c->Print(".ps");
    c->Delete();
  }
  cout << "  Done" << endl;
}
