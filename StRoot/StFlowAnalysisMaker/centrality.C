///////////////////////////////////////////////////////////////////////////////
//
// $Id: centrality.C,v 1.3 2003/02/25 19:25:31 posk Exp $
//
// Author:       Art Poskanzer, LBNL, July 2000
// Description:  Macro to plot flow, etc. as a function of centrality.
//               It gets one channel from a profile histogram in a set of files
//               starting with anaXX.root given by first run number XX.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               First time type .x centrality.C() to see the menu.
//               After the first execution, just type centrality(N) .
//               A negative N plots all pages starting with page N.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <iomanip.h>
Int_t runNumber      =  0;
const Int_t nCens    = 10;
TFile* histFile[nCens];
TCanvas* can;
char tmp[10];

TCanvas* centrality(Int_t pageNumber=0, Int_t selN=2, Int_t harN=2){

  char  runName[6];
  const int bins = nCens + 2;
  int canvasWidth = 780, canvasHeight = 600;             // landscape
  
  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();
  gStyle->SetOptStat(kFALSE);

  // names of histograms made by StFlowAnalysisMaker
  const char* baseName[] = { 
    "Flow_Cos",
    "Flow_Res",
    "Flow_vObs",
    "Flow_v",
    "Flow_v_ScalarProd",
    "Flow_Cumul_v_Order2",
    "Flow_Cumul_v_Order4"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);

  // input the first run number
  if (runNumber == 0) {
    cout << "     first run number? ";
    fgets(tmp, sizeof(tmp), stdin);
    runNumber = atoi(tmp);
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " first run name = " << runName << endl;

  // open the files
    for (int n = 0; n < nCens; n++) {
      char  fileName[30];
      sprintf(fileName, "ana%2d.root", runNumber + n);
      cout << " file name = " << fileName << endl;
      histFile[n] = new TFile(fileName);
    }
  }

  // input the page number
  while (pageNumber <= 0 || pageNumber > nNames) {
    if (pageNumber < 0) {                                // plot all
      centralityAll(nNames, selN, harN, -pageNumber);
      return can;
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
  TString* histName = new TString(baseName[pageNumber]);
  histName->Append("_Sel");
  histName->Append(*sel);

  // book histogram
  char har[2];
  sprintf(har,"%d",harN);
  TString* histCenName = new TString(histName->Data());
  histCenName->Append("_Har");
  histCenName->Append(*har);
  cout << " graph name= " << histCenName->Data() << endl;
//   float nMax= 880.;
//   float xMin[] = {0.,20.,100.,180.,270.,360.,460.,560.,660.,870.,950.};
//   float x[bins+1];
//   for (int i=0; i<=bins; i++){ x[i]= xMin[i]/nMax; }   // normalize
//   TH1F* histCen = new TH1F(histCenName->Data(), histCenName->Data(), bins, x);
  TH1F* histCen = new TH1F(histCenName->Data(), histCenName->Data(),
			   nCens, -0.5, nCens-0.5);
  TH1* hist = (TH1*)histFile[0]->Get(histName->Data());
  char* yTitle = hist->GetYaxis()->GetTitle();


  // make the graph page
  can = new TCanvas(baseName[pageNumber], baseName[pageNumber],
			   canvasWidth, canvasHeight);
  can->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  
  // make the plots
  double content;
  double error;
  for (int cen = 0; cen < nCens; cen++) {
    TH1* hist = (TH1*)histFile[cen]->Get(histName->Data());
    if (!hist) {
      cout << "### Can't find histogram " << histName->Data() << endl;
      return can;
    }

    // get and put contents
    content = hist->GetBinContent(harN);
    error   = hist->GetBinError(harN);
    cout << "centrality= " << cen << " content= " << setprecision(3) <<
      content << " error= "  << setprecision(2)  << error << endl;
    histCen->SetBinContent(cen+1, content);
    histCen->SetBinError(cen+1, error);
  }
  histCen->SetMarkerStyle(kFullCircle);
  histCen->SetMarkerColor(kRed);
  histCen->Draw("E1");

  //label the axes
  TLatex l;
  l.SetNDC();
  l.SetTextSize(0.07);
  l.SetTextColor(kBlue);
  //l.DrawLatex(0.75,0.07,"n_{ch}/n_{max}");
  l.DrawLatex(0.65,0.03,"Centrality" );
  l.SetTextAngle(90);
  if (pageNumber==0) {
    if (harN == 2) {
      l.DrawLatex(0.08,0.4,"<cos[2(#Psi_{2}^{a}-#Psi_{2}^{b})]>");
    } else {
      l.DrawLatex(0.08,0.4,"<cos(#Psi^{a}-#Psi^{b})>");
    }
  } else if (pageNumber>=3 && harN == 2) {
    l.DrawLatex(0.08,0.7,"v_{2} (%)" );
  } else {
    histCen->SetYTitle(yTitle);
  }

//   TLine* lineZeroCen = new TLine(0., 0., x[bins], 0.);
  TLine* lineZeroCen = new TLine(-0.5, 0., nCens-0.5, 0.);
  lineZeroCen->Draw();
  gPad->Update();

  delete histName;
  delete histCenName;
  
  return can;
}


void centralityAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    can = centrality(i, selN, harN);
    can->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) can->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
  }
  cout << "  Done" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: centrality.C,v $
// Revision 1.3  2003/02/25 19:25:31  posk
// Improved plotting.
//
// Revision 1.2  2001/11/09 21:14:59  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.1  2001/05/22 20:07:56  posk
// Plots vs. centrality.
//
//
///////////////////////////////////////////////////////////////////////////////
