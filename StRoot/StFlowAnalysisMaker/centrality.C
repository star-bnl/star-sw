///////////////////////////////////////////////////////////////////////////////
//
// $Id: centrality.C,v 1.2 2001/11/09 21:14:59 posk Exp $
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
Int_t runNumber      = 0;
const Int_t nCens    = 8;
TFile* histFile[nCens];
char tmp[10];

TCanvas* centrality(Int_t pageNumber=0, Int_t selN=1, Int_t harN=2){

  char  runName[6];
  const int bins = nCens + 2;
  int canvasWidth = 780, canvasHeight = 600;             // landscape
  
  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowAnalysisMaker
  const char* baseName[] = { "Flow_Cos",
			     "Flow_Res",
			     "Flow_vObs",
			     "Flow_v" };
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
      return c;
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
  float nMax= 880.;
  float xMin[] = {0.,20.,100.,180.,270.,360.,460.,560.,660.,870.,950.};
  float x[bins+1];
  for (int i=0; i<=bins; i++){ x[i]= xMin[i]/nMax; }   // normalize
  TH1F* histCen = new TH1F(histCenName->Data(), histCenName->Data(), bins, x);
  TH1* hist = (TH1*)histFile[0].Get(histName->Data());
  char* yTitle = hist->GetYaxis()->GetTitle();


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
  
  // make the plots
  double content;
  double error;
  for (int i = 0; i < nCens; i++) {
    int cen = i + 2;
    TH1* hist = (TH1*)histFile[i].Get(histName->Data());
    if (!hist) {
      cout << "### Can't find histogram " << histName->Data() << endl;
      return c;
    }

    // get and put contents
    content = hist->GetBinContent(harN);
    error   = hist->GetBinError(harN);
    cout << "centrality= " << cen-1 << " content= " << setprecision(3) <<
      content << " error= "  << setprecision(2)  << error << endl;
    histCen->SetBinContent(cen, content);
    histCen->SetBinError(cen, error);
  }
  gStyle->SetOptStat(0);
  histCen->Draw();

  //label the axes
  TLatex l;
  l.SetNDC();
  l.SetTextAlign(12);
  l.SetTextSize(0.07);
  l.SetTextColor(4);
  l.SetIndiceSize(0.5); 
  l.DrawLatex(0.75,0.07,"n_{ch}/n_{max}" );
  if (pageNumber==0) {
    l.SetTextAngle(90);
    if (harN == 2) {
      l.DrawLatex(0.07,0.4,"<cos[2(#Psi_{2}^{a}-#Psi_{2}^{b})]>" );
    } else {
      l.DrawLatex(0.07,0.4,"<cos(#Psi^{a}-#Psi^{b})>" );
    }
  } else if (pageNumber==3 && harN == 2){
    l.SetTextAngle(90);
    l.DrawLatex(0.1,0.7,"v_{2} (%)" );
  } else {
    histCen->SetYTitle(yTitle);
  }

  TLine* lineZeroCen = new TLine(0., 0., x[bins], 0.);
  lineZeroCen->Draw();
  gPad->Update();

  delete histName;
  delete histCenName;
  
  return c;
}


void centralityAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = centrality(i, selN, harN);
    c->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) c->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
    c->Delete();
  }
  cout << "  Done" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: centrality.C,v $
// Revision 1.2  2001/11/09 21:14:59  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.1  2001/05/22 20:07:56  posk
// Plots vs. centrality.
//
//
///////////////////////////////////////////////////////////////////////////////
