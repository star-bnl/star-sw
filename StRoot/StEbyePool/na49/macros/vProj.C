///////////////////////////////////////////////////////////////////////////////
//
// $Id: vProj.C,v 1.3 2001/03/16 22:35:10 posk Exp $
//
// Author:       Art Poskanzer, May 2000
// Description:  Projects v(y,pt) on the y and Pt axes
//                 with cross section weighting.
//               The cross sections come from dNdydPt.C .
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: vProj.C,v $
// Revision 1.3  2001/03/16 22:35:10  posk
// plotGraphs.C makes the final graphs.
//
// Revision 1.2  2001/03/06 17:33:04  posk
// All macros now work.
//
//
// Revision 1.1  2001/02/23 00:58:19  posk
// NA49 version of STAR software.
//
///////////////////////////////////////////////////////////////////////////////

gROOT->Reset();

bool   crossSection = kTRUE;
//bool   crossSection = kFALSE; // should be same as profile hists

float  yLow  = 3.;
float  yLowY = 2.;
float  yUp   = 5.;
float  ptUp  = 2.;

TFile*   histFile;
TH2*     hist;
TH2*     yieldHist;
TH1F*    projX;
TH1F*    projY;
TH1F*    yieldY;
TH1F*    yieldPt;
TAxis*   xAxis;
TAxis*   yAxis;
int      xBins;
int      yBins;
int      bins;
float    xMin;
float    xMax;
float    yMin;
float    yMax;
double   yCM = 2.92;
char     temp[10];
int      runNumber = 0;
char     runName[6];
char     fileName[30];
int      nCens = 6;
int      cent;
TCanvas* can;

void vProj(int sel=2, int har=2) {
    
  // input the run number
  if (runNumber == 0) {
    cout << "     run number? ";
    fgets(temp, sizeof(temp), stdin);
    runNumber = atoi(temp);
    sprintf(runName, "ana%2d", runNumber);             // add ana prefix
    cout << " run name = " << runName << endl;
    sprintf(fileName, "ana%d.root", runNumber);
    cent = runNumber % 10;                             // unit digit = cent
    if (cent <= 0 || cent > nCens) {
      cout << " Not valid centrality = " << cent << ", using 3" << endl;
      cent = 3;
    }
    cout << " Centrality = " << cent << endl;
  }

  // set style
  gROOT->SetStyle("Bold");                              
  gROOT->ForceStyle();
  gStyle->SetOptStat(0);

  // read file and get hists
  Init(sel, har);

  // plot 2D
  NewCanvas(hist->GetName());
  hist->Draw("COLZ");
  if (!Pause()) return;

  if (!crossSection) {
    NewCanvas(yieldHist->GetName());
    yieldHist->Draw("COLZ");
    if (!Pause()) return;
  }

  gROOT->LoadMacro("dNdydPt.C");

  // fill and plot rapidity projection
  double y;
  double pt;
  double yield;
  double yieldSum;
  double v;
  double vSum;
  double err2Sum;
  float  content;
  float  error;
  if (crossSection) cout << "Please wait" << endl;
  for (int xBin=1; xBin<=xBins; xBin++) {
    yieldSum = 0.;
    vSum     = 0.;
    err2Sum  = 0.;
    content  = 0.;
    error    = 0.;
    y = xAxis->GetBinCenter(xBin);
    if (y > yLowY && y < yUp) {
      for (int yBin=1; yBin<=yBins; yBin++) {
	pt = yAxis->GetBinCenter(yBin);
	if (pt < ptUp) {
	  if (crossSection) {
	    yield     = dNdydPt(0, y - yCM, pt, cent) +
	      dNdydPt(1, y - yCM, pt, cent);               // pi+ + pi-
	  } else {
	    yield = yieldHist->GetCellContent(xBin, yBin);
	  }
	  if(hist->GetCellContent(xBin, yBin) != 0.0)
	    yieldSum += yield;
	  vSum     += yield * hist->GetCellContent(xBin, yBin);
	  err2Sum  += pow(yield * hist->GetCellError(xBin, yBin), 2.);
	}
      }
      if (yieldSum) {
	content = vSum / yieldSum;
	error   = sqrt(err2Sum) / yieldSum;
      }
      yieldY->SetBinContent(xBin, yieldSum);
      projX->SetBinContent(xBin, content);
      projX->SetBinError(xBin, error);
    }
  }
  
  NewCanvas(yieldY->GetName());
  yieldY->Draw();
  TLine* lineYcm     = new TLine(yCM, 0., yCM, 10000.);
  lineYcm ->Draw();
  if (!Pause()) return;
  delete lineYcm;

  NewCanvas(projX->GetName());
  ReBin(projX);
  projX->SetXTitle("Rapidity");
  projX->Draw();
  TLine* lineZeroY = new TLine(xMin, 0., xMax, 0.);
  TLine* lineYcm     = new TLine(yCM, -10., yCM, 10.);
  lineZeroY->Draw();
  lineYcm    ->Draw();
  VectorOut(projX, xBins/2);
  if (!Pause()) return;

  //fill and plot pt projection
  if (crossSection) cout << "Please wait" << endl;
  for (int yBin=1; yBin<=yBins; yBin++) {
    yieldSum = 0.;
    vSum     = 0.;
    err2Sum  = 0.;
    content  = 0.;
    error    = 0.;
    pt = yAxis->GetBinCenter(yBin);
    for (int xBin=1; xBin<=xBins; xBin++) {
      y = xAxis->GetBinCenter(xBin);
      if (y > yLow && y < yUp) {
	v = hist->GetCellContent(xBin, yBin);
	if (har % 2 == 1 && y < 0.) v *= -1; // backward particles for odd har
	if (crossSection) {
	  yield     = dNdydPt(0, y - yCM, pt, cent) +
	    dNdydPt(1, y - yCM, pt, cent);
	} else {
	  yield = yieldHist->GetCellContent(xBin, yBin);
	}
	if(v != 0.0)
	  yieldSum += yield;
	vSum     += yield * v;
	err2Sum  += pow(yield * hist->GetCellError(xBin, yBin), 2.);
      }
    }
    if (yieldSum) {
      content = vSum / yieldSum;
      error   = sqrt(err2Sum) / yieldSum;
    }
    yieldPt->SetBinContent(yBin, yieldSum);
    projY->SetBinContent(yBin, content);
    projY->SetBinError(yBin, error);
  }

  NewCanvas(yieldPt->GetName());
  gPad->SetLogy(1);
  yieldPt->Draw();
  if (!Pause()) return;
  gPad->SetLogy(0);

  NewCanvas(projY->GetName());
  ReBin(projY);
  projY->SetXTitle("Pt (GeV)");
  projY->Draw();
  TLine* lineZeroPt = new TLine(yMin, 0., yMax, 0.);
  lineZeroPt->Draw();
  VectorOut(projY, yBins/2);
  if (!Pause()) return;
}

void Init(int sel, int har) {
  if (!histFile) histFile = new TFile(fileName, "READ");
  if (!histFile) {
    cout << "### Can't find file " << fileName << endl;
    return;
  }

  // Convert integers to text
  char selText[2];
  sprintf(selText, "%d", sel);
  char harText[2];
  sprintf(harText, "%d", har);

  // Get the 2D yield histogram
  TString* histName = new TString("Flow_YieldPart2D");
  yieldHist = (TH2*)histFile->Get(histName->Data());
  if (!yieldHist) {
    cout << "### Can't find histogram " << histName->Data() << endl;
    return;
  }
  delete histName;
  
  // Get the 2D v histogram
  TString* histName = new TString("Flow_v2D_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  hist = (TH2*)histFile->Get(histName->Data());
  if (!hist) {
    cout << "### Can't find histogram " << histName->Data() << endl;
    return;
  }
  hist->SetMaximum(20.);
  hist->SetMinimum(-20.);
  delete histName;

  // Create the 1D v histograms
  xAxis = hist->GetXaxis();
  xBins = xAxis->GetNbins();
  xMin  = xAxis->GetXmin();
  xMax  = xAxis->GetXmax();
  histName = new TString("Flow_vY_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  projX = new TH1F(histName->Data(), histName->Data(), xBins, xMin, xMax);
  projX->SetXTitle("Rapidity");
  projX->SetYTitle("Flow (%)");
  projX->SetMaximum(10.);
  projX->SetMinimum(-10.);
  delete histName;

  yAxis = hist->GetYaxis();
  yBins = yAxis->GetNbins();
  yMin  = yAxis->GetXmin();
  yMax  = yAxis->GetXmax();
  histName = new TString("Flow_vPt_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  projY = new TH1F(histName->Data(), histName->Data(), yBins, yMin, yMax);
  projY->SetXTitle("Pt (GeV)");
  projY->SetYTitle("Flow (%)");
  projY->SetMaximum(15.);
  projY->SetMinimum(-5.);
  delete histName;

  // Create 1D Yield histograms
  histName = new TString("Flow_YieldY_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  yieldY = new TH1F(histName->Data(), histName->Data(), xBins, xMin, xMax);
  yieldY->SetXTitle("Rapidity");
  yieldY->SetYTitle("Yield");
  delete histName;
  
  histName = new TString("Flow_YieldPt_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  yieldPt = new TH1F(histName->Data(), histName->Data(), yBins, yMin, yMax);
  yieldPt->SetXTitle("Pt (GeV)");
  yieldPt->SetYTitle("Yield");
  delete histName;
  
  return;
}

void NewCanvas(char* canvasName) {

  // delete old canvas
  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases();
  if (cOld) cOld->Delete();
    
  // make new canvas
  can = new TCanvas(canvasName, canvasName, 780, 600);
  can->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  cout << canvasName << endl;

  return;
}

void ReBin(proj) {
  // rebin two channels to one

  proj->Rebin();
  proj->Scale(0.5);

}

void VectorOut(proj, bins) {

  cout<<proj->GetName()<<" x values: { ";
  for (int bin=1; bin<=bins; bin++) {
    float x = proj->GetBinCenter(bin);
    cout<<x<<", ";
  }
  cout<<"}"<<endl<<endl;

  cout<<proj->GetName()<<" content: { ";
  for (int bin=1; bin<=bins; bin++) {
    float content = proj->GetBinContent(bin);
    cout<<content<<", ";
  }
  cout<<"}"<<endl<<endl;

  cout<<proj->GetName()<<" errors: { ";
  for (int bin=1; bin<=bins; bin++) {
    float error = proj->GetBinError(bin);
    cout<<error<<", ";
  }
  cout<<"}"<<endl<<endl;

}

bool Pause() {
  can->Update();
  cout << "save? y/[n], quit? q" << endl;
  fgets(temp, sizeof(temp), stdin);
  if (strstr(temp,"y")!=0) can->Print(".ps");
  else if (strstr(temp,"q")!=0) return kFALSE;

  return kTRUE;
}
