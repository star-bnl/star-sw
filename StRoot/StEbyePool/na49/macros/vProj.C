// Projects v(y,pt) on the y and Pt axes.
// It calculates the mean weighted by the cross section.
// Art Poskanzer, LBNL,  May '00
Int_t cent = 1;               // centrality

TFile*  histFile;
TH2*    hist;
TH1F*   projX;
TH1F*   projY;
TAxis*  xaxis;
TAxis*  yaxis;
Int_t   xbins;
Int_t   ybins;
Float_t xmin;
Float_t xmax;
Float_t ymin;
Float_t ymax;
char    temp[3];
Int_t   runNumber = 0;
char    runName[6];
TCanvas* can;

void vProj(int sel, int har) {

  // delete old canvas
  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases();
  if (cOld) cOld->Delete();
    
  // input the run number
  if (runNumber == 0) {
    cout << "     run number? ";
    cin >> runNumber;
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " run name = " << runName << endl;
  }

  // set style
  gROOT->SetStyle("Bold");                              
  gROOT->ForceStyle();

  // read file and get hists
  Init(sel, har);

  NewCanvas();

  // plot 2D
  gStyle->SetOptStat(0);
  hist->Draw("COLZ");
  Pause();

  yieldHist->Draw("COLZ");
  Pause();

  gROOT->LoadMacro("dNdydPt.C");

  // fill and plot xproj
  Double_t y;
  Double_t pt;
  Double_t yield;
  Double_t yieldSum;
  Double_t vSum;
  Double_t err2Sum;
  Float_t  content;
  Float_t  error;
  for (Int_t xbin=1; xbin<=xbins; xbin++) {
    yieldSum = 0.;
    vSum     = 0.;
    err2Sum  = 0.;
    content  = 0.;
    error    = 0.;
    y = xaxis->GetBinCenter(xbin);
    for (Int_t ybin=1; ybin<=ybins; ybin++) {
      pt = yaxis->GetBinCenter(ybin);
      yield     = dNdydPt(0, y, pt, cent) + dNdydPt(1, y, pt, cent); // pi+ + pi-
      //yield = yieldHist->GetCellContent(xbin, ybin);
      yieldSum += yield;
      vSum     += yield * hist->GetCellContent(xbin, ybin);
      err2Sum  += pow(yield * hist->GetCellError(xbin, ybin), 2.);
    }
    if (yieldSum) {
      content = vSum / yieldSum;
      error   = sqrt(err2Sum) / yieldSum;
    }
    yieldEta->SetBinContent(xbin, yieldSum);
    projX->SetBinContent(xbin, content);
    projX->SetBinError(xbin, error);
  }

  gStyle->SetOptStat(0);
  yieldEta->Draw();
  Pause();

  projX->Draw();
  TLine* lineZeroEta = new TLine(xmin, 0., xmax, 0.);
  lineZeroEta->Draw();
  Pause();

  //fill and plot yproj
  Double_t v;
  for (Int_t ybin=1; ybin<=ybins; ybin++) {
    yieldSum = 0.;
    vSum     = 0.;
    err2Sum  = 0.;
    content  = 0.;
    error    = 0.;
    pt = yaxis->GetBinCenter(ybin);
    for (Int_t xbin=1; xbin<=xbins; xbin++) {
      y = xaxis->GetBinCenter(xbin);
      yield     = dNdydPt(0, y, pt, cent) + dNdydPt(1, y, pt, cent);
      //yield = yieldHist->GetCellContent(xbin, ybin);
      yieldSum += yield;
      v = hist->GetCellContent(xbin, ybin);
      if (har % 2 == 1 && y < 0.) v *= -1; // backward particles for odd harmonic
      vSum     += yield * v;
      err2Sum  += pow(yield * hist->GetCellError(xbin, ybin), 2.);
    }
    if (yieldSum) {
      content = vSum / yieldSum;
      error   = sqrt(err2Sum) / yieldSum;
    }
    yieldPt->SetBinContent(ybin, yieldSum);
    projY->SetBinContent(ybin, content);
    projY->SetBinError(ybin, error);
  }

  gStyle->SetOptStat(0);
  gPad->SetLogy();
  yieldPt->Draw();
  Pause();

  gPad->SetLiny();
  projY->Draw();
  TLine* lineZeroPt = new TLine(ymin, 0., ymax, 0.);
  lineZeroPt->Draw();
  Pause();
}

void Init(int sel, int har) {
  if (!histFile) histFile = new TFile("flow.hist.root", "READ");
  if (!histFile) {
    cout << "### Can't find file" << endl;
    return;
  }

  // Convert integers to text
  char selText[2];
  sprintf(selText, "%d", sel);
  char harText[2];
  sprintf(harText, "%d", har);

  // Get the 2D yield histogram
  TString* histName = new TString("Flow_Yield2D_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  yieldHist = (TH2*)histFile.Get(histName->Data());
  delete histName;
  if (!yieldHist) {
    cout << "### Can't find histogram " << histName->Data() << endl;
    return;
  }

  // Get the 2D v histogram
  TString* histName = new TString("Flow_v2D_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  hist = (TH2*)histFile.Get(histName->Data());
  delete histName;
  if (!hist) {
    cout << "### Can't find histogram " << histName->Data() << endl;
    return;
  }

  // Create the 1D v histograms
  xaxis = hist->GetXaxis();
  xbins = xaxis->GetNbins();
  xmin  = xaxis->GetXmin();
  xmax  = xaxis->GetXmax();
  histName = new TString("Flow_vEta_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  projX = new TH1F(histName->Data(), histName->Data(), xbins, xmin, xmax);
  projX->SetXTitle("Pseudorapidity");
  projX->SetYTitle("Flow (%)");
  delete histName;

  yaxis = hist->GetYaxis();
  ybins = yaxis->GetNbins();
  ymin  = yaxis->GetXmin();
  ymax  = yaxis->GetXmax();
  histName = new TString("Flow_vPt_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  projY = new TH1F(histName->Data(), histName->Data(), ybins, ymin, ymax);
  projY->SetXTitle("Pt (GeV)");
  projY->SetYTitle("Flow (%)");
  delete histName;

  // Create 1D Yield histograms
  histName = new TString("Flow_YieldEta_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  yieldEta = new TH1F(histName->Data(), histName->Data(), xbins, xmin, xmax);
  yieldEta->SetXTitle("Pseudorapidity");
  yieldEta->SetYTitle("Yield");
  delete histName;

  histName = new TString("Flow_YieldPt_Sel");
  histName->Append(*selText);
  histName->Append("_Har");
  histName->Append(*harText);
  yieldPt = new TH1F(histName->Data(), histName->Data(), ybins, ymin, ymax);
  yieldPt->SetXTitle("Pt (GeV)");
  yieldPt->SetYTitle("Yield");
  delete histName;

  return;
}

void NewCanvas() {
  // make new canvas
  can = new TCanvas("vProjections", "v Projections", 780, 600);
  can->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
}

void Pause() {
  can->Update();
  //return;                       // no pause
  cout << "save? y/[n]" << endl;
  fgets(temp, sizeof(temp), stdin);
  if (strstr(temp,"y")!=0) can->Print(".ps");

  return;
}
