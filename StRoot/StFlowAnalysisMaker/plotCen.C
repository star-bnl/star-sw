///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotCen.C,v 1.5 2000/09/29 22:53:19 posk Exp $
//
// Author:       Art Poskanzer, LBNL, July 2000
// Description:  Macro to plot histograms made by StFlowAnalysisMaker.
//               Plots a set of histograms with different centrality
//               starting with anaXX.root given by first run number XX.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               Default selN = 1 and harN = 2.
//               First time type .x plotCen.C() to see the menu.
//               After the first execution, just type plotCen(N) .
//               A negative N plots all pages starting with page N.
//               Place a symbolic link to this file in StRoot/macros/analysis .
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: plotCen.C,v $
// Revision 1.5  2000/09/29 22:53:19  posk
// More histograms.
//
// Revision 1.4  2000/09/26 20:54:13  posk
// Updated documentation.
//
// Revision 1.3  2000/09/15 22:52:56  posk
// Added Pt weighting for event plane calculation.
//
// Revision 1.2  2000/09/07 16:42:09  posk
// Updated list of histograms.
//
// Revision 1.1  2000/08/31 18:50:33  posk
// Added plotCen.C to plot from a series of files with different centralities.
//
//
///////////////////////////////////////////////////////////////////////////////

const Int_t nCens    = 8;
const Float_t twopi  = 2. * 3.1416;
const Float_t etaMax = 1.5;
Int_t runNumber      = 0;
char  runName[6];
char  fileName[30];
char  histTitle[30];
TFile* histFile[nCens];
int canvasWidth = 600, canvasHeight = 780;             // portrait
int columns = 2;
int rows    = nCens/columns;
int pads    = rows*columns;

TCanvas* plotCen(Int_t pageNumber=0, Int_t selN=1, Int_t harN=2){

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char* baseName[] = { "Flow_Res_Sel",
			     "Flow_Charge",
			     "Flow_Dca",
			     "Flow_DcaGlobal",
			     "Flow_Chi2",
			     "Flow_FitPts",
			     "Flow_MaxPts",
			     "Flow_FitOverMax",
			     "Flow_Mult",
			     "Flow_OrigMult",
			     "Flow_MultOverOrig",
			     "Flow_MultEta",
			     "Flow_MultPart",
			     "Flow_VertexZ",
			     "Flow_VertexXY2D",
			     "Flow_EtaSymVerZ2D",
			     "Flow_EtaSym",
			     "Flow_CTBvsZDC2D",
			     "Flow_MeanDedx2D",
			     //"Flow_EtaPtPhi3D",
			     "Flow_EtaPtPhi2D.PhiEta",
                             "Flow_EtaPtPhi2D.PhiPt",
  			     "Flow_YieldAll2D",
  			     "Flow_YieldAll.Eta",
  			     "Flow_YieldAll.Pt",
  			     "Flow_YieldPart2D",
  			     "Flow_YieldPart.Eta",
  			     "Flow_YieldPart.Pt",
  			     "Flow_PidPiPlusPart",
  			     "Flow_PidPiMinusPart",
  			     "Flow_PidProtonPart",
  			     "Flow_PidAntiProtonPart",
  			     "Flow_PidKplusPart",
  			     "Flow_PidKminusPart",
  			     "Flow_PidDeuteronPart",
  			     "Flow_PidAntiDeuteronPart",
  			     "Flow_PidElectronPart",
  			     "Flow_PidPositronPart",
  			     "Flow_PidMult",
  			     "Flow_Cent",
   			     //"Flow_Bin_Eta",
   			     //"Flow_Bin_Pt",
                             "Flow_CosPhiLab",
			     "Flow_Phi_Sel",
			     "Flow_Phi_Weight_Sel",
			     "Flow_Phi_Flat_Sel",
			     "Flow_Psi_Subs",
			     "Flow_Psi_Sel",
			     "Flow_Mul_Sel",
			     "Flow_MeanPt_Sel",
			     "Flow_q_Sel",
			     "Flow_Psi_Sub_Corr_Sel",
			     "Flow_Psi_Sub_Corr_Diff_Sel",
			     "Flow_Phi_Corr_Sel",
			     "Flow_Yield2D_Sel",
			     "Flow_Yield.Eta_Sel",
			     "Flow_Yield.Pt_Sel",
			     "Flow_vObs2D_Sel",
			     "Flow_vObsEta_Sel",
			     "Flow_vObsPt_Sel",
			     "Flow_v2D_Sel",
			     "Flow_vEta_Sel",
			     "Flow_vPt_Sel"};
  const int nNames = sizeof(baseName) / sizeof(char*);
  const int nSingles = 39 + 1;

  // construct array of short names
  char* shortName[] = new char*[nNames];
  for (int n = 0; n < nNames; n++) {
    shortName[n] = new char[30];
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"_Sel");
    if (cp) *cp = '\0';                                  // truncate
  }

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
  while (pageNumber <= nSingles || pageNumber > nNames) {
    if (pageNumber < 0) {                                // plot all
      plotCenAll(nNames, selN, harN, -pageNumber);
      return;
    }
    if (pageNumber > 0 && pageNumber <= nSingles) {      // plot singles
      if (pageNumber == 1) {
	char sel[2];
	sprintf(sel,"%d",selN);
	strcat(shortName[pageNumber-1],"_Sel");
	strcat(shortName[pageNumber-1],sel);
      cout << shortName[pageNumber-1] << endl;
      }
      TCanvas* c = plotCenSingles(shortName[pageNumber-1]);
      return c;
    }
    cout << "-1: \t All" << endl;                        // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << shortName[i] << endl;
    }
    cout << "     page number? ";
    cin >> pageNumber;
  }
  pageNumber--;

  // set constants
  float qMax    =   3.5;
  float phiMax  = twopi; 
  int   n_qBins =    50;
  TString* histProjName = NULL;

  char sel[2];
  sprintf(sel,"%d",selN);
  char har[2];
  sprintf(har,"%d",harN);
  char* temp = new char[30];                       // construct histName
  strcpy(temp,shortName[pageNumber]);
  char* cproj = strstr(temp,".");
  if (cproj) {                                     // a projection
    *cproj = '\0';                                 // remove from "." on
    strcat(temp,"2D_Sel");
    TString* histName = new TString(temp);
    histProjName = new TString(baseName[pageNumber]);
    histProjName->Append(*sel);
    histProjName->Append("_Har");
    histProjName->Append(*har);
  } else {
    TString* histName = new TString(baseName[pageNumber]);
  }
  histName->Append(*sel);
  histName->Append("_Har");
  histName->Append(*har);
  cout << " graph name= " << histName->Data() << endl;

  // make the graph page
  TCanvas* c = new TCanvas(shortName[pageNumber],shortName[pageNumber],
			   canvasWidth,canvasHeight);
  c->ToggleEventStatus();
  TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,histName->Data());
  title->Draw();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();

  // make the plots
  graphPad->Divide(columns,rows);
  TLine* lineZeroEta = new TLine(-etaMax, 0., etaMax, 0.);
  TLine* lineOnePhi  = new TLine(0., 1., phiMax, 1.);
  for (int i = 0; i < pads; i++) {
    int fileN = i;                           // file number
    int padN = fileN + 1;                    // pad number
    cout << "centrality= " << padN << endl;
    if (histProjName) {
      TH2* hist = (TH2*)histFile[fileN].Get(histName->Data());
    } else {
      TH1* hist = (TH1*)histFile[fileN].Get(histName->Data());
    }
    if (!hist) {
      cout << "### Can't find histogram " << histName->Data() << endl;
      return;
    }
    sprintf(histTitle,"Centrality %d",padN);
    hist->SetTitle(histTitle);
    graphPad->cd(padN);
    if (strstr(shortName[pageNumber],"2D")!=0) {          // 2D
      if (strstr(shortName[pageNumber],"_v")!=0) {
	hist->SetMaximum(20.);
	hist->SetMinimum(-20.);
	gStyle->SetOptStat(0);
      } else {
	gStyle->SetOptStat(10);
      }
      hist->Draw("COLZ");
    } else if (strstr(shortName[pageNumber],".Eta")!=0) { // 2D X projection
      TH1D* projX = hist->ProjectionX(histName->Data(), 0, 9999, "E");
      projX->SetName(histProjName->Data());
      char* xTitle = hist->GetXaxis()->GetTitle();
      projX->SetXTitle(xTitle);
      projX->SetYTitle("Counts");
      gStyle->SetOptStat(10);
      if (projX) projX->Draw("H");
      lineZeroEta->Draw();
    } else if (strstr(shortName[pageNumber],".Pt")!=0) {  // 2D Y projection
      TH1D* projY = hist->ProjectionY(histName->Data(), 0, 9999, "E");
      projY->SetName(histProjName->Data());
      projY->SetXTitle("Pt (GeV)");
      projY->SetYTitle("Counts");
      gPad->SetLogy();
      gStyle->SetOptStat(100110);
      if (projY) projY->Draw("H");
    } else if (strstr(shortName[pageNumber],"Corr")!=0) { // azimuthal corr.
      float norm = (float)(hist->GetNbinsX()) / hist->Integral(); 
      cout << "  Normalized by: " << norm << endl;
      hist->Scale(norm);                           // normalize height to one
      if (strstr(shortName[pageNumber],"Sub")!=0) { 
	TF1* funcCos1 = new TF1("funcCos1",
				"1+[0]*2/100*cos([1]*x)", 0., twopi);
	funcCos1->SetParNames("k=1", "har");
	if (strstr(shortName[pageNumber],"Diff")!=0) {
	  funcCos1->SetParameters(0, harN+1);             // initial values
	} else {
	  funcCos1->SetParameters(0, harN);               // initial values
	}
	funcCos1->SetParLimits(1, 1, 1);                  // har is fixed
	hist->Fit("funcCos1");
	delete funcCos1;
      } else {
	TF1* funcCos2 = new TF1("funcCos2",
	    "1+[0]*2/100*cos([2]*x)+[1]*2/100*cos(([2]+1)*x)", 0., twopi);
	funcCos2->SetParNames("k=1", "k=2", "har");
	funcCos2->SetParameters(0, 0, harN);              // initial values
	funcCos2->SetParLimits(2, 1, 1);                  // har is fixed
	hist->Fit("funcCos2");
	delete funcCos2;
      }
      if (strstr(shortName[pageNumber],"Phi")!=0)
	hist->SetMinimum(0.9*(hist->GetMinimum()));
      gStyle->SetOptStat(10);
      gStyle->SetOptFit(111);
      hist->Draw("E1");
    } else if (strstr(shortName[pageNumber],"_q")!=0) {     // q distibution
      double area = hist->Integral() * qMax / (float)n_qBins; 
      cout << "  Area = " << area << endl;
      TF1* func_q = new TF1("func_q", "[0]*2.*x*exp(-x*x)", 0., qMax);
      func_q->SetParameter(0, area);
      gStyle->SetOptStat(100110);
      gStyle->SetOptFit(111);
      hist->Draw("E1");
      func_q->SetLineStyle(kDotted);
      func_q->Draw("same");
    } else if (strstr(shortName[pageNumber],"Phi")!=0) {    // Phi distibutions
      hist->SetMinimum(0.9*(hist->GetMinimum()));
      if (strstr(shortName[pageNumber],"Weight")!=0) {
	gStyle->SetOptStat(0);
	hist->Draw(); 
	lineOnePhi->Draw();
      } else {
	gStyle->SetOptStat(10);
	hist->Draw(); 
      }
    } else if (strstr(shortName[pageNumber],"Psi")!=0) {    // Psi distibutions
      gStyle->SetOptStat(10);
      hist->Draw("E1"); 
    } else if (strstr(shortName[pageNumber],"Eta")!=0) {    // Eta distibutions
      if (strstr(shortName[pageNumber],"_v")!=0 ) {
	hist->SetMaximum(10.);
	hist->SetMinimum(-10.);
      }
      gStyle->SetOptStat(100110);
      hist->Draw();
      lineZeroEta->Draw();
    } else if (strstr(shortName[pageNumber],"Pt")!=0) {     // Pt distibutions
      gStyle->SetOptStat(100110);
	hist->Draw();
	if (strstr(shortName[pageNumber],"v")!=0) {
	  ptMax = hist->GetXaxis()->GetXmax();
	  TLine* lineZeroPt  = new TLine(0., 0., ptMax, 0.);
	  lineZeroPt->Draw();
	}
    } else {                                                // all others
      gStyle->SetOptStat(100110);
      hist->Draw(); 
    }
    gPad->Update();
  }
  delete [] temp;
  delete histName;
  if (histProjName) delete histProjName;
  for (int m = 0; m < nNames; m++) {  
    delete [] shortName[m];
  }
  delete [] shortName;

  return c;
}

// macro for the single plots
TCanvas* plotCenSingles(char* shortName){
  cout << "  graph name= " << shortName << endl;
  TString* histProjName = NULL;
  char* temp = new char[30];                        // construct histName
  strcpy(temp,shortName);
  char* cproj = strstr(temp,".");
  if (cproj) {                                      // a projection
    *cproj = '\0';                                  // remove from "." on
    cproj = strstr(temp,"2");
    if (cproj) {                                    // a 2D projection 
      *cproj = '\0';                                // remove from "2D" on
      strcat(temp,"3D");
    } else {
      strcat(temp,"2D");
    }
    TString* histName = new TString(temp);
    histProjName = new TString(shortName);
  } else {
    TString* histName = new TString(shortName);
  }

  // make the graph page
  TCanvas* c = new TCanvas(shortName,shortName,canvasWidth,canvasHeight);
  c->ToggleEventStatus();
  TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,histName->Data());
  title->Draw();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();

  // make the plots
  graphPad->Divide(columns,rows);
  for (int i = 0; i < pads; i++) {
    int fileN = i;                           // file number
    int padN = fileN + 1;                    // pad number
    cout << "centrality= " << padN << endl;
    if (histProjName) {
      if (strstr(temp,"3")) {
	TH3* hist = (TH3*)histFile[fileN].Get(histName->Data());
      } else {
	TH2* hist = (TH2*)histFile[fileN].Get(histName->Data());
      }
    } else {
      TH1* hist = (TH1*)histFile[fileN].Get(histName->Data());
    }
    if (!hist) {
      cout << "### Can't find histogram " << histName->Data() << endl;
      return;
    }
    sprintf(histTitle,"Centrality %d",padN);
    hist->SetTitle(histTitle);
    graphPad->cd(padN);
    if (strstr(shortName,"3D")!=0) {                  // 3D
      gStyle->SetOptStat(10);
      hist->Draw("");
    } else if (strstr(shortName,".PhiEta")!=0) {      // 3D Phi Eta projection
      TH2D* projZX = hist->Project3D("zx");
      projZX->SetName(histProjName->Data());
      projZX->SetYTitle("azimuthal angle (rad)");
      projZX->SetXTitle("pseudorapidity");
      gStyle->SetOptStat(10);
      if (projZX) projZX->Draw("COLZ");
    } else if (strstr(shortName,".PhiPt")!=0) {       // 3D Phi Pt projection
      TH2D* projZY = hist->Project3D("zy");
      projZY->SetName(histProjName->Data());
      projZY->SetYTitle("azimuthal angle (rad");
      projZY->SetXTitle("Pt (GeV)");
      gStyle->SetOptStat(10);
      if (projZY) projZY->Draw("COLZ");
    } else if (strstr(shortName,"XY")!=0) {           // Vertex XY
      TLine* lineZeroX = new TLine(-1., 0., 1., 0.);
      TLine* lineZeroY = new TLine(0., -1., 0., 1.);
      gStyle->SetOptStat(10);
      hist->Draw("COLZ");
      lineZeroX->Draw();
      lineZeroY->Draw();
    } else if (strstr(shortName,"2D")!=0) {           // 2D
      gStyle->SetOptStat(10);
      hist->Draw("COLZ");
    } else if (strstr(shortName,".Eta")!=0) {         // 2D Eta projection
      TH1D* projX = hist->ProjectionX(histName->Data(), 0, 9999);
      projX->SetName(histProjName->Data());
      char* xTitle = hist->GetXaxis()->GetTitle();
      projX->SetXTitle(xTitle);
      projX->SetYTitle("Counts");
      gStyle->SetOptStat(10);
      if (projX) projX->Draw("H");
    } else if (strstr(shortName,".Pt")!=0) {          // 2D Pt projection
      TH1D* projY = hist->ProjectionY(histName->Data(), 0, 9999); 
      projY->SetName(histProjName->Data());
      projY->SetXTitle("Pt (GeV)");
      projY->SetYTitle("Counts");
      gPad->SetLogy();
      gStyle->SetOptStat(100110);
      if (projY) projY->Draw("H");
    } else if (strstr(shortName,"Bin")!=0) {          // Bin hists
      if (strstr(shortName,"Pt")!=0) {
	TLine* lineDiagonal = new TLine(0., 0., ptMax, ptMax);
      } else {
	TLine* lineDiagonal = new TLine(-etaMax, -etaMax, etaMax, etaMax);
      }
      gStyle->SetOptStat(0);
      hist->SetMarkerStyle(21);
      hist->SetMarkerColor(2);
      hist->Draw();
      lineDiagonal->Draw();
    } else if (strstr(shortName,"CosPhi")!=0) {       // CosPhiLab
      TLine* lineZeroHar = new TLine(0.5, 0., 6.5, 0.);
      gStyle->SetOptStat(0);
      hist->Draw();
      lineZeroHar->Draw();
    } else if (strstr(shortName,"PidMult")!=0) {      // PID Mult
      gPad->SetLogy();
      gStyle->SetOptStat(0);
      hist->Draw();
    } else if (strstr(shortName,"Pid")!=0) {          // PID
      gStyle->SetOptStat(110110);
      hist->Draw();
    } else {
      gStyle->SetOptStat(100110);
      hist->Draw();
    }
    gPad->Update();
  }
  delete [] temp;
  delete histName;
  if (histProjName) delete histProjName;
  
  return c;
}

void plotCenAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  char temp[3];
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = plotCen(i, selN, harN);
    c->Update();
    cout << "save? y/[n]" << endl;
    fgets(temp, sizeof(temp), stdin);
    if (strstr(temp,"y")!=0) c->Print(".ps");
    c->Delete();
  }
  cout << "  Done" << endl;
}
