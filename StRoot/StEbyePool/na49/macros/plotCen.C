///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotCen.C,v 1.7 2002/03/23 21:46:08 posk Exp $
//
// Author:       Art Poskanzer, LBNL, July 2000
// Description:  Macro to plot histograms made by StFlowAnalysisMaker.
//               Plots a set of histograms with different centrality
//               starting with anaXX.root given by first run number XX.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               Default selN = 2 and harN = 2.
//               First time type .x plotCen.C() to see the menu.
//               After the first execution, just type plotCen(N) .
//               A negative N plots all pages starting with page N.
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
const  Int_t nCens = 6;
int    runNumber   = 0;
char   runName[6];
char   fileName[30];
char   histTitle[30];
TFile* histFile[nCens];
int    canvasWidth = 600, canvasHeight = 780;             // portrait
int    columns = 2;
int    rows    = nCens/columns;
int    pads    = rows*columns;
char   tmp[10];

TCanvas* plotCen(Int_t pageNumber=0, Int_t selN=2, Int_t harN=2){

  int  eBeam = 158; //for 158 GeV data
  //int  eBeam = 40;  //for 40 GeV data
  bool multiGraph;
  bool doubleGraph;
  bool singleGraph;

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char* baseName[] = {
    "Flow_Res_Sel",
    "Flow_Cos_Sel",
    "Flow_v_Sel",
    "Flow_VertexZ",
    "Flow_VertexXY2D",
    "Flow_EtaSym",
    "Flow_EVeto",
    "Flow_Cent",
    "Flow_OrigMult",
    "Flow_Mult",
    "Flow_MultOverOrig",
    "Flow_MultPart",
    "Flow_Charge",
    "Flow_Bx",
    "Flow_By",
    "Flow_Chi2",
    "Flow_FitPts",
    "Flow_MaxPts",
    "Flow_FitOverMax",
    "Flow_MeanDedx2D_Pos",
    "Flow_MeanDedx2D_Neg",
    "Flow_MeanDedxPiMinus2D",
    "Flow_MeanDedxPiPlus2D",
    "Flow_MeanDedxProton2D",
    "Flow_PidMult",
    //"Flow_YPtPhi3D",
    //"Flow_YPtPhi2D.PhiY",
    //"Flow_YPtPhi2D.PhiPt",
    "Flow_YieldAll2D",
    "Flow_YieldAll.Y",
    "Flow_YieldAll.Pt",
    "Flow_YieldPart2D",
    "Flow_YieldPart.Y",
    "Flow_YieldPart.Pt",
    //"Flow_Bin_Y",
    //"Flow_Bin_Pt",
    "Flow_CosPhiLab", 
    //"Flow_Phi_Sel",                // first multi graph hist
    //"Flow_Phi_Weight_Sel",
    //"Flow_Phi_Flat_Sel",
    "Flow_Mean_Cos2D_Sel",
    "Flow_Mean_Sin2D_Sel",
    "Flow_Mean_Cos_Flat2D_Sel",
    "Flow_Mean_Sin_Flat2D_Sel",
    "Flow_Mul_Sel",
    "Flow_SumPt2_Sel",
    "Flow_Yield2D_Sel",
    "Flow_Yield.Y_Sel",
    "Flow_Yield.Pt_Sel",
    "Flow_Psi_Subs",
    "Flow_Psi_Sel",
    "Flow_Psi_Sub_Corr_Sel",
    "Flow_Psi_Sub_Corr_Diff_Sel",
    "Flow_Phi_Corr_Sel",
    "Flow_vObs2D_Sel",
    "Flow_vObsY_Sel",
    "Flow_vObsPt_Sel",
    "Flow_v2D_Sel",
    "Flow_vY_Sel",
    "Flow_vPt_Sel",
    //"Flow_q_Sel"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);
  const int nDoubles = 3;
  const int nSingles = 29 + nDoubles;

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
    fgets(tmp, sizeof(tmp), stdin);
    runNumber = atoi(tmp);
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
      plotCenAll(nNames, selN, harN, -pageNumber);
      return;
    }
    cout << "-1: \t All" << endl;                        // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << shortName[i] << endl;
    }
    cout << "     page number? ";
    fgets(tmp, sizeof(tmp), stdin);
    pageNumber = atoi(tmp);
  }

  // set flags
  multiGraph  = kFALSE;
  doubleGraph = kFALSE;
  singleGraph = kFALSE;
  if (pageNumber > 0 && pageNumber <= nDoubles) {
    doubleGraph = kTRUE;
  } else if (pageNumber > nDoubles && pageNumber <= nSingles) {
    singleGraph = kTRUE;
  } else {
    multiGraph  = kTRUE;
  }
  pageNumber--;

  // set constants
  float twopi   = 2. * 3.1416;
  float etaMax  =   1.5;
  float yMin    =    1.;
  float yMax    =    7.;
  float qMax    =   3.5;
  float phiMax  = twopi; 
  int   n_qBins =    50;
  float Ycm     =  2.92;
  TString* histProjName = NULL;

  if(eBeam == 40) {
    Ycm = 2.24;
  }
  // construct histName and histProjName
  char sel[2];
  sprintf(sel,"%d",selN);
  char har[2];
  sprintf(har,"%d",harN);
  char* temp = new char[30];                       // construct histName
  strcpy(temp,shortName[pageNumber]);
  char* cproj = strstr(temp,".");
  if (cproj) {                                     // a projection
    *cproj = '\0';                                 // remove from "." on
    if (singleGraph) {
      cproj = strstr(temp,"2");
      if (cproj) {                                 // a 2D projection 
	*cproj = '\0';                             // remove from "2D" on
	strcat(temp,"3D");
      } else {
	strcat(temp,"2D");
      }
    } else {
      strcat(temp,"2D_Sel");
    }
    TString* histName = new TString(temp);
    histProjName = new TString(baseName[pageNumber]);
    if (multiGraph) {
      histProjName->Append(*sel);
      histProjName->Append("_Har");
      histProjName->Append(*har);
    }
  } else {                                         // not projection
    TString* histName = new TString(baseName[pageNumber]);
  }
  if (!singleGraph) histName->Append(*sel);
  if (multiGraph) {
    histName->Append("_Har");
    histName->Append(*har);
  }
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
  graphPad->Divide(columns,rows);
  TLine* lineZeroY = new TLine(yMin, 0., yMax, 0.);
  TLine* lineYcm   = new TLine(Ycm, -10., Ycm, 10.);
  for (int i = 0; i < pads; i++) {
    int fileN = i;                           // file number
    int padN = fileN + 1;                    // pad number
    sprintf(histTitle,"Centrality %d",padN);
    cout << "centrality= " << padN << endl;

    // get the histogram
    bool twoD;
    bool threeD;
    if (histProjName) {
      if (strstr(temp,"3D")) {                      // 2D projection
	TH3* hist3D = dynamic_cast<TH3*>(histFile[fileN]->Get(histName->Data()));
	if (!hist3D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	hist3D->SetTitle(histTitle);
	twoD = kTRUE;
      } else {                                      // 1D projection
	TH2* hist2D = dynamic_cast<TH2*>(histFile[fileN]->Get(histName->Data()));
	if (!hist2D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	hist2D->SetTitle(histTitle);
      }
    } else {
      if (strstr(shortName[pageNumber],"3D")!=0) {  // 3D
	threeD = kTRUE;
	TH3* hist3D = dynamic_cast<TH3*>(histFile[fileN]->Get(histName->Data()));
	if (!hist3D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	hist3D->SetTitle(histTitle);
      } else if (strstr(shortName[pageNumber],"2D")!=0) { // 2D
	twoD = kTRUE;
	TH2* hist2D = dynamic_cast<TH2*>(histFile[fileN]->Get(histName->Data()));
	if (!hist2D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	hist2D->SetTitle(histTitle);
      } else {                                            // 1D
	TH1* hist = dynamic_cast<TH1*>(histFile[fileN]->Get(histName->Data()));
	if (!hist) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	float ptMax = hist->GetXaxis()->GetXmax();
	hist->SetTitle(histTitle);
      }
    }
    
    // make the plots
    graphPad->cd(padN);
    
    if (threeD) {                                            // 3D
      gStyle->SetOptStat(10);
      hist3D->Draw("BOX");
    } else if (twoD) {                                       // 2D
      if (strstr(shortName[pageNumber],".PhiY")!=0) {        // 3D Phi Y proj.
	TH2D* projZX = (TH2*)hist3D->Project3D("zx");
	projZX->SetName(histProjName->Data());
	projZX->SetYTitle("azimuthal angle (rad)");
	projZX->SetXTitle("rapidity");
	gStyle->SetOptStat(0);
	if (projZX) projZX->Draw("COLZ");
      } else if (strstr(shortName[pageNumber],".PhiPt")!=0) { // 3D Phi Pt proj.
	TH2D* projZY = (TH2*)hist3D->Project3D("zy");
	projZY->SetName(histProjName->Data());
	projZY->SetYTitle("azimuthal angle (rad");
	projZY->SetXTitle("Pt (GeV)");
	gStyle->SetOptStat(0);
	if (projZY) projZY->Draw("COLZ");
      } else	if (strstr(shortName[pageNumber],"XY")!=0) {    // Vertex XY
	TLine* lineZeroX = new TLine(-1., 0., 1., 0.);
	TLine* lineZero  = new TLine(0., -1., 0., 1.);
	gStyle->SetOptStat(10);
	hist2D->Draw("COLZ");
	lineZeroX->Draw();
	lineZero->Draw();
      } else if (strstr(shortName[pageNumber],"Dedx")!=0) {   // dE/dx
	TF1* pionLine = new TF1("pionLine", BetheBlochFunc, -2., 5., 1);
	TF1* protLine = new TF1("protLine", BetheBlochFunc, -2., 5., 1);
	TF1* elecLine = new TF1("elecLine", BetheBlochFunc, -2., 5., 1);
	double pionMass = 0.1396;
	double protMass = 0.9383;
	double elecMass = 0.00051;
	pionLine->SetParameters(&pionMass);
	protLine->SetParameters(&protMass);
	elecLine->SetParameters(&elecMass);
	gStyle->SetOptStat(10);
	hist2D->Draw("COLZ");
	pionLine->Draw("SAME");
	protLine->Draw("SAME");
	elecLine->Draw("SAME");
	gPad->Update();
      } else if (strstr(shortName[pageNumber],"_v")!=0) {    // v
	hist2D->SetMaximum(20.);
	hist2D->SetMinimum(-20.);
	gStyle->SetOptStat(0);
	hist2D->Draw("COLZ");
      } else {                                               // other 2D
	gStyle->SetOptStat(10);
	hist2D->Draw("COLZ");
      }
    } else if (strstr(shortName[pageNumber],".Y")!=0) { // 2D Y projection
      if (singleGraph) {
	TH1D* projX = hist2D->ProjectionX(histName->Data(), 0, 9999);
      } else {
	TH1D* projX = hist2D->ProjectionX(histName->Data(), 0, 9999, "E");
      }
      projX->SetName(histProjName->Data());
      char* xTitle = hist2D->GetXaxis()->GetTitle();
      projX->SetXTitle(xTitle);
      projX->SetYTitle("Counts");
      gStyle->SetOptStat(0);
      if (projX) projX->Draw("H");
      if (!singleGraph) lineZeroY->Draw();
    } else if (strstr(shortName[pageNumber],".Pt")!=0) { // 2D Pt projection
      if (singleGraph) {
	TH1D* projY = hist2D->ProjectionY(histName->Data(), 0, 9999); 
      } else {
	TH1D* projY = hist2D->ProjectionY(histName->Data(), 0, 9999, "E");
	}
      projY->SetName(histProjName->Data());
      projY->SetXTitle("Pt (GeV)");
	projY->SetYTitle("Counts");
	gPad->SetLogy();
	gStyle->SetOptStat(0);
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
    } else if (strstr(shortName[pageNumber],"_q")!=0) {   // q distibution
      double area = hist->Integral() * qMax / (float)n_qBins; 
      cout << "  Area = " << area << endl;
      TF1* func_q = new TF1("func_q", "[0]*2.*x*exp(-x*x)", 0., qMax);
      func_q->SetParameter(0, area);
      gStyle->SetOptStat(100110);
      gStyle->SetOptFit(111);
      hist->Draw("E1");
      func_q->SetLineStyle(kDotted);
      func_q->Draw("same");
    } else if (strstr(shortName[pageNumber],"Phi")!=0) {  // Phi distibutions
      hist->SetMinimum(0.9*(hist->GetMinimum()));
      if (strstr(shortName[pageNumber],"Weight")!=0) {
	TLine* lineOnePhi  = new TLine(0., 1., phiMax, 1.);
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
    } else if (strstr(shortName[pageNumber],"Y")!=0) {      // Y distibutions
      if (strstr(shortName[pageNumber],"_v")!=0 ) {
	hist->SetMaximum(10.);
	hist->SetMinimum(-10.);
      }
      gStyle->SetOptStat(100110);
      hist->Draw();
      lineZeroY->Draw();
      lineYcm->Draw();
    } else if (strstr(shortName[pageNumber],"Pt")!=0) {     // Pt distibutions
      if (strstr(shortName[pageNumber],"_v")!=0 ) {
	hist->SetMaximum(15.);
	hist->SetMinimum(-5.);
      }
      gStyle->SetOptStat(100110);
      hist->Draw();
      if (strstr(shortName[pageNumber],"v")!=0) {
	TLine* lineZeroPt  = new TLine(0., 0., ptMax, 0.);
	lineZeroPt->Draw();
      }
    } else if (strstr(shortName[pageNumber],"Bin")!=0) {    // Bin hists
      if (strstr(shortName[pageNumber],"Pt")!=0) {
	TLine* lineDiagonal = new TLine(0., 0., ptMax, ptMax);
      } else {
	TLine* lineDiagonal = new TLine(-etaMax, -etaMax, etaMax, etaMax);
      }
      gStyle->SetOptStat(0);
      hist->SetMarkerStyle(kFullSquare);
      hist->SetMarkerColor(kRed);
      hist->Draw();
      lineDiagonal->Draw();
    } else if (strstr(shortName[pageNumber],"CosPhi")!=0) {  // CosPhiLab
      TLine* lineZeroHar = new TLine(0.5, 0., 6.5, 0.);
      gStyle->SetOptStat(0);
      hist->Draw();
      lineZeroHar->Draw();
    } else if (strstr(shortName[pageNumber],"PidMult")!=0) {  // PID Mult
      gPad->SetLogy();
      gStyle->SetOptStat(0);
      hist->Draw();
    } else if (strstr(shortName[pageNumber],"_v")!=0 ) {      // v 1D
      hist->SetMaximum(5.);
      gStyle->SetOptStat(0);
      hist->Draw();
    } else {                                              // all other 1D
      gStyle->SetOptStat(100110);
      hist->Draw(); 
    }
    //gPad->Update();
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

void plotCenAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = plotCen(i, selN, harN);
    c->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) c->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
    c->Delete();
  }
  cout << "  Done" << endl;
}

static Double_t BetheBlochFunc(double* lnp, double* par) {
  double mass = par[0];
  double d;
  double x  = exp(*lnp)/mass;
  double x0 = sqrt(2.*log(10.)/(3.*0.2));
  double x1 = pow(10., 2.38 - 1./3.*x0);
  double x2 = pow(10., 2.38 + 2./3.*x0);
  double p0 = 1.59755/(9.8 + 2.*log(10.)*2.38 - 1.);
  if (x < x1) {
    d = 0.;
  } else {
    d = -2.*log(x) + 2.*log(10.)*2.38;
    if (x < x2) {
      double d1 = 2./3.*x0;
      double d2 = log(x)/log(10.);
      double d4 = 2.38 + d1 - d2;
      double d3 = d4*d4*d4;
      d -= 0.2*d3;
    }
  }
  double p1 = (1. + x*x)/(x*x);
  double p2 = log(1. + x*x);
  Double_t dedx = p0*(p1*(9.8 + p2 + d) - 1.);
  
  return dedx;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plotCen.C,v $
// Revision 1.7  2002/03/23 21:46:08  posk
// More 40 GeV compatability.
//
// Revision 1.6  2001/11/06 18:02:49  posk
// 40 GeV compatability.
//
// Revision 1.5  2001/10/24 21:48:41  posk
// Improved graphs.
//
// Revision 1.4  2001/08/17 22:15:03  posk
// Updated to also do 40 GeV.
//
// Revision 1.3  2001/05/14 23:22:47  posk
// Minor changes.
//
// Revision 1.2  2001/03/06 17:33:00  posk
// All macros now work.
//
// Revision 1.1  2001/02/23 00:58:27  posk
// NA49 version of STAR software.
//
// Revision 1.5  2000/09/29 22:53:19  posk
// More histograms.
//
// Revision 1.1  2000/08/31 18:50:33  posk
// Added plotCen.C to plot from a series of files with different centralities.
//
///////////////////////////////////////////////////////////////////////////////
