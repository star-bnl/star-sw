///////////////////////////////////////////////////////////////////////////////
//
// $Id: plot.C,v 1.9 2002/03/23 21:46:04 posk Exp $
//
// Author:       Art Poskanzer, LBNL, Aug 1999
// Description:  Macro to plot histograms made by StFlowAnalysisMaker.
//               If selN = 0 plot all selections and harmonics.
//               First time type .x plot.C() to see the menu.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               Hist file is anaXX.root where XX is the number.
//               Default hist file is flow.hist.root .
//               After the first execution, just type plot(N) .
//               A negative N plots all pages starting with page N.
//               After plot(-N) type two returns.
//
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h> 
//const Int_t nHars    = 6;
const  Int_t nHars    = 3;
const  Int_t nSels    = 2;
const  Int_t nSubs    = 2;
Int_t  runNumber      = 0;
char   runName[6];
char   fileNumber[4]  = "x";
char   fileName[30];
TFile* histFile;
char   tmp[10];

TCanvas* plot(Int_t pageNumber=0, Int_t selN=0, Int_t harN=0){

  int  eBeam = 158; //for 158GeV data
  //int  eBeam = 40;  //for 40GeV data
  bool multiGraph  = kFALSE;                            // set flags
  bool singleGraph = kFALSE;
  if (selN == 0) multiGraph = kTRUE;

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  //gROOT->SetStyle("Video");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char* baseName[] = {
    "Flow_Res_Sel",
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
    "Flow_YPtPhi2D.PhiY",
    "Flow_YPtPhi2D.PhiPt",
    "Flow_YieldAll2D",
    "Flow_YieldAll.Y",
    "Flow_YieldAll.Pt",
    "Flow_YieldPart2D",
    "Flow_YieldPart.Y",
    "Flow_YieldPart.Pt",
    //"Flow_Bin_Y",
    //"Flow_Bin_Pt",
    "Flow_CosPhiLab", 
    "Flow_Phi_Sel",                // first multi graph hist
    "Flow_Phi_Weight_Sel",
    "Flow_Phi_Flat_Sel",
    "Flow_Mean_Cos2D_Sel",
    "Flow_Mean_Sin2D_Sel",
    "Flow_Mean_Cos_Flat2D_Sel",
    "Flow_Mean_Sin_Flat2D_Sel",
    "Flow_Mul_Sel",
    "Flow_SumPt2_Sel",
    "Flow_PtY_Sel",
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
    "Flow_q_Sel"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);
  const int nSingles = 31 + 1;

  // construct array of short names
  char* shortName[] = new char*[nNames];
  for (int n = 0; n < nNames; n++) {
    shortName[n] = new char[30];
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"_Sel");
    if (cp) *cp = '\0';                                  // truncate
  }

  // input the run number
  if (runNumber == 0) {
    cout << "     run number? ";    
    fgets(tmp, sizeof(tmp), stdin);
    runNumber = atoi(tmp);
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " run name = " << runName << endl;
  }

  // input the file number (0 opens flow.hist.root)
  if (strstr(fileNumber, "x")!=0) {
    cout << "     anaXX.root file number? [0= flow.hist.root] ";
    fgets(fileNumber, sizeof(fileNumber), stdin);
    fileNumber[strlen(fileNumber)-1] = '\0';
    if (strlen(fileNumber) == 1 && strstr(fileNumber,"0")) {
      sprintf(fileName, "flow.hist.root");
    } else {
      sprintf(fileName, "ana%s.root", fileNumber);       // insert
    }
    cout << " file name = " << fileName << endl;
    histFile = new TFile(fileName);
  }
  
  // input the page number
  while (pageNumber <= 1 || pageNumber > nNames) {
    if (pageNumber < 0) {                                 // plot all
      plotAll(nNames, selN, harN, -pageNumber);
      return;
    }
    if (pageNumber == 1) {                                // plot resolution
      TCanvas* c = plotResolution();
      return c;
    }
    cout << "-1: \t All" << endl;                         // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << shortName[i] << endl;
    }
    cout << "     page number? ";
    fgets(tmp, sizeof(tmp), stdin);
    pageNumber = atoi(tmp);
  }
  if (pageNumber > 0 && pageNumber <= nSingles) {         // plot singles
    singleGraph = kTRUE;
    multiGraph  = kFALSE;
  }
  pageNumber--;
  cout << "  graph name= " << shortName[pageNumber] << endl;

  // set constants
  float twopi   = 2. * TMath::Pi();
  float phiMax  = twopi; 
  float Ycm     =  2.92;
  TString* histProjName = NULL;

  if(eBeam == 40) Ycm = 2.24;

  // set row and column numbers
  char* cp = strstr(shortName[pageNumber],"Subs");
  int columns = (cp) ? nSubs + nSels : nSels;
  int rows = (strcmp(shortName[pageNumber],"Flow_Psi_Sub_Corr_Diff")!=0) ?
    nHars : nHars -1;
  int pads = rows*columns;

  // make the graph page
  if (multiGraph) {
    int canvasWidth = 600, canvasHeight = 780;             // portrait
  } else {
    int canvasWidth = 780, canvasHeight = 600;             // landscape
  }
  TCanvas* c = new TCanvas(shortName[pageNumber], shortName[pageNumber],
			   canvasWidth, canvasHeight);
  c->ToggleEventStatus();
  if (multiGraph) {
    TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,shortName[pageNumber]);
    title->Draw();
  }
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();

  if (multiGraph) {               // many graphs on one page
    graphPad->Divide(columns,rows);
    int firstK = 0, firstJ = 0, lastK = columns, lastJ = rows;
  } else if (singleGraph) {       // single graph on a page
    int firstK = 0, firstJ = 0, lastK = 1, lastJ = 1;
  } else {                        // one graph from a multi graph page
    int firstK = selN -1, firstJ = harN -1, lastK = selN, lastJ = harN;
  }
  TLine* lineZeroY  = new TLine(1., 0., 7., 0.);
  TLine* lineYcm    = new TLine(Ycm, -10., Ycm, 10.);
  TLine* lineOnePhi = new TLine(0., 1., phiMax, 1.);
  for (int j = firstJ; j < lastJ; j++) {
    char countRows[2];
    sprintf(countRows,"%d",j+1);
    for (int k = firstK ; k < lastK; k++) {
      char countColumns[2];
      sprintf(countColumns,"%d",k+1);
      int padN = j*columns + k + 1;                    // pad number

      // construct histName and histProjName
      char* temp = new char[30];
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
	if (!singleGraph) {
	  histProjName->Append(*countColumns);
	  histProjName->Append("_Har");
	  histProjName->Append(*countRows);
	}
      } else {                                         // not projection
	TString* histName = new TString(baseName[pageNumber]);
      }
      if (!singleGraph) {
	histName->Append(*countColumns);
	histName->Append("_Har");
	histName->Append(*countRows);
      }
      cout << " col= " << k+1 << " row= " << j+1 << " pad= " << padN << "\t" 
	   << histName->Data() << endl;

      // get the histogram
      bool twoD;
      bool threeD;
      if (histProjName) {
	if (strstr(temp,"3D")) {                      // 2D projection
	  TH3* hist3D = dynamic_cast<TH3*>(histFile->Get(histName->Data()));
	  if (!hist3D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	  twoD = kTRUE;
	} else {                                      // 1D projection
	  TH2* hist2D = dynamic_cast<TH2*>(histFile->Get(histName->Data()));
	  if (!hist2D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	}
      } else {
	if (strstr(shortName[pageNumber],"3D")!=0) {  // 3D
	  threeD = kTRUE;
	  TH3* hist3D = dynamic_cast<TH3*>(histFile->Get(histName->Data()));
	  if (!hist3D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	} else if (strstr(shortName[pageNumber],"2D")!=0) { // 2D
	  twoD = kTRUE;
	  TH2* hist2D = dynamic_cast<TH2*>(histFile->Get(histName->Data()));
	  if (!hist2D) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	} else {                                            // 1D
	  TH1* hist = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
	  if (!hist) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	  float max = hist->GetXaxis()->GetXmax();
	}
      }
      
      // make the plots
      if (multiGraph) graphPad->cd(padN);
      if (threeD) {                                            // 3D
	gStyle->SetOptStat(10);
	hist3D->Draw("BOX");
      } else if (twoD) {                                       // 2D
	if (strstr(shortName[pageNumber],".PhiY")!=0) {        // 3D Phi Y proj.
	  TH2D* projZX = hist3D->Project3D("zx");
	  projZX->SetName(histProjName->Data());
	  projZX->SetYTitle("azimuthal angle (rad)");
	  projZX->SetXTitle("rapidity");
	  gStyle->SetOptStat(0);
	  if (projZX) projZX->Draw("COLZ");
	} else if (strstr(shortName[pageNumber],".PhiPt")!=0) { // 3D Phi Pt proj.
	  TH2D* projZY = hist3D->Project3D("zy");
	  projZY->SetName(histProjName->Data());
	  projZY->SetYTitle("azimuthal angle (rad");
	  projZY->SetXTitle("Pt (GeV)");
	  gStyle->SetOptStat(0);
	  if (projZY) projZY->Draw("COLZ");
	} else	if (strstr(shortName[pageNumber],"XY")!=0) {    // Vertex XY
	  TLine* lineZeroX = new TLine(-1., 0., 1., 0.);
	  TLine* lineZeroY = new TLine(0., -1., 0., 1.);
	  gStyle->SetOptStat(10);
	  hist2D->Draw("COLZ");
	  lineZeroX->Draw();
	  lineZeroY->Draw();
	} else if (strstr(shortName[pageNumber],"Dedx")!=0) {   // dE/dx
	  TF1* pionLine = new TF1("pionLine", BetheBlochFunc, -2., 5., 1);
	  TF1* protLine = new TF1("protLine", BetheBlochFunc, -2., 5., 1);
	  TF1* elecLine = new TF1("elecLine", BetheBlochFunc, -2., 5., 1);
	  TF1* kaonLine = new TF1("kaonLine", BetheBlochFunc, -2., 5., 1);
	  double pionMass = 0.1396;
	  double protMass = 0.9383;
	  double elecMass = 0.00051;
	  double kaonMass = 0.4937;
	  pionLine->SetParameters(&pionMass);
	  protLine->SetParameters(&protMass);
	  elecLine->SetParameters(&elecMass);
	  kaonLine->SetParameters(&kaonMass);
	  gStyle->SetOptStat(10);
	  hist2D->Draw("COLZ");
	  pionLine->Draw("SAME");
	  protLine->Draw("SAME");
	  elecLine->Draw("SAME");
	} else if (strstr(shortName[pageNumber],"_v")!=0) {    // v
	  hist2D->SetMaximum(20.);
	  hist2D->SetMinimum(-20.);
	  gStyle->SetOptStat(0);
	  hist2D->Draw("COLZ");
	} else if (strstr(shortName[pageNumber],"Mean_")!=0) { // meanSinCos
	  hist2D->SetMaximum(0.2);
	  hist2D->SetMinimum(-0.2);
	  gStyle->SetOptStat(10);
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
	gStyle->SetOptStat(100110);
	//gStyle->SetOptStat(0);
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
	gStyle->SetOptStat(100110);
	//gStyle->SetOptStat(0);
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
	    funcCos1->SetParameters(0, j+2);                // initial values
	  } else {
	    funcCos1->SetParameters(0, j+1);                // initial values
	  }
	  funcCos1->SetParLimits(1, 1, 1);                  // har is fixed
	  hist->Fit("funcCos1");
	  delete funcCos1;
	} else {
	  TF1* funcCos2 = new TF1("funcCos2",
	 "1+[0]*2/100*cos([2]*x)+[1]*2/100*cos(([2]+1)*x)", 0., twopi);
	  funcCos2->SetParNames("k=1", "k=2", "har");
	  funcCos2->SetParameters(0, 0, j+1);               // initial values
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
	gStyle->SetOptStat(100110);
	gStyle->SetOptFit(111);
	hist->Draw("E1");
	float n_qBins = (float)hist->GetNbinsX();
	double area = hist->Integral() * max / n_qBins; 
	TString* histName = new TString("Flow_Mul_Sel");
	histName->Append(*countColumns);
	histName->Append("_Har");
	histName->Append(*countRows);
	TH1* histMult = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
	if (!histMult) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	delete histName;
	float mult = histMult->GetMean();
	TF1* fit_q = new TF1("qDist", qDist, 0., max, 3); // fit q dist
	fit_q->SetParNames("v", "mult", "area");
	float qMean = hist->GetMean();
	//float v2N = (qMean > 1.) ? qMean - 1. : 0.;
	//float vGuess = 100. * sqrt(v2N / mult);
	float vGuess = (qMean > 2.) ? 8. : 1.;
	fit_q->SetParameters(vGuess, mult, area); // initial values
	fit_q->SetParLimits(1, 1, 1);             // mult is fixed
	fit_q->SetParLimits(2, 1, 1);             // area is fixed
	hist->Fit("qDist", "Q");
	fit_q->Draw("same");
	TF1* func_q = new TF1("func_q", "[0]*2.*x*exp(-x*x)", 0., max);
	func_q->SetParameter(0, area);
	func_q->SetLineStyle(kDotted);
	func_q->Draw("same");
      } else if (strstr(shortName[pageNumber],"CosPhiLab")!=0) {  // CosPhiLab
	TLine* lineZeroHar = new TLine(0.5, 0., nHars + 0.5, 0.);
	gStyle->SetOptStat(0);
	hist->Draw();
	lineZeroHar->Draw();
      } else if (strstr(shortName[pageNumber],"Phi")!=0) {  // Phi distibutions
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
	  TLine* lineZeroPt  = new TLine(0., 0., max, 0.);
	  lineZeroPt->Draw();
	}
      } else if (strstr(shortName[pageNumber],"Bin")!=0) {    // Bin hists
	if (strstr(shortName[pageNumber],"Pt")!=0) {
	  TLine* lineDiagonal = new TLine(0., 0., max, max);
	} else {
	  TLine* lineDiagonal = new TLine(-max, -max, max, max);
	}
	gStyle->SetOptStat(0);
	hist->SetMarkerStyle(kFullSquare);
	hist->SetMarkerColor(kRed);
	hist->Draw();
	lineDiagonal->Draw();
      } else if (strstr(shortName[pageNumber],"PidMult")!=0) {  // PID Mult
	gPad->SetLogy();
	gStyle->SetOptStat(0);
	hist->Draw();
      } else {                                              // all other 1D
	gStyle->SetOptStat(100110);
	hist->Draw(); 
      }
      delete [] temp;
      delete histName;
      if (histProjName) delete histProjName;
    }
  }
  for (int m = 0; m < nNames; m++) {  
    delete [] shortName[m];
  }
  delete [] shortName;

  return c;
}

// macro for the resolution plot
TCanvas* plotResolution(){
  char* resName[] = {"Flow_Cos_Sel","Flow_Res_Sel","Flow_v_Sel"};
  int columns = nSels;
  int rows = 3;
  int pads = rows*columns;

  // make the graph page
  TCanvas* c = new TCanvas(resName[1],resName[1],600,780);
  c->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TLine* lineZeroHar = new TLine(0.5, 0., nHars + 0.5, 0.);
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(columns,rows);

  // make the plots
  for (int j = 0; j < rows; j++) {
    int resNumber = j;
    cout << "resolution name= " << resName[resNumber] << endl;
    for (int k = 0; k < columns; k++) {
      char countColumns[2];
      sprintf(countColumns,"%d",k+1);
      int padN = j*columns + k +1;
      TString* histName = new TString(resName[resNumber]);
      histName->Append(*countColumns);
      cout << "row= " << j << " col= " << k << " pad= " << padN << "\t" 
	   << histName->Data() << endl;
      TH1* hist = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      graphPad->cd(padN);
      gStyle->SetOptStat(0);
      if (strstr(resName[resNumber],"_v")!=0) {
	hist->SetMaximum(5.);
	hist->SetMinimum(-5.);
      } else {
	hist->SetMaximum(0.5);
	hist->SetMinimum(0.);
      }
      hist->Draw();
      lineZeroHar->Draw();
      hist->Print("all");
      delete histName;
    }
  }

  return c;
} 

//-----------------------------------------------------------------------

void plotAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = plot(i, selN, harN);
    c->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) c->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
    c->Delete();
  }
  cout << "  plotAll Done" << endl;
}

//-----------------------------------------------------------------------

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

//-----------------------------------------------------------------------

static Double_t qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area

  double expo = par[1]*par[0]*par[0]/10000. + q[0]*q[0];
  Double_t dNdq = par[2] * (2. * q[0] * exp(-expo) * 
    TMath::BesselI0(2.*q[0]*par[0]/100.*sqrt(par[1])));

  return dNdq;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plot.C,v $
// Revision 1.9  2002/03/23 21:46:04  posk
// More 40 GeV compatability.
//
// Revision 1.8  2002/01/16 18:21:36  posk
// Fit q in plot.C. Updated momentum conservation corr. in vProj.C.
//
// Revision 1.7  2001/11/06 18:02:46  posk
// 40 GeV compatability.
//
// Revision 1.6  2001/10/24 21:48:36  posk
// Improved graphs.
//
// Revision 1.5  2001/08/17 22:14:57  posk
// Updated to also do 40 GeV.
//
// Revision 1.4  2001/05/14 23:22:42  posk
// Minor changes.
//
// Revision 1.3  2001/03/06 17:32:59  posk
// All macros now work.
//
// Revision 1.2  2001/02/26 23:07:14  posk
// Rearranged macros.
//
// Revision 1.1  2001/02/23 00:58:19  posk
// NA49 version of STAR software.
//
// Revision 1.29  2000/10/12 21:01:32  posk
//
///////////////////////////////////////////////////////////////////////////////
