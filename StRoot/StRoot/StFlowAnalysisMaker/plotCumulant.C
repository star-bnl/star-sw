///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotCumulant.C,v 1.11 2007/02/06 19:00:52 posk Exp $
//
// Author:       Art Poskanzer, LBNL, Nov 2001
// Description:  Macro to plot histograms made by StFlowCumulantMaker.
//               If orderN = 0 plot all selections and harmonics.
//               First time type .x plotCumulant.C() to see the menu.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               Hist file is anaXX.root where XX is the number.
//               Default hist file is flow.cumulant.root .
//               After the first execution, just type plotCumulant(N) .
//               A negative N plots all pages starting with page N.
//               Place a symbolic link to this file in StRoot/macros/analysis .
//
///////////////////////////////////////////////////////////////////////////////

#include <math.h> 
const  Int_t nHars    = 2; // 2
const  Int_t nOrders  = 2;
Int_t  runNumber      = 0;
char   runName[6];
char   fileNum[4]  = "x";
char   fileName[30];
TFile* histFile;
char   tmp[10];
TCanvas* c;

TCanvas* plotCumulant(Int_t pageNumber=0, Int_t selN=2, Int_t orderN=0, Int_t harN=0){

  bool multiGraph  = kFALSE;                            // set flags
  bool singleGraph = kFALSE;
  if (orderN == 0) multiGraph = kTRUE;
  bool mixGraph = kFALSE;
  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowCumulantMaker
  const char* baseName[] = {
    "Flow_Cumul_v_Order2",
    "Flow_Cumul_v_Order4",
    "Flow_CumulMix_v",
    "Flow_CumulMix_vEta",
    "Flow_CumulMix_vPt",
    "Flow_Cumul_vEta_Order",
    "Flow_Cumul_vPt_Order"
    //"Flow_Cumul_v2D_Order"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);
  const int nSingles =  5;
  float Ycm = 0.0;

  // construct array of short names
  char* shortName[] = new char*[nNames];
  for (int n = 0; n < nNames; n++) {
    shortName[n] = new char[30];
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"Order");
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
  if (strstr(fileNum, "x")!=0) {
    cout << "     anaXX.root file number? [0= flow.hist.root] ";
    fgets(fileNum, sizeof(fileNum), stdin);
    fileNum[strlen(fileNum)-1] = '\0';
    if (strlen(fileNum) == 1 && strstr(fileNum,"0")) {
      sprintf(fileName, "flow.hist.root");
    } else {
      sprintf(fileName, "ana%s.root", fileNum);       // insert
    }
    cout << " file name = " << fileName << endl;
    histFile = new TFile(fileName);
  }
  
  // input the page number
  while (pageNumber <= 0 || pageNumber > nNames) {
    if (pageNumber < 0) {                                 // plot all
      plotCumulantAll(nNames, orderN, selN, harN, -pageNumber);
      return c;
    }
    cout << "-1: \t All" << endl;                         // print menu
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << baseName[i] << endl;
    }
    cout << "     page number? ";
    fgets(tmp, sizeof(tmp), stdin);
    pageNumber = atoi(tmp);
  }
  if (pageNumber > 0 && pageNumber <= nSingles) {         // plot singles
    singleGraph = kTRUE;
    multiGraph  = kFALSE;
    strcpy(shortName[pageNumber-1], baseName[pageNumber-1]); 
  }
  pageNumber--;
  cout << "  graph name= " << shortName[pageNumber] << endl;

  if (strstr(shortName[pageNumber],"Mix")!=0) {            // Mixed harmonic v1
    mixGraph = kTRUE;
    harN = 1;
  }

  // set row and column numbers
  int columns = nOrders;
  int rows = nHars;
  int pads = rows*columns;

  // make the graph page
  if (multiGraph) {
    int canvasWidth = 600, canvasHeight = 780;             // portrait
  } else {
    int canvasWidth = 780, canvasHeight = 600;             // landscape
  }
  c = new TCanvas(shortName[pageNumber], shortName[pageNumber],
			   canvasWidth, canvasHeight);
  c->ToggleEventStatus();
  if (multiGraph) {
    TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,shortName[pageNumber]);
    title->Draw();
  }
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now.AsString());
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
    int firstK = orderN/2 -1, firstJ = harN -1, lastK = orderN/2, lastJ = harN;
  }
  for (int j = firstJ; j < lastJ; j++) {
    for (int k = firstK ; k < lastK; k++) {
      char countOrder[2];
      sprintf(countOrder,"%d",2*(k+1));
      int padN = j*columns + k + 1;                          // pad number

      // construct histName
      TString* histName = new TString(baseName[pageNumber]);
      if (multiGraph) {
	histName->Append(*countOrder);
      }
      histName->Append("_Sel");
      *histName += selN;
      if (multiGraph) {
	histName->Append("_Har");
	*histName += j+1;
      } else if (mixGraph && strcmp(shortName[pageNumber],"Flow_CumulMix_v") != 0) {
	histName->Append("_Har");
	*histName += j+1;
      }
      cout << " col= " << k+1 << " row= " << j+1 << " pad= " << padN << "\t" 
	   << histName->Data() << endl;

      // get the histogram
      bool twoD;
      if (strstr(shortName[pageNumber],"2D")!=0) {            // 2D
	twoD = kTRUE;
	TH2* hist2D = dynamic_cast<TH2*>(histFile->Get(histName->Data()));
	if (!hist2D) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
      } else {                                                // 1D
	TH1* hist = dynamic_cast<TH1*>(histFile->Get(histName->Data()));
	if (!hist) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return c;
	}
	float xMax = hist->GetXaxis()->GetXmax();
      }
      
      // make the plots
      if (multiGraph) graphPad->cd(padN);
      if (twoD) {                                             // 2D
	gStyle->SetOptStat(10);
	//hist2D->Draw("COLZ");
	hist2D->Draw("LEGO1");
      } else {                                                // all other 1D
	gStyle->SetOptStat(100110);
	hist->Draw(); 
	float yMax, yMin;
	if (strstr(shortName[pageNumber],"Eta")!=0) {
	  if (strstr(shortName[pageNumber],"_v")!=0) {
	    yMax = 10.;
	    yMin = -10.;
	  } else {
	    yMax = 0.005;
	    yMin = -0.005;
	  }
	  hist->SetMaximum(yMax);
	  hist->SetMinimum(yMin);
	  TLine* lineZeroEta = new TLine(-xMax, 0., xMax, 0.);
	  lineZeroEta->Draw();
	  TLine* lineEtaCM   = new TLine(Ycm, yMin, Ycm, yMax);
	  lineEtaCM->Draw();
	} else if (strstr(shortName[pageNumber],"Pt")!=0) {
	  if (strstr(shortName[pageNumber],"_v")!=0) {
	    yMax = 30.;
	    yMin = -10.;
	  } else {
	    yMax = 0.02;
	    yMin = -0.02;
	  }
	  hist->SetMaximum(yMax);
	  hist->SetMinimum(yMin);
	  TLine* lineZeroPt = new TLine(0., 0., xMax, 0.);
	  lineZeroPt->Draw();
	} else {
	  hist->SetMinimum(0.);
	  TLine* lineZeroHar = new TLine(0.5, 0., nHars+0.5, 0.);
	  lineZeroHar->Draw();
	}
      }
      delete histName;
    }
  }
  for (int m = 0; m < nNames; m++) {  
    delete [] shortName[m];
  }
  delete [] shortName;

  return c;
}

void plotCumulantAll(Int_t nNames, Int_t orderN, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    c = plotCumulant(i, selN, orderN, harN);
    c->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) c->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
  }
  cout << "  plotCumulantAll Done" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plotCumulant.C,v $
// Revision 1.11  2007/02/06 19:00:52  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.10  2006/02/24 18:13:39  posk
// Reduced number of histograms.
//
// Revision 1.9  2004/12/09 23:47:12  posk
// Minor changes in code formatting.
// Added hist for TPC primary dca to AnalysisMaker.
//
// Revision 1.8  2004/03/01 22:43:45  posk
// Changed some "->" to ".".
//
// Revision 1.7  2003/08/26 21:10:14  posk
// Calculates v8 if nHars=8.
//
// Revision 1.6  2003/06/27 21:25:46  posk
// v4 and v6 are with repect to the 2nd harmonic event plane.
//
// Revision 1.5  2003/02/25 19:25:34  posk
// Improved plotting.
//
// Revision 1.4  2002/01/14 23:43:06  posk
// Renamed ScalerProd histograms. Moved print commands to FlowMaker::Finish().
//
// Revision 1.3  2001/12/18 19:27:44  posk
// "proton" and "antiproton" replaced by "pr+" and "pr-".
//
// Revision 1.2  2001/12/11 22:04:22  posk
// Four sets of phiWgt histograms.
// StFlowMaker StFlowEvent::PhiWeight() changes.
// Cumulant histogram names changed.
//
// Revision 1.1  2001/11/09 21:15:14  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
//
///////////////////////////////////////////////////////////////////////////////
