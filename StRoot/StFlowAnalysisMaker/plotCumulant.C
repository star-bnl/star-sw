///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotCumulant.C,v 1.1 2001/11/09 21:15:14 posk Exp $
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
//const Int_t nHars    = 6;
const  Int_t nHars    = 2;
const  Int_t nSels    = 2;
const  Int_t nOrders  = 2;
Int_t  runNumber      = 0;
char   runName[6];
char   fileNumber[4]  = "x";
char   fileName[30];
TFile* histFile;
char   tmp[10];

TCanvas* plotCumulant(Int_t pageNumber=0, Int_t orderN=0, Int_t selN=2, Int_t harN=0){

  bool multiGraph  = kFALSE;                            // set flags
  bool singleGraph = kFALSE;
  if (orderN == 0) multiGraph = kTRUE;

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowCumulantMaker
  const char* baseName[] = {
    "Flow_v_cumulantOrder",
    "Flow_Cumulant_Order",
    "Flow_CumulantEta_Order",
    "Flow_CumulantPt_Order",
    "Flow_vEta_cumulantOrder",
    "Flow_vPt_cumulantOrder",
    "Flow_Cumulant2D_Order",
    "Flow_v2D_cumulantOrder"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);
  const int nSingles =  2;

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

  // input the file number (0 opens flow.cumulant.root)
  if (strstr(fileNumber, "x")!=0) {
    cout << "     anaXX.root file number? [0= flow.cumulant.root] ";
    fgets(fileNumber, sizeof(fileNumber), stdin);
    fileNumber[strlen(fileNumber)-1] = '\0';
    if (strlen(fileNumber) == 1 && strstr(fileNumber,"0")) {
      sprintf(fileName, "flow.cumulant.root");
    } else {
      sprintf(fileName, "ana%s.root", fileNumber);       // insert
    }
    cout << " file name = " << fileName << endl;
    histFile = new TFile(fileName);
  }
  
  // input the page number
  while (pageNumber <= 0 || pageNumber > nNames) {
    if (pageNumber < 0) {                                 // plot all
      plotAll(nNames, orderN, selN, harN, -pageNumber);
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
  float Ycm     =   0.0;

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
  TLine* lineZeroY  = new TLine(-4.5, 0., 4.5, 0.);
  TLine* lineYcm    = new TLine(Ycm, -10., Ycm, 10.);
  char countSel[2];
  sprintf(countSel,"%d",selN);
  for (int j = firstJ; j < lastJ; j++) {
    char countRows[2];
    sprintf(countRows,"%d",j+1);
    for (int k = firstK ; k < lastK; k++) {
      char countColumns[2];
      sprintf(countColumns,"%d",k+1);
      char countOrder[2];
      sprintf(countOrder,"%d",2*(k+1));
      int padN = j*columns + k + 1;                          // pad number

      // construct histName
      TString* histName = new TString(baseName[pageNumber]);
      histName->Append(*countOrder);
      histName->Append("_Sel");
      histName->Append(*countSel);
      if (!singleGraph) {
	histName->Append("_Har");
	histName->Append(*countRows);
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
	float ptMax = hist->GetXaxis()->GetXmax();
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

void plotAll(Int_t nNames, Int_t orderN, Int_t selN, Int_t harN, Int_t first = 1) {
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = plotCumulant(i, orderN, selN, harN);
    c->Update();
    cout << "save? y/[n], quit? q" << endl;
    fgets(tmp, sizeof(tmp), stdin);
    if (strstr(tmp,"y")!=0) c->Print(".ps");
    else if (strstr(tmp,"q")!=0) return;
    c->Delete();
  }
  cout << "  plotAll Done" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: plotCumulant.C,v $
// Revision 1.1  2001/11/09 21:15:14  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
//
///////////////////////////////////////////////////////////////////////////////
