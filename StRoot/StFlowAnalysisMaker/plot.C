///////////////////////////////////////////////////////////////////////////////
//
// $Id: plot.C,v 1.20 2000/05/26 21:25:23 posk Exp $
//
// Author:       Art Poskanzer, LBNL, Aug 1999
// Description:  Macro to plot histograms made by StFlowAnalysisMaker.
//               If selN = 0 plot all selections and harmonics.
//               First time type .x plot.C() to see the menu.
//               Run Number appended to "ana" is entered in the bottom, left box.
//               File Number is prepended to flow.hist.root.
//               Default hist file is just flow.hist.root .
//               After the first execution, just type plot(N) .
//               A negative N plots all pages starting with page N.
//
//               Place a symbolic link to this file in StRoot/macros/analysis .
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: plot.C,v $
// Revision 1.20  2000/05/26 21:25:23  posk
// Use TProfile2D class and profile projection methods.
// Correction needed for >2 subevents.
//
// Revision 1.19  2000/05/03 16:38:35  posk
// Compatable with ROOT 2.24/02.
//
// Revision 1.18  2000/04/13 22:34:16  posk
// Resolution correction is now made.
//
// Revision 1.17  2000/04/10 18:49:10  posk
// Asks for the histogram file number.
//
// Revision 1.16  2000/03/28 23:25:37  posk
// Allow multiple instances.
//
// Revision 1.15  2000/03/21 00:24:45  posk
// Added GetCVS and changed some plot names.
//
// Revision 1.12  2000/02/18 23:44:54  posk
// Added PID and centrality.
//
// Revision 1.11  2000/02/04 16:26:43  posk
// Added correct calculation of event plane resolution for large flow.
//
// Revision 1.10  2000/01/27 00:04:31  posk
// Corrected error in pt plots.
//
// Revision 1.7  1999/12/21 18:14:14  posk
// More graphs.
//
// Revision 1.6  1999/12/21 01:19:29  posk
// Added more histograms.
//
// Revision 1.2  1999/10/05 16:54:14  posk
// Added getPhiWeight method for making the event plane isotropic.
//
//
///////////////////////////////////////////////////////////////////////////////

//const Int_t nHars    = 6;
const Int_t nHars    = 3;
const Int_t nSels    = 2;
const Int_t nSubs    = 2;
const Float_t twopi  = 2. * 3.1416;
const Float_t etaMax = 2.;
const Float_t ptMax  = 2.;
Int_t runNumber      = 0;
char  runName[6];
char  fileNumber[4]  = "x";
char  fileName[30];
TFile* histFile;


TCanvas* plot(Int_t pageNumber=0, Int_t selN=0, Int_t harN=0){

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
  gROOT->SetStyle("Bold");                              // set style
  gROOT->ForceStyle();

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char* baseName[] = { "Flow_Res_Sel",
			     "Flow_Charge",
			     "Flow_Dca",
			     "Flow_Chi2",
			     "Flow_FitPts",
			     "Flow_MaxPts",
			     "Flow_FitOverMax",
			     "Flow_Mult",
			     "Flow_OrigMult",
			     "Flow_MultOverOrig",
			     "Flow_CorrMult",
			     "Flow_VertexZ",
			     "Flow_VertexXY2D",
			     "Flow_EtaSym",
			     //"Flow_EtaPtPhi3D",
			     "Flow_EtaPtPhi2D.PhiEta",
                             "Flow_EtaPtPhi2D.PhiPt",
  			     "Flow_YieldAll2D",
  			     "Flow_YieldAll.Eta",
  			     "Flow_YieldAll.Pt",
  			     "Flow_PidPiPlus",
  			     "Flow_PidPiMinus",
  			     "Flow_PidProton",
  			     "Flow_PidMult",
  			     "Flow_Cent",
   			     "Flow_Bin_Eta",
   			     "Flow_Bin_Pt",
                             "Flow_CosPhiLab",
			     "Flow_Phi_Sel",
			     "Flow_Phi_Weight_Sel",
			     "Flow_Phi_Flat_Sel",
			     "Flow_Psi_Subs",
			     "Flow_Psi_Sel",
			     "Flow_Mult_Sel",
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
  const int nSingles = 26 + 1;

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
    cin >> runNumber;
    sprintf(runName, "ana%2d", runNumber);               // add ana prefix
    cout << " run name = " << runName << endl;
  }

  // input the file number (default opens flow.hist.root)
  if (strstr(fileNumber, "x")!=0) {
    cout << "     file number? [none] " << flush;
    fgets(fileNumber, sizeof(fileNumber), stdin);
    fileNumber[strlen(fileNumber)-1] = '\0';             // remove CR
    sprintf(fileName, "%sflow.hist.root", fileNumber);   // prepend
    cout << " file name = " << fileName << endl;
    histFile = new TFile(fileName);
  }

  // input the page number
  while (pageNumber <= nSingles || pageNumber > nNames) {
    if (pageNumber < 0) {                                // plot all
      plotAll(nNames, selN, harN, -pageNumber);
      return;
    }
    if (pageNumber == 1) {                               // plot resolution
      TCanvas* c = plotResolution();
      return c;
    }
    if (pageNumber > 0 && pageNumber <= nSingles) {      // plot singles
      TCanvas* c = plotSingles(shortName[pageNumber-1]);
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
  cout << "  graph name= " << shortName[pageNumber] << endl;

  // set constants
  //float qMax    =     2.;
  float qMax    =     5.;
  float phiMax  = twopi; 
  int   n_qBins =    50;
  TString* histProjName = NULL;

  // set row and column numbers
  char* cp = strstr(shortName[pageNumber],"Subs");
  int columns = (cp) ? nSubs + nSels : nSels;
  int rows = (strcmp(shortName[pageNumber],"Flow_Psi_Sub_Corr_Diff")!=0) ?
    nHars : nHars -1;
  int pads = rows*columns;

  // make the graph page
  if (selN == 0) {
    int canvasWidth = 600, canvasHeight = 780;             // portrait
  } else {
    int canvasWidth = 780, canvasHeight = 600;             // landscape
  }
  TCanvas* c = new TCanvas(shortName[pageNumber],shortName[pageNumber],
			   canvasWidth,canvasHeight);
  c->ToggleEventStatus();
  if (selN==0) {
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

  // make the plots
  if (selN==0) {
    graphPad->Divide(columns,rows);
    int firstK = 0, firstJ = 0, lastK = columns, lastJ = rows;
  } else {
    int firstK = selN -1, firstJ = harN -1, lastK = selN, lastJ = harN;
  }
  TLine* lineZeroEta = new TLine(-etaMax, 0., etaMax, 0.);
  TLine* lineZeroPt  = new TLine(0., 0., ptMax, 0.);
  TLine* lineOnePhi  = new TLine(0., 1., phiMax, 1.);
  for (int j = firstJ; j < lastJ; j++) {
    char countRows[2];
    sprintf(countRows,"%d",j+1);
    for (int k = firstK ; k < lastK; k++) {
      char countColumns[2];
      sprintf(countColumns,"%d",k+1);
      int padN = j*columns + k + 1;                    // pad number
      char* temp = new char[30];                       // construct histName
      strcpy(temp,shortName[pageNumber]);
      char* cproj = strstr(temp,".");
      if (cproj) {                                     // a projection
	*cproj = '\0';                                 // remove from "." on
	strcat(temp,"2D_Sel");
	TString* histName = new TString(temp);
	histProjName = new TString(baseName[pageNumber]);
	histProjName->Append(*countColumns);
	histProjName->Append("_Har");
	histProjName->Append(*countRows);
      } else {
	TString* histName = new TString(baseName[pageNumber]);
      }
      histName->Append(*countColumns);
      histName->Append("_Har");
      histName->Append(*countRows);
      cout << " col= " << k+1 << " row= " << j+1 << " pad= " << padN << "\t" 
	   << histName->Data() << endl;
      if (histProjName) {
	TH2* hist = (TH2*)histFile.Get(histName->Data());
      } else {
	TH1* hist = (TH1*)histFile.Get(histName->Data());
      }
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      if (selN == 0) graphPad->cd(padN);
      if (strstr(shortName[pageNumber],"2D")!=0) {          // 2D
 	if (strcmp(shortName[pageNumber],"Flow_v2D")==0 ||
	    strcmp(shortName[pageNumber],"Flow_vObs2D")==0) {
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
	projX->SetXTitle("pseudorapidity");
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
  	lineZeroPt->Draw();
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
	gStyle->SetOptStat(100110);
	hist->Draw();
  	lineZeroEta->Draw();
      } else if (strstr(shortName[pageNumber],"Pt")!=0) {     // Pt distibutions
	gStyle->SetOptStat(100110);
	hist->Draw();
  	lineZeroPt->Draw();
      } else {                                                // all others
	gStyle->SetOptStat(100110);
	hist->Draw(); 
      }
      delete [] temp;
      delete histName;
      if (histProjName) delete histProjName;
      gPad->Update();
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
  char* resName[] = {"Flow_Cos_Sel","Flow_Res_Sel"};
  int columns = nSels;
  int rows = 2;
  int pads = rows*columns;

  // make the graph page
  TCanvas* c = new TCanvas(resName[1],resName[1],600,780);
  c->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TLine* lineZeroHar = new TLine(0.5, 0., 6.5, 0.);
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
      TH1* hist = (TH1*)histFile.Get(histName->Data());
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      graphPad->cd(padN);
      gStyle->SetOptStat(0);
      hist->SetMaximum(1.1);
      hist->Draw();
      if (j == 0) lineZeroHar->Draw();
      hist->Print("all");
      delete histName;
    }
  }

  return c;
}

// macro for the single plots
TCanvas* plotSingles(char* shortName){
  cout << "  graph name= " << shortName << endl;
  // make the graph page
  TCanvas* c = new TCanvas(shortName,shortName,780,600);
  c->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();

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
  cout << histName->Data() << endl;
  if (histProjName) {
    if (strstr(temp,"3")) {
      TH3* hist = (TH3*)histFile.Get(histName->Data());
    } else {
      TH2* hist = (TH2*)histFile.Get(histName->Data());
    }
  } else {
    TH1* hist = (TH1*)histFile.Get(histName->Data());
  }
  if (!hist) {
    cout << "### Can't find histogram " << histName->Data() << endl;
    return;
  }

  // make the plots
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
  } else if (strstr(shortName,"2D")!=0) {           // 2D
    gStyle->SetOptStat(10);
    hist->Draw("COLZ");
  } else if (strstr(shortName,".Eta")!=0) {         // 2D Eta projection
    TH1D* projX = hist->ProjectionX(histName->Data(), 0, 9999);
    projX->SetName(histProjName->Data());
    projX->SetXTitle("pseudorapidity");
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
    gStyle->SetOptStat(0);
    hist->Draw();
  } else if (strstr(shortName,"Pid")!=0) {          // PID
    gStyle->SetOptStat(110110);
    hist->Draw();
  } else {
    gStyle->SetOptStat(100110);
    hist->Draw();
  }
  delete [] temp;
  delete histName;
  if (histProjName) delete histProjName;
  
  return c;
}

void plotAll(Int_t nNames, Int_t selN, Int_t harN, Int_t first = 1) {
  char temp[3];
  for (int i =  first; i < nNames + 1; i++) {
    TCanvas* c = plot(i, selN, harN);
    c->Update();
    cout << "save? y/[n]" << endl;
    fgets(temp, sizeof(temp), stdin);
    if (strstr(temp,"y")!=0) c->Print(".ps");
    c->Delete();
  }
  cout << "  plotAll Done" << endl;
}

#endif

