///////////////////////////////////////////////////////////////////////////////
//
// $Id: plot.C,v 1.2 1999/10/05 16:54:14 posk Exp $
//
// Author: Art Poskanzer, LBNL, Aug 1999
// Description:  Macro to plot histograms made by StFlowAnalysisMaker
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: plot.C,v $
// Revision 1.2  1999/10/05 16:54:14  posk
// Added getPhiWeight method for making the event plane isotropic.
//
//
//
///////////////////////////////////////////////////////////////////////////////

TFile histFile("flow.hist.root");

const int nHarmonics = 4;
const int nSubEvents = 4;
const float twopi = 2. * 3.1416;

TCanvas* plot(int pageNumber=0, int eventN=0, int harN=0){

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();

  // names of histograms made by StFlowAnalysisMaker
  // also projections of some of these histograms
  const char *baseName[] = { "Flow_Res_Event",
			     "Flow_Phi_Event",
			     "Flow_Phi_Weight_Event",
			     "Flow_Phi_Flat_Event",
			     "Flow_Psi_Subs",
			     "Flow_Psi_Event",
			     "Flow_Mult_Event",
			     "Flow_MeanPt_Event",
			     "Flow_q_Event",
			     "Flow_Psi_Sub_Corr_Event",
			     "Flow_Psi_Sub_Corr_Diff_Event",
			     "Flow_Phi_Corr_Event",
			     "Flow_Sum_v2D_Event",
			     "Flow_Yield2D_Event",
			     "Flow_Yield.Eta_Event",
			     "Flow_Yield.Pt_Event",
			     "Flow_vObs2D_Event",
			     "Flow_v2D_Event",
			     "Flow_v.Eta_Event",
			     "Flow_v.Pt_Event"};
  // 			     "Flow_Bin_Eta_Event",
  // 			     "Flow_Bin_Pt_Event"};
  const int nNames = sizeof(baseName) / sizeof(*baseName);

  // construct array of short names
  char* shortName[] = new char*[nNames];
  for (int n = 0; n < nNames; n++) {
    shortName[n] = new char[30];
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"_Event");
    if (cp) *cp = '\0';
  }

  // input the page number
  while (pageNumber <= 1 || pageNumber > nNames) {
    if (pageNumber == -1) {     // plot all
      plotAll(nNames, eventN, harN);
      return;
    }
    if (pageNumber == 1) {     // plot profile
      TCanvas* c = plotProfile();
      return c;
    }
    cout << "-1: \t All" << endl;
    for (int i = 0; i < nNames; i++) {
      cout << i+1 << ":\t " << shortName[i] << endl;
    }
    cout << "     page number? ";
    cin >> pageNumber;
  }
  pageNumber--;
  cout << "  name= " << shortName[pageNumber] << endl;

  // set the constants
  float qMax   =     2.;
  float etaMax =     2.;
  float ptMax  =     2.;
  float phiMax = twopi; 
  int n_qBins  =    50;

  char* cp = strstr(shortName[pageNumber],"Subs");
  int columns = (cp) ? nSubEvents : nSubEvents/2;
  int rows = (strcmp(shortName[pageNumber],"Flow_Psi_Sub_Corr_Diff")!=0) ?
    nHarmonics : nHarmonics -1;
  int pads = rows*columns;

  // make the plots
  if (eventN == 0) {
    int canvasWidth = 600; int canvasHeight = 780;
  } else {
    int canvasWidth = 780; int canvasHeight = 600;
  }
  TCanvas* c = new TCanvas(shortName[pageNumber],shortName[pageNumber],
			   canvasWidth,canvasHeight);
  c->ToggleEventStatus();
  if (eventN==0) {
    TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,shortName[pageNumber]);
    title->Draw();
  }
  TPad* graphPad = new TPad("Graphs","Graphs",0.02,0.02,0.98,0.95);
  graphPad->Draw();
  graphPad->cd();
  if (eventN==0) {
    graphPad->Divide(columns,rows);
    int firstK = 0; int firstJ = 0;
    int lastK = columns; int lastJ = rows;
  } else {
    int firstK = eventN -1; int lastK = eventN;
    int firstJ = harN -1; int lastJ = harN;
  }
  TLine* lineZeroEta = new TLine(-etaMax, 0., etaMax, 0.);
  TLine* lineZeroPt = new TLine(0., 0., ptMax, 0.);
  TLine* lineOnePhi = new TLine(0., 1., phiMax, 1.);
  for (int j = firstJ; j < lastJ; j++) {
    char countRows[2];
    sprintf(countRows,"%d",j+1);
    for (int k = firstK ; k < lastK; k++) {
      char countColumns[2];
      sprintf(countColumns,"%d",k+1);
      int padN = j*columns + k + 1; // pad number
      char* temp = new char[30];    // construct histName
      strcpy(temp,shortName[pageNumber]);
      char* cproj = strstr(temp,".");
      if (cproj) {     // a projection
	*cproj = '\0'; // remove from "." on
	strcat(temp,"2D_Event");
	TString* histName = new TString(temp);
	TString* histProjName = new TString(baseName[pageNumber]);
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
      //TH1* hist= (TH1*)chain->Maker("FlowAnalysis")->GetHistList()->
      //FindObject(histName->Data()); // find hist
      TH1* hist = (TH1*)histFile.Get(histName->Data());
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      if (eventN == 0) graphPad->cd(padN);
      if (strstr(shortName[pageNumber],"2D")!=0) {             // 2D
 	if (strcmp(shortName[pageNumber],"Flow_v2D")==0 ||
	    strcmp(shortName[pageNumber],"Flow_vObs2D")==0) {
	  hist->SetMaximum(50.);
	  hist->SetMinimum(-50.);
	  gStyle->SetOptStat(0);
	} else {
	  gStyle->SetOptStat(10);
	}
     	hist->Draw("COLZ");
      } else if (strstr(shortName[pageNumber],".Eta")!=0) { // 2D X projection
	TH1D* projX = hist->ProjectionX(histName->Data(), 0, 9999, "E");
	projX->SetName(histProjName->Data());
	projX->SetXTitle("pseudorapidity");
	if (strcmp(shortName[pageNumber],"Flow_Yield.Eta")==0) {
	  projX->SetYTitle("Counts");
	  gStyle->SetOptStat(10);
	  if (projX) projX->Draw("H");
	} else if (strcmp(shortName[pageNumber],"Flow_v.Eta")==0) {
	  projX->SetYTitle("Flow (%)");
	  gStyle->SetOptStat(0);
	  if (projX) projX->Draw("E1");
	}
  	lineZeroEta->Draw();
      } else if (strstr(shortName[pageNumber],".Pt")!=0) { // 2D Y projection
	TH1D* projY = hist->ProjectionY(histName->Data(), 0, 9999, "E"); 
	projY->SetName(histProjName->Data());
	projY->SetXTitle("Pt");
	if (strcmp(shortName[pageNumber],"Flow_Yield.Pt")==0) {
	  projY->SetYTitle("Counts");
	  gPad->SetLogy();
	  gStyle->SetOptStat(100110);
	  if (projY) projY->Draw("H");
	} else if (strcmp(shortName[pageNumber],"Flow_v.Pt")==0) {
	  projY->SetYTitle("Flow (%)");
	  gStyle->SetOptStat(0);
	  if (projY) projY->Draw("E1");
	}
  	lineZeroPt->Draw();
      } else if (strstr(shortName[pageNumber],"Corr")!=0) { // azimuthal corr.
	float norm = (float)(hist->GetNbinsX()) / hist->Integral(); 
	cout << "  Normalized by: " << norm << endl;
	hist->Scale(norm); // normalize height to one
	if (strstr(shortName[pageNumber],"Diff")!=0) { 
	  TF1* funcCos1 = new TF1("funcCos1",
	    "1+[0]*2/100*cos([1]*x)",0.,twopi);
	  funcCos1->SetParNames("k=1", "har");
	  funcCos1->SetParameters(0, j+2); // initial values
	  funcCos1->SetParLimits(1, 1, 1); // har is fixed
	  hist->Fit("funcCos1");
	  delete funcCos1;
	} else {
	  TF1* funcCos2 = new TF1("funcCos2",
	    "1+[0]*2/100*cos([2]*x)+[1]*2/100*cos(([2]+1)*x)",0.,twopi);
	  funcCos2->SetParNames("k=1", "k=2", "har");
	  funcCos2->SetParameters(0, 0, j+1); // initial values
	  funcCos2->SetParLimits(2, 1, 1);    // har is fixed
	  hist->Fit("funcCos2");
	  delete funcCos2;
	}
	//hist->SetMinimum(0.8);
	if (strstr(shortName[pageNumber],"Phi")!=0) hist->SetMinimum(0.9);
	gStyle->SetOptStat(10);
	gStyle->SetOptFit(111);
	hist->Draw("E1");
      } else if (strstr(shortName[pageNumber],"_q")!=0) { // q distibution
	double area = hist->Integral() * qMax / (float)n_qBins; 
	cout << "  Area = " << area << endl;
	TF1* func_q = new TF1("func_q", "[0]*2.*x*exp(-x*x)", 0., qMax);
	func_q->SetParameter(0, area);
	gStyle->SetOptStat(100110);
	gStyle->SetOptFit(111);
	hist->Draw("E1");
	func_q->SetLineStyle(kDotted);
	func_q->Draw("same");
      } else if (strstr(shortName[pageNumber],"Phi")!=0) { // Phi distibutions
	hist->SetMinimum(0.8*(hist->GetMaximum()));
	if (strstr(shortName[pageNumber],"Weight")!=0) {
	  gStyle->SetOptStat(0);
	  hist->Draw(); 
	  lineOnePhi->Draw();
	} else {
	  gStyle->SetOptStat(10);
	  hist->Draw(); 
	}
      } else if (strstr(shortName[pageNumber],"Psi")!=0) { // Psi distibutions
	gStyle->SetOptStat(10);
	hist->Draw("E1"); 
      } else {
	gStyle->SetOptStat(100110);
	hist->Draw(); 
      }
      delete [] temp;
      delete histName;
      delete histProjName;
      gPad->Update();
    }
  }
  for (int m = 0; m < nNames; m++) {  
    delete [] shortName[m];
  }
  delete [] shortName;
  return c;
}

// macro for the profile plots
TCanvas* plotProfile(int pageN=1){
  pageN--;
  char* profName[] = {"Flow_prof_Cos_Event","Flow_Res_Event"};
  int columns = nSubEvents/2;
  int rows = 2;
  int pads = rows*columns;

  TCanvas* c = new TCanvas(profName[1],profName[1],600,780);
  c->Divide(columns,rows);
  for (int j = 0; j < rows; j++) {
    int profNumber = j;
    cout << "profile name= " << profName[profNumber] << endl;
    for (int k = 0; k < columns; k++) {
      char countColumns[2];
      sprintf(countColumns,"%d",k+1);
      int padN = j*columns + k +1;
      TString* histName = new TString(profName[profNumber]);
      histName->Append(*countColumns);
      cout << "row= " << j << " col= " << k << " pad= " << padN << "\t" 
	   << histName->Data() << endl;
      //TH1* hist= (TH1*)chain->Maker("FlowAnalysis")->GetHistList()->
      //	FindObject(histName->Data());
      TH1* hist = (TH1*)histFile.Get(histName->Data());
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      c->cd(padN);
      gStyle->SetOptStat(0);
      if (hist) hist->Draw();
      if (hist) hist->Print("all");
      delete histName;
    }
  }
  return c;
}

void plotAll(int nNames = 21, int eventN = 0, int harN = 0) {
  for (int i =  1; i < nNames + 1; i++) {
    TCanvas* c = plot(i, eventN, harN);
    c->Print(".ps");
    c->Delete();
  }
  cout << "  plotAll Done" << endl;
}
