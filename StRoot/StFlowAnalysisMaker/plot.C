///////////////////////////////////////////////////////////////////////////////
//
// $Id: plot.C,v 1.10 2000/01/27 00:04:31 posk Exp $
//
// Author: Art Poskanzer, LBNL, Aug 1999
// Description:  Macro to plot histograms made by StFlowAnalysisMaker
//               If selN = 0 plot all selections and harmonics
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: plot.C,v $
// Revision 1.10  2000/01/27 00:04:31  posk
// Corrected error in pt plots.
//
// Revision 1.9  2000/01/24 23:02:13  posk
// Merged updates
//
// Revision 1.8  2000/01/13 21:50:24  posk
// Updates and corrections.
//
// Revision 1.7  1999/12/21 18:14:14  posk
// More graphs.
//
// Revision 1.6  1999/12/21 01:19:29  posk
// Added more histograms.
//
// Revision 1.5  1999/12/04 00:15:41  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.4  1999/11/24 18:14:07  posk
// Now reads event quantities with StFlowEvent methods
//
// Revision 1.3  1999/11/05 00:02:04  posk
// Changed the flow vector, Q, to a TVector2.
//
// Revision 1.2  1999/10/05 16:54:14  posk
// Added getPhiWeight method for making the event plane isotropic.
//
//
//
///////////////////////////////////////////////////////////////////////////////

TFile histFile("flow.hist.root");

//const Int_t nHars    = 6;
const Int_t nHars    = 3;
const Int_t nSels    = 2;
const Int_t nSubs    = 2;
const Float_t twopi  = 2. * 3.1416;
const Float_t etaMax = 2.;
const Float_t ptMax  = 2.;
Int_t runNo = 0;
char  runNumber[6];

// gSystem->Load("boldStyle");
// boldStyle();

TCanvas* plot(Int_t pageNumber=0, Int_t selN=0, Int_t harN=0){

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
    
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
			     "Flow_VertexZ",
			     "Flow_VertexXY2D",
			     "Flow_EtaSym",
			     "Flow_EtaPtPhi3D",
			     "Flow_EtaPtPhi2D.PhiEta",
                             "Flow_EtaPtPhi2D.PhiPt",
  			     "Flow_YieldAll2D",
  			     "Flow_YieldAll.Eta",
  			     "Flow_YieldAll.Pt",
   			     "Flow_Bin_Eta",
   			     "Flow_Bin_Pt",
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
			     "Flow_Sum_v2D_Sel",
			     "Flow_vObs2D_Sel",
			     "Flow_vObsEta_Sel",
			     "Flow_vObsPt_Sel",
			     "Flow_v2D_Sel",
			     "Flow_vEta_Sel",
			     "Flow_vPt_Sel",
			     "Flow_v.Eta_Sel",
			     "Flow_v.Pt_Sel" };
  //const int nNames = sizeof(baseName) / sizeof(baseName[0]);
  const int nNames = sizeof(baseName) / 4;
  const int nSingles = 20 + 1;

  // construct array of short names
  char* shortName[] = new char*[nNames];
  for (int n = 0; n < nNames; n++) {
    shortName[n] = new char[30];
    strcpy(shortName[n], baseName[n]);
    char* cp = strstr(shortName[n],"_Sel");
    if (cp) *cp = '\0';
  }

  // input the run number
  if (runNo == 0) {
    cout << "     run number? ";
    cin >> runNo;
    sprintf(runNumber,"ana%2d",runNo);
    cout << " run number = " << runNumber << endl;
  }

  // input the page number
  while (pageNumber <= nSingles || pageNumber > nNames) {
    if (pageNumber < 0) {           // plot all
      plotAll(nNames, selN, harN, -pageNumber);
      return;
    }
    if (pageNumber == 1) {            // plot resolution
      TCanvas* c = plotResolution();
      return c;
    }
    if (pageNumber > 0 && pageNumber <= nSingles) { // plot singles
      TCanvas* c = plotSingles(shortName[pageNumber-1]);
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
  //float qMax    =     2.;
  float qMax    =     5.;
  float phiMax  = twopi; 
  int   n_qBins =    50;

  char* cp = strstr(shortName[pageNumber],"Subs");
  int columns = (cp) ? nSubs + nSels : nSels;
  int rows = (strcmp(shortName[pageNumber],"Flow_Psi_Sub_Corr_Diff")!=0) ?
    nHars : nHars -1;
  int pads = rows*columns;

  // make the plots
  if (selN == 0) {
    int canvasWidth = 600, canvasHeight = 780;
  } else {
    int canvasWidth = 780, canvasHeight = 600;
  }
  TCanvas* c = new TCanvas(shortName[pageNumber],shortName[pageNumber],
			   canvasWidth,canvasHeight);
  c->ToggleEventStatus();
  if (selN==0) {
    TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,shortName[pageNumber]);
    title->Draw();
  }
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runNumber);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();
  if (selN==0) {
    graphPad->Divide(columns,rows);
    int firstK = 0, firstJ = 0, lastK = columns, lastJ = rows;
  } else {
    int firstK = selN -1, firstJ = harN -1, lastK = selN, lastJ = harN;
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
	strcat(temp,"2D_Sel");
	TString* histName = new TString(temp);
	TString* histProjName = new TString(baseName[pageNumber]);
	histProjName->Append(*countColumns);
	histProjName->Append("_Har");
	histProjName->Append(*countRows);
      } else {
	TString* histName = new TString(baseName[pageNumber]);
	TString* histProjName = new TString(baseName[pageNumber]); // not needed
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
      if (selN == 0) graphPad->cd(padN);
      if (strstr(shortName[pageNumber],"2D")!=0) {             // 2D
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
	projY->SetXTitle("Pt (GeV)");
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
	if (strstr(shortName[pageNumber],"Sub")!=0) { 
	  TF1* funcCos1 = new TF1("funcCos1",
	    "1+[0]*2/100*cos([1]*x)",0.,twopi);
	  funcCos1->SetParNames("k=1", "har");
	  if (strstr(shortName[pageNumber],"Diff")!=0) {
	    funcCos1->SetParameters(0, j+2); // initial values
	  } else {
	    funcCos1->SetParameters(0, j+1); // initial values
	  }
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
	if (strstr(shortName[pageNumber],"Phi")!=0) hist->SetMinimum(0.8);
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
       	hist->SetMinimum(0.9*(hist->GetMinimum()));
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
      } else if (strstr(shortName[pageNumber],"Eta")!=0) { // Eta distibutions
	gStyle->SetOptStat(100110);
	hist->Draw();
  	lineZeroEta->Draw();
      } else if (strstr(shortName[pageNumber],"Pt")!=0) {  // Pt distibutions
	gStyle->SetOptStat(100110);
	hist->Draw();
  	lineZeroPt->Draw();
      } else {                                             // all others
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

// macro for the resolution plots
TCanvas* plotResolution(){
  char* profName[] = {"Flow_prof_Cos_Sel","Flow_Res_Sel"};
  int columns = nSels;
  int rows = 2;
  int pads = rows*columns;

  TCanvas* c = new TCanvas(profName[1],profName[1],600,780);
  c->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runNumber);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TLine* lineZeroHar = new TLine(0.5, 0., 6.5, 0.);
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(columns,rows);
  for (int j = 0; j < rows; j++) {
    int profNumber = j;
    cout << "resolution name= " << profName[profNumber] << endl;
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
      graphPad->cd(padN);
      gStyle->SetOptStat(0);
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
  cout << "  name= " << shortName << endl;
  TCanvas* c = new TCanvas(shortName,shortName,780,600);
  c->ToggleEventStatus();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runNumber);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.99);
  graphPad->Draw();
  graphPad->cd();
  char* temp = new char[30];    // construct histName
  strcpy(temp,shortName);
  char* cproj = strstr(temp,".");
  if (cproj) {                  // a projection
    *cproj = '\0';              // remove from "." on
    cproj = strstr(temp,"2");
    if (cproj) {                // a 2D projection 
      *cproj = '\0';            // remove from "2D" on
      strcat(temp,"3D");
    } else {
      strcat(temp,"2D");
    }
    TString* histName = new TString(temp);
    TString* histProjName = new TString(shortName);
  } else {
    TString* histName = new TString(shortName);
  }
  cout << histName->Data() << endl;
  TH1* hist = (TH1*)histFile.Get(histName->Data());
  if (!hist) {
    cout << "### Can't find histogram " << histName->Data() << endl;
    return;
  }

  if (strstr(shortName,".PhiEta")!=0) {        // 3D Phi Eta projection
    TH2D* projZX = hist->Project3D("zxe");
    projZX->SetName(histProjName->Data());
    projZX->SetYTitle("azimuthal angle (rad)");
    projZX->SetXTitle("pseudorapidity");
    gStyle->SetOptStat(10);
    if (projZX) projZX->Draw("COLZ");
  } else if (strstr(shortName,".PhiPt")!=0) {  // 3D Phi Pt projection
    TH2D* projZY = hist->Project3D("zye");
    projZY->SetName(histProjName->Data());
    projZY->SetYTitle("azimuthal angle (rad");
    projZY->SetXTitle("Pt (GeV)");
    gStyle->SetOptStat(10);
    if (projZY) projZY->Draw("COLZ");
  } else 
  if (strstr(shortName,"2D")!=0) {             // 2D
    gStyle->SetOptStat(10);
    hist->Draw("COLZ");
  } else if (strstr(shortName,".Eta")!=0) {    // 2D Eta projection
    TH1D* projX = hist->ProjectionX(histName->Data(), 0, 9999, "E");
    projX->SetName(histProjName->Data());
    projX->SetXTitle("pseudorapidity");
    projX->SetYTitle("Counts");
    gStyle->SetOptStat(10);
    if (projX) projX->Draw("H");
  } else if (strstr(shortName,".Pt")!=0) {     // 2D Pt projection
    TH1D* projY = hist->ProjectionY(histName->Data(), 0, 9999, "E"); 
    projY->SetName(histProjName->Data());
    projY->SetXTitle("Pt (GeV)");
    projY->SetYTitle("Counts");
    gPad->SetLogy();
    gStyle->SetOptStat(100110);
    if (projY) projY->Draw("H");
  } else if (strstr(shortName,"Bin")!=0) {
    if (strstr(shortName,"Pt")!=0) {
      TLine* lineDiagonal  = new TLine(0., 0., ptMax, ptMax);
    } else {
      TLine* lineDiagonal = new TLine(-etaMax, -etaMax, etaMax, etaMax);
    }
    gStyle->SetOptStat(0);
    hist->SetMarkerStyle(21);
    hist->SetMarkerColor(2);
    hist->Draw();
    lineDiagonal->Draw();
  } else {
    gStyle->SetOptStat(100110);
    hist->Draw();
  }

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

