// macro for plots of histograms made by StFlowAnalysisMaker
TFile histFile("FlowHists.root");

const int nHarmonics = 4;
const int nSubEvents = 4;
const float twopi = 2. * 3.1416;

TCanvas* plot(int pageNumber=0){

  // names of histograms made by StFlowAnalysisMaker
  const char *baseName[] = { "Flow_Res_Event",
			     "Flow_Phi_Event",
			     "Flow_Phi_Flat_Event",
			     "Flow_Psi_Subs",
			     "Flow_Psi_Event",
			     "Flow_Mult_Event",
			     "Flow_MeanPt_Event",
			     "Flow_q_Event",
			     "Flow_Psi_Sub_Corr_Event",
			     "Flow_Psi_Sub_Corr_Diff_Event",
			     "Flow_phi_Corr_Event",
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
      plotAll(nNames);
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
  float qMax   =  2.;
  float EtaMax =  2.;
  float PtMax  =  2.;
  int n_qBins  = 50;

  int columns, rows;
  char* cp = strstr(shortName[pageNumber],"Subs");
  columns = (cp) ? nSubEvents : nSubEvents/2;
  rows = (strcmp(shortName[pageNumber],"Flow_Psi_Sub_Corr_Diff")!=0) ?
    nHarmonics : nHarmonics -1;
  int pads = rows*columns;
  cout << "pads= " << pads << endl;

  // make the plots
  TCanvas* c = new TCanvas(shortName[pageNumber],shortName[pageNumber],600,780);
  TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,shortName[pageNumber]);
  title->Draw();
  TPad* GraphPad = new TPad("Graphs","Graphs",0.02,0.02,0.98,0.95);
  GraphPad->Draw();
  GraphPad->cd();
  GraphPad->Divide(columns,rows);
  TLine* lineZeroEta = new TLine(-EtaMax,0.,EtaMax,0.);
  TLine* lineZeroPt = new TLine(0.,0.,PtMax,0.);
  for (int j = 0; j < rows; j++) {
    char CountRows[2];
    sprintf(CountRows,"%d",j);
    for (int k = 0 ; k < columns; k++) {
      char CountColumns[2];
      sprintf(CountColumns,"%d",k);
      int padN = j*columns + k + 1; // pad number
      char* temp = new char[30];    // construct histName
      strcpy(temp,shortName[pageNumber]);
      char* cproj = strstr(temp,".");
      if (cproj) {     // a projection
	*cproj = '\0'; // remove from "." on
	strcat(temp,"2D_Event");
	TString* histName = new TString(temp);
	TString* histProjName = new TString(baseName[pageNumber]);
	histProjName->Append(*CountColumns + 1);
	histProjName->Append("_Har");
	histProjName->Append(*CountRows + 1);
      } else {
	TString* histName = new TString(baseName[pageNumber]);
      }
      histName->Append(*CountColumns + 1);
      histName->Append("_Har");
      histName->Append(*CountRows + 1);
      cout << "row= " << CountRows << " col= " << CountColumns <<
	" pad= " << padN << "\t" << histName->Data() << endl;
      //TH1* hist= (TH1*)chain->Maker("FlowAnalysis")->GetHistList()->
      //FindObject(histName->Data()); // find hist
      TH1* hist = (TH1*)histFile.Get(histName->Data());
      if (!hist) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      GraphPad->cd(padN);
      if (strstr(shortName[pageNumber],"2D")!=0) { // 2D
     	hist->Draw("COLZ");
      } else if (strstr(shortName[pageNumber],".Eta")!=0) { // 2D X projection
	TH1D* projX = hist->ProjectionX(histName->Data(), 0, 9999, "E");
	projX->SetName(histProjName->Data());
	projX->SetXTitle("pseudorapidity");
	if (strcmp(shortName[pageNumber],"Flow_Yield.Eta")==0) projX->
	      SetYTitle("Counts");
	if (strcmp(shortName[pageNumber],"Flow_v.Eta")==0) projX->
              SetYTitle("Flow (%)");
	if (projX) projX->Draw();
  	lineZeroEta->Draw();
      } else if (strstr(shortName[pageNumber],".Pt")!=0) { // 2D Y projection
	TH1D* projY = hist->ProjectionY(histName->Data(), 0, 9999, "E"); 
	projY->SetName(histProjName->Data());
	projY->SetXTitle("Pt");
	if (strcmp(shortName[pageNumber],"Flow_Yield.Pt")==0) {
	  projY->SetYTitle("Counts");
	  gPad->SetLogy();
	}
	if (strcmp(shortName[pageNumber],"Flow_v.Pt")==0) projY->
              SetYTitle("Flow (%)");
	if (projY) projY->Draw();
  	lineZeroPt->Draw();
      } else if (strstr(shortName[pageNumber],"Corr")!=0) { // azimuthal dists.
	float norm = (float)(hist->GetNbinsX()) / hist->Integral(); 
	cout << "  Normalized by: " << norm << endl;
	hist->Scale(norm); // normalize height to one
 	TF1* funcCos = new TF1("funcCos",
 	   "1+[0]*2/100*cos([2]*x)+[1]*2/100*cos(([2]+1)*x)",0.,twopi);
	funcCos->SetParNames("k=1", "k=2", "harN");
	if (strstr(shortName[pageNumber],"Diff")!=0) { 
	  funcCos->SetParameters(0, 0, j+2); // initial values
	} else {
	  funcCos->SetParameters(0, 0, j+1); // initial values
	}
	funcCos->SetParLimits(2, 1, 1); // harN is fixed
	hist->Fit("funcCos");
	hist->SetMinimum(0.8);
	hist->Draw();
	delete funcCos;
      } else if (strstr(shortName[pageNumber],"_q")!=0) { // q distibution
	double area = hist->Integral() * qMax / (float)n_qBins; 
	cout << "  Area = " << area << endl;
	TF1* func_q = new TF1("func_q", "[0]*x*exp(-x*x)", 0., qMax);
	//func_q->SetParName(0, "area");
	func_q->SetParameter(0, area);
	hist->Draw();
	func_q->SetLineStyle(2);
	func_q->Draw("same");
      } else {
 	// hist->SetMaximum(10.);
  	// hist->SetMinimum(-10.);
	hist->Draw(); 
      }
      delete [] temp;
      delete histName;
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
  //const int nSubEvents = 4;
  int columns = nSubEvents/2;
  int rows = 2;
  int pads = rows*columns;
  cout << "pads= " << pads << endl;

  TCanvas* c = new TCanvas(profName[1],profName[1],600,780);
  c->Divide(columns,rows);
  for (int j = 0; j < rows; j++) {
    int profNumber = j;
    cout << "profile name= " << profName[profNumber] << endl;
    for (int k = 0; k < columns; k++) {
      char CountColumns[2];
      sprintf(CountColumns,"%d",k);
      int padN = j*columns + k +1;
      TString* histName = new TString(profName[profNumber]);
      histName->Append(*CountColumns + 1);
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
      if (hist) hist->Draw();
      delete histName;
    }
  }
  return c;
}

void plotAll(int nNames = 21) {
  // boldStyle();
  for (int i =  1; i < nNames + 1; i++) {
    TCanvas* c = plot(i);
    c->Print(".ps");
    c->Delete();
  }
  cout << "  plotAll Done" << endl;
}
